


source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")
#atlas <- atlas_remote(parquet_date = tail(atlas_dates, 1))
#atlas <- atlas_local(parquet_date = tail(atlas_dates, 1), getwd())
#atlas <- duckdbfs::open_dataset("atlas_2024-11-07.parquet", tblname = "atlas")
atlas <- duckdbfs::open_dataset("data/atlas_2024-11-07.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif.parquet")


#g1 <- atlas |> 
#  group_by(phylum) |>
#  summarize(n = n()) |>
#  collect()

#g1 <- atlas |> 
#  group_by(group_fr) |>
#  summarize(n = n()) |>
#  collect()

#g2 <- atlas |> 
#  group_by(group_fr, valid_scientific_name) |>
#  summarize(n = n()) |>
#  group_by(group_fr) |>
#  summarize(species = n()) |>
#  collect()

#g <- full_join(g1, g2) |>
#       mutate(moy = n / species)
#g

#png("bar.png", width = 16, height = 12, units = "in", res = 200)
#barplot(g$moy, names.arg = g$group_fr, cex.names = 0.45)
#dev.off()


#g <- atlas |> 
#  group_by(group_en) |>
#  summarize(n = n()) |>
#  collect() |>
#  as.data.frame() |>
#  _$group_en  


#reptiles <- atlas |> 
#  filter(group_en %in% c("Reptiles", "Amphibians")) |>
#  mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
#  to_sf(crs = 4326) |> 
#  collect() |>
#  as.data.frame()


reptiles <- atlas |> 
  filter(group_en %in% c("Reptiles", "Amphibians")) |>
  collect() |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg) |>
  mutate(species = valid_scientific_name) |>
  mutate(source = "atlas") |>
  mutate(date = paste(year_obs, formatC(month_obs, width = 2, flag = 0), formatC(day_obs, width = 2, flag = 0), sep = "-"))
  #mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  #to_sf(crs = 4326)# |> # |>
  #collect() |>

reptiles_gbif <- gbif |> 
  filter(class %in% c("Squamata", "Amphibia", "Testudines")) |>
  filter(!is.na(decimallongitude) &  !is.na(decimallatitude)) |>
  #count(class) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  collect() |>
  mutate(source = "gbif") |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)

reptiles_gbif <- reptiles_gbif[!lengths(st_intersects(reptiles_gbif, qc)), ]

  #st_transform(epsg)

#birds <- atlas |> # too heavy for memory
#  filter(group_en %in% c("Birds")) |>
#  collect() |>
#  as.data.frame()

mammals <- atlas |> 
  filter(group_en %in% c("Mammals")) |>
  collect() |>
  as.data.frame()



#params<-list()
#params$species <- "Bonasa umbellus"
#sp <- species[6]
genus <- strsplit(sp, " ")[[1]][1] # temp fix to also get subspecies and string manipulations do not seem to work when dplyr remote


obs <- atlas |> 
  mutate(genus = case_match(genus, "Emys" ~ "Emydoidea", .default = genus)) |>
  filter(genus == !!genus) |> 
  #filter(valid_scientific_name == species) |> 
  #mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  #to_sf(crs = 4326) |> 
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  collect() |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)

obs <- obs |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(observation_value != "0") |>
  #mutate(species = sub("^(([^ ]+ ){1}[^ ]+).*", "\\1", valid_scientific_name)) |>
  filter(species == !!sp)

obs <- obs |>
  mutate(date = as.character(as.Date(paste(year_obs, month_obs, day_obs, sep="-"), format = "%Y-%m-%d"))) |>
  mutate(source = "atlas")

obs <- st_transform(obs, epsg)

obs_gbif <- gbif |>
  mutate(species = case_match(species, "Emys blandingii" ~ "Emydoidea blandingii", .default = species)) |>
  filter(species == !!sp) |>
  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  filter(coordinate_uncertainty <= 50000 | is.na(coordinate_uncertainty)) |>
  #mutate(geom = ST_Point(as.numeric(decimallongitude), as.numeric(decimallatitude))) |> 
  #to_sf(crs = 4326) |> 
  collect() |>
  mutate(recordedby = sapply(recordedby, paste, collapse = "; ")) |>
  mutate(source = "gbif") |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)# |>

#x$recordedby <- sapply(x$recordedby, paste, collapse = "; ")
  
obs_gbif <- obs_gbif[!lengths(st_intersects(obs_gbif, qc)), ]
  
if(FALSE){
  par(mar = c(1, 1, 1, 1))
  plot(st_buffer(st_geometry(obs), 1600000), col = NA, border = NA)
  plot(st_geometry(na), col = "grey90", border = "grey80", add = TRUE)
  plot(st_geometry(qc), col = "grey90", border = NA, add = TRUE)
  plot(st_geometry(qc), border = "grey80", lwd = 1, add = TRUE)
  plot(st_geometry(lakes), col = "white", lwd = 0.1, add = TRUE)
  plot(st_geometry(obs), pch = 21, bg = adjustcolor("forestgreen", 0.25), col = adjustcolor("forestgreen", 0.75), lwd = 0.25, add = TRUE)
  plot(st_geometry(obs_gbif), pch = 21, bg = adjustcolor("brown", 0.25), col = adjustcolor("brown", 0.75), lwd = 0.25, add = TRUE)
}

n <- intersect(names(reptiles), names(reptiles_gbif))
background <- rbind(reptiles[, n], reptiles_gbif[, n])

n <- intersect(names(obs), names(obs_gbif))
obs <- rbind(obs[, n], obs_gbif[, n])

obs <- obs[region, ]
background <- background[region, ]

th <- 2000 # minimal precision

png(file.path("results/graphics", paste(sp, "_locations.png")), width = 8, height = 8, units = "in", res = 300)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(st_crop(na, obs)))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
plot(st_geometry(obs[is.na(obs$coordinate_uncertainty), ]), pch = 16, col = "red", add = TRUE)
plot(st_geometry(obs[which(obs$coordinate_uncertainty >= th), ]), pch = 16, col = "blue", add = TRUE)
plot(st_geometry(obs[which(obs$coordinate_uncertainty < th), ]), pch = 16, col = "forestgreen", add = TRUE)
legend("bottomright", inset = c(0.025, 0.025), pch = 16, col = c("forestgreen", "blue", "red"), legend = c(paste("<", th), paste(">=", th), "NA"), cex = 1.5, bty = "n", title = "Précision (en m)")
mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()


obs <- obs[which(obs$coordinate_uncertainty <= th), ]
background <- background[which(background$coordinate_uncertainty <= th), ]

bg <- background[sample(1:nrow(background), 100000), ]
bg <- rbind(obs, bg)

buff <- st_buffer(obs, 250000) |> st_union()
nbuff <- 100000
x <- st_difference(region, buff) |> st_sample(size = nbuff)# |> st_as_sf()
x <- st_as_sf(cbind(st_drop_geometry(bg[rep(1, nbuff), ]), geometry = x), crs = epsg)
bg <- rbind(bg, x)