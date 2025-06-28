


source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")
#atlas <- atlas_remote(parquet_date = tail(atlas_dates, 1))
#atlas <- atlas_local(parquet_date = tail(atlas_dates, 1), getwd())
#atlas <- duckdbfs::open_dataset("atlas_2024-11-07.parquet", tblname = "atlas")
atlas <- duckdbfs::open_dataset("data/atlas_2024-11-07.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif_2025-03-01.parquet")
ebird <- duckdbfs::open_dataset("data/ebd_relJan-2025_niches.parquet")
ebird_checklists <- duckdbfs::open_dataset("data/ebd_sampling_relJan-2025.parquet")


genus <- strsplit(sp, " ")[[1]][1] # temp fix to also get subspecies and string manipulations do not seem to work when dplyr remote

switch(species_target_groups[[sp]],
  reptiles = source("scripts/data_reptiles.r"),
  mammals = source("scripts/data_mammals.r"),
  birds = source("scripts/data_birds.r")
)


### Observations #####################################################

obs_atlas <- obs_atlas |>
  filter(observation_value != "0") |>
  mutate(date = as.character(as.Date(paste(year_obs, month_obs, day_obs, sep="-"), format = "%Y-%m-%d"))) |>
  mutate(source = "atlas") |>
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)

counts <- obs_atlas |> 
  st_drop_geometry() |>
  count(dataset_name)


obs_gbif <- obs_gbif |>
  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  filter(coordinate_uncertainty <= 50000 | is.na(coordinate_uncertainty)) |>
  mutate(recordedby = sapply(recordedby, paste, collapse = "; ")) |>
  mutate(source = "gbif") |>
  mutate(dataset_name = institutioncode) |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)# |>

#x$recordedby <- sapply(x$recordedby, paste, collapse = "; ")
  
obs_gbif <- obs_gbif[!lengths(st_intersects(obs_gbif, qc)), ]


### Background #######################################################

background_atlas <- background_atlas |> 
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg) |>
  mutate(species = valid_scientific_name) |>
  mutate(source = "atlas") |>
  mutate(date = paste(year_obs, formatC(month_obs, width = 2, flag = 0), formatC(day_obs, width = 2, flag = 0), sep = "-"))

background_gbif <- background_gbif |> 
  filter(!is.na(decimallongitude) &  !is.na(decimallatitude)) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  mutate(source = "gbif") |>
  mutate(dataset_name = institutioncode) |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)

background_gbif <- background_gbif[!lengths(st_intersects(background_gbif, qc)), ]



if(species_target_groups[[sp]] == "birds"){
  background <- background_ebird
  obs <- obs_ebird
} else {
  n <- intersect(names(background_atlas), names(background_gbif))
  background <- rbind(background_atlas[, n], background_gbif[, n])
  n <- intersect(names(obs_atlas), names(obs_gbif))
  obs <- rbind(obs_atlas[, n], obs_gbif[, n])
}

obs <- obs[region, ]
background <- background[region, ]

th <- 30000 # minimal precision

png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_locations.png")), width = 8, height = 8, units = "in", res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_crop(na, obs)))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
plot(st_geometry(obs[is.na(obs$coordinate_uncertainty), ]), pch = 16, col = "red", add = TRUE)
plot(st_geometry(obs[which(obs$coordinate_uncertainty >= th), ]), pch = 16, col = "blue", add = TRUE)
plot(st_geometry(obs[which(obs$coordinate_uncertainty < th), ]), pch = 16, col = "forestgreen", add = TRUE)
legend("bottomright", inset = c(0.025, 0.025), pch = 16, col = c("forestgreen", "blue", "red"), legend = c(paste("<", th), paste(">=", th), "NA"), cex = 1.5, bty = "n", title = "Précision (en m)")
#mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()

obs_qc <- obs[qc, ]

png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_data.png")), width = 8, height = 8, units = "in", res = 300)
par(mar = c(0.5, 0.5, 0.5, 0.5))
plot(st_geometry(st_crop(na, obs_qc)))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 1, add = TRUE)
text(st_coordinates(st_centroid(st_buffer(na, -50000))), labels = na$NAME_1, col = "white", lwd = 0.25, cex = 0.75)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE, lwd = 0.5)
plot(st_geometry(obs_qc), col = factor(obs_qc$dataset_name), add = TRUE, pch = 16)
legend("bottomright", inset = c(0.025, 0.025), pch = 16, col = factor(levels(factor(obs_qc$dataset_name))), legend = paste0(levels(factor(obs_qc$dataset_name)), " (n = ", counts$n[match(levels(factor(obs_qc$dataset_name)), counts$dataset_name)], ")"), cex = 1, bty = "n", title = "Dataset")
#mtext(side = 3, line = -2.5, text = sp, font = 2, cex = 2, adj = 0.02)
dev.off()




obs <- obs[which(obs$coordinate_uncertainty <= th), ]
background <- background[which(background$coordinate_uncertainty <= th), ]

bg <- background[sample(1:nrow(background), 100000), ]

n <- intersect(names(obs), names(bg))
bg <- rbind(obs[, n], bg[, n])

buff <- st_buffer(obs, 250000) |> st_union()
nbuff <- 100000
x <- st_difference(region, buff) |> st_sample(size = nbuff)# |> st_as_sf()
x <- st_as_sf(cbind(st_drop_geometry(bg[rep(1, nbuff), ]), geometry = x), crs = epsg)
bg <- rbind(bg, x)

th_small <- th
obs_small <- obs[qc, ]
obs_small <- obs_small[which(obs_small$coordinate_uncertainty <= th), ]
bg_small <- bg[qc, ]
bg_small <- bg_small[which(bg_small$coordinate_uncertainty <= th), ]

obs <- list(large = obs, small = obs_small)
bg <- list(large = bg, small = bg_small)