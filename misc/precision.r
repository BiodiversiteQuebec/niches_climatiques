

source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")
#atlas <- atlas_remote(parquet_date = tail(atlas_dates, 1))
#atlas <- atlas_local(parquet_date = tail(atlas_dates, 1), getwd())
#atlas <- duckdbfs::open_dataset("atlas_2024-11-07.parquet", tblname = "atlas")
atlas <- duckdbfs::open_dataset("atlas_2024-11-07.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("gbif.parquet")


x <- atlas |> 
  filter(valid_scientific_name %in% species) |>
  collect() |>
  as.data.table()
  
x <- st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326) 

th <- 20000
plot(st_geometry(st_crop(na, obs)))
plot(st_geometry(na), col = "grey90", border = "white", lwd = 2, add = TRUE)
text(st_coordinates(st_centroid(na)), labels = na$NAME_1, col = "white", lwd = 0.5)
plot(st_geometry(lakes), col = "white", border = "grey80", add = TRUE)
plot(st_geometry(obs[is.na(obs$coordinate_uncertainty), ]), pch = 16, col = "red", add = TRUE)
plot(st_geometry(obs[which(obs$coordinate_uncertainty >= th), ]), pch = 16, col = "blue", add = TRUE)
plot(st_geometry(obs[which(obs$coordinate_uncertainty < th), ]), pch = 16, col = "forestgreen", add = TRUE)
legend("bottomright", pch = 16, col = c("red", "blue", "forestgreen"), legend = c("NA", paste(">=", th), paste("<", th)), cex = 2, bty = "n")




x <- atlas |> 
  filter(valid_scientific_name == "Emydoidea blandingii") |> 
  #filter(valid_scientific_name == species) |> 
  #mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  #to_sf(crs = 4326) |> 
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  collect() |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)










g <- atlas |> 
  filter(group_en == "Reptiles") |>
  group_by(valid_scientific_name) |>
  summarize(n = n()) |>
  collect() |>
  as.data.frame() |>
  _$valid_scientific_name  



obs_gbif <- gbif |>
  filter(species == !!sp)  

atlas |>
  filter(group_en == "Reptiles") |>
  collect() |>
  filter(valid_scientific_name == "Emydoidea blandingii") |>    
  nrow()

atlas |>
  #filter(group_en == "Reptiles") |>
  filter(valid_scientific_name == "Emydoidea blandingii") |> 
  collect() |> 
  nrow() 

gbif |>
  filter(species == "Emydoidea blandingii") |> 
  collect() |>
  nrow()

g <- gbif |>
  #filter(species == "Graptemys geographica") |> 
  filter(class == "Testudines") |>
  #head() |>
  collect()
  
  filter(species == "Emydoidea blandingii") |> 
  collect() |> 
  nrow()   



x <- atlas |>
  filter(family == "Hylidae") |>
  as.data.frame() |>
  collect()



obs <- atlas |> 
  #mutate(genus = case_when(
  #  genus == "Emys" ~ "Emydoidea"
  #)) |>
  filter(genus == !!genus) |>
  collect()   


obs <- atlas |> 
  mutate(genus = case_when(
    genus == "Emys" ~ "Emydoidea"
  )) |>
  filter(genus == !!genus) |> 
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  collect()   