


reptiles <- atlas |> 
  filter(group_en %in% c("Reptiles", "Amphibians")) |>
  collect() |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg) |>
  mutate(species = valid_scientific_name) |>
  mutate(source = "atlas") |>
  mutate(date = paste(year_obs, formatC(month_obs, width = 2, flag = 0), formatC(day_obs, width = 2, flag = 0), sep = "-"))


reptiles_gbif <- gbif |> 
  filter(class %in% c("Squamata", "Amphibia", "Testudines")) |>
  filter(!is.na(decimallongitude) &  !is.na(decimallatitude)) |>
  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
  collect() |>
  mutate(source = "gbif") |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)

reptiles_gbif <- reptiles_gbif[!lengths(st_intersects(reptiles_gbif, qc)), ]





