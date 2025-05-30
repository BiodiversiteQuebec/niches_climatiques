

### Background #################################################

background_atlas <- atlas |> 
  filter(group_en %in% c("Birds")) |>
  filter(!dataset_name %in% c("Données de localisation des grands mammifères")) |>
  filter(!grepl("GPS locations of Northern Gannets", dataset_name, ignore.case = TRUE)) |>
  collect() |>
  head()

background_gbif <- gbif |> 
  filter(class %in% c("Aves")) |>
  collect() |>
  head()

background_ebird <- ebird_checklists |> 
  mutate(day = substr(as.character(date), 6, 10)) |>
  filter(day >= !!breeding_periods[[sp]][1] & day <= !!breeding_periods[[sp]][2]) |>
  collect() |>  
  filter(!is.na(longitude) &  !is.na(latitude)) |>
  mutate(coordinate_uncertainty = 1000 * as.numeric(distance)) |>
  mutate(source = "ebird") |>
  mutate(dataset_name = "ebird") |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)

### Observations ###############################################  

obs_atlas <- atlas |> 
  filter(genus == !!genus) |> 
  collect() |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(species == !!sp) |>
  head()

obs_gbif <- gbif |>
  filter(species == !!sp) |>
  collect() |>
  head()

obs_ebird <- ebird |> 
  mutate(species = case_match(species, "Botaurus exilis" ~ "Ixobrychus exilis", .default = species)) |>
  filter(species == !!sp) |>
  mutate(day = substr(as.character(date), 6, 10)) |>
  filter(day >= !!breeding_periods[[sp]][1] & day <= !!breeding_periods[[sp]][2]) |>
  collect() |>  
  filter(!is.na(longitude) &  !is.na(latitude)) |>
  mutate(coordinate_uncertainty = 1000 * as.numeric(distance)) |>
  mutate(source = "ebird") |>
  mutate(dataset_name = "ebird") |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)