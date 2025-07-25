

#atlas |> 
#  filter(group_en %in% c("Birds")) |>
#  count(dataset_name) |>
#  arrange(-n)

#ebird_checklists |> 
#  #head() |>
#  count(state) |>
#  arrange(-n)

#gbif |>
#  filter(class %in% c("Aves")) |>
#  #head() |>
#  count(institutioncode) |>
#  arrange(-n)
    
keep_months <- as.integer(substr(breeding_periods[[sp]], 1, 2)) 
keep_months <- keep_months[1]:keep_months[2]

### Background #################################################

background_atlas <- atlas |> 
  filter(group_en %in% c("Birds")) |>
  filter(!dataset_name %in% c("Données de localisation des grands mammifères", "EOD – eBird Observation Dataset")) |>
  filter(!grepl("GPS locations of Northern Gannets", dataset_name, ignore.case = TRUE)) |>
  filter(month_obs %in% keep_months) |> # giving the date format does not work before the 
  collect() |>
  mutate(date = as.character(as.Date(paste(year_obs, month_obs, day_obs, sep="-"), format = "%Y-%m-%d"))) |>
  mutate(day = substr(as.character(date), 6, 10)) |>
  filter(day >= !!breeding_periods[[sp]][1] & day <= !!breeding_periods[[sp]][2])

background_gbif <- gbif |> # ebird was removed from this gbif
  filter(class %in% c("Aves")) |>
  mutate(day = substr(as.character(eventdate), 6, 10)) |>
  filter(day >= !!breeding_periods[[sp]][1] & day <= !!breeding_periods[[sp]][2]) |>
  #head() |>
  collect()

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
  filter(!dataset_name %in% c("Données de localisation des grands mammifères", "EOD – eBird Observation Dataset")) |>
  filter(!grepl("GPS locations of Northern Gannets", dataset_name, ignore.case = TRUE)) |>
  filter(genus == !!genus) |> 
  filter(month_obs %in% keep_months) |> # giving the date format does not work before the collect
  collect() |>
  mutate(date = as.character(as.Date(paste(year_obs, month_obs, day_obs, sep="-"), format = "%Y-%m-%d"))) |>
  mutate(day = substr(as.character(date), 6, 10)) |>
  filter(day >= !!breeding_periods[[sp]][1] & day <= !!breeding_periods[[sp]][2]) |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(species == !!sp)

obs_gbif <- gbif |>
  filter(species == !!sp) |>
  mutate(day = substr(as.character(eventdate), 6, 10)) |>
  filter(day >= !!breeding_periods[[sp]][1] & day <= !!breeding_periods[[sp]][2]) |>
  #head() |>
  collect()

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






