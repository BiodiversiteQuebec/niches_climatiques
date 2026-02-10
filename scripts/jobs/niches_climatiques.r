

species <- c("Pseudacris triseriata", "Hemidactylium scutatum", "Gyrinophilus porphyriticus", "Desmognathus ochrophaeus", "Emydoidea blandingii", "Glyptemys insculpta", "Nerodia sipedon", "Lampropeltis triangulum", "Aquila chrysaetos", "Catharus bicknelli", "Setophaga cerulea", "Coturnicops noveboracensis", "Ixobrychus exilis", "Glaucomys volans")


species_vars <- list(
`Pseudacris triseriata` = c("wetland", "marais", "marecage", "geomflat"), 
`Hemidactylium scutatum` = c("tourbiere", "marais", "organique"), 
`Gyrinophilus porphyriticus` = c("elevation", "ruggedness", "forest", "geomflat", "twi", "distance_to_streams", "southstlawrence"), 
`Desmognathus ochrophaeus` = c("elevation", "ruggedness", "forest", "geomflat", "twi", "distance_to_streams", "southstlawrence"), 
`Emydoidea blandingii` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde", "distance_to_streams", "distance_to_rivers"),
`Glyptemys insculpta` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde", "sand", "distance_to_streams", "distance_to_rivers"),
`Nerodia sipedon` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde", "distance_to_openwater"),
`Lampropeltis triangulum` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"), 
`Aquila chrysaetos` = c("elevation", "ruggedness", "roche"), 
`Catharus bicknelli` = c("elevation", "ruggedness"), 
`Setophaga cerulea` = c("forest", "ph", "silt", "nitrogen"), 
`Coturnicops noveboracensis` = c("geomflat", "marais", "wetland", "prairie_humide", "glaciomarin", "organique", "distance_to_coast"), 
`Ixobrychus exilis` = c("geomflat", "wetland", "marais", "eau_peu_profonde", "glaciomarin", "glaciolacustre", "alluvion"), 
`Glaucomys volans` = c("forest")
)

species_vars <- lapply(species_vars, function(i){c(i, "urban", "cropland")})


species_target_groups <- list(`Pseudacris triseriata` = c("reptiles"), 
                            `Hemidactylium scutatum` = c("reptiles"), 
                            `Gyrinophilus porphyriticus` = c("reptiles"), 
                            `Desmognathus ochrophaeus` = c("reptiles"), 
                            `Emydoidea blandingii` = c("reptiles"), 
                            `Glyptemys insculpta` = c("reptiles"), 
                            `Nerodia sipedon` = c("reptiles"), 
                            `Lampropeltis triangulum` = c("reptiles"), 
                            `Aquila chrysaetos` = c("birds"), 
                            `Catharus bicknelli` = c("birds"), 
                            `Setophaga cerulea` = c("birds"), 
                            `Coturnicops noveboracensis` = c("birds"), 
                            `Ixobrychus exilis` = c("birds"), 
                            `Glaucomys volans` = c("mammals")
)

breeding_periods <- list(
    `Aquila chrysaetos` = c("06-07", "08-23"), 
    `Catharus bicknelli` = c("06-07", "07-12"), 
    `Setophaga cerulea` = c("05-24", "07-12"), 
    `Coturnicops noveboracensis` = c("06-07", "08-24"), 
    `Ixobrychus exilis` = c("06-07", "07-19")
)

run_model <- 1:9 

info <- duckdbfs::open_dataset("data/atlas_2025-03-17.parquet", tblname = "atlas") |>
  filter(valid_scientific_name %in% species) |>
  rename(species = valid_scientific_name) |>
  rename(nom = vernacular_fr) |>
  distinct(species, nom) |> #, .keep_all = TRUE)
  #head() |>
  collect() |>
  as.data.frame()


info$period <- sapply(breeding_periods[match(info$species, names(breeding_periods))], paste, collapse = " / ")
info$period <- ifelse(info$period == "", NA, info$period)

write.csv(info, file.path("results", gsub(".r", "_info.csv", args)), row.names = FALSE)
