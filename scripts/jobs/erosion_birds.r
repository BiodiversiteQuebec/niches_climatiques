
library(ebirdst)

atlas <- duckdbfs::open_dataset("data/atlas_2025-03-17.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif_2025-03-01.parquet")
#ebird <- duckdbfs::open_dataset("/home/frousseu/data2/ebd_relJan-2025.parquet")
ebird <- duckdbfs::open_dataset("data/ebd_relJan-2025.parquet")

species_info <- atlas |>
  #head() |> collect()
  filter(observed_rank %in% c("species", "subspecies", "variety", "form")) |>
  mutate(group = tolower(group_en)) |>
  filter(group %in% c("birds")) |>
  rename(species = valid_scientific_name) |>
  count(group, order, genus, species) |>
  arrange(-n) |> 
  collect() |>
  mutate(species = sub("^(([^ ]+ )[^ ]+).*", "\\1", species)) |>
  group_by(across(-n)) |>
  summarise(n = sum(n), .groups = "drop") |>
  arrange(-n) |> 
  as.data.frame()


eb <- ebirdst_runs |>
         mutate(start = ifelse(is.na(breeding_start), "01-01", substr(breeding_start, 6, 10))) |>
         mutate(end = ifelse(is.na(breeding_end), "12-31", substr(breeding_end, 6, 10))) |>
         rename(species = scientific_name) |>
         mutate(species = case_match(species, 
          "Botaurus exilis" ~ "Ixobrychus exilis", 
          "Acanthis flammea" ~ "Acanthis hornemanni",
          "Astur atricapillus" ~ "Accipiter atricapillus",
          "Astur cooperii" ~ "Accipiter cooperii",
          "Ardea ibis" ~ "Bubulcus ibis",
          "Botaurus exilis" ~ "Ixobrychus exilis",
          "Larus smithsonianus" ~ "Larus argentatus",
          "Nannopterum auritum" ~ "Phalacrocorax auritus",
          "Corthylio calendula" ~ "Regulus calendula",
          "Troglodytes aedon/musculus" ~ "Troglodytes aedon",
          .default = species)) |>
         select(species, start, end) |>
         unique() |>
         as.data.frame()

species_info <- merge(species_info, eb, all.x = TRUE)
species_info <- species_info[species_info$n >= 500, ]
species_info <- species_info[rev(order(species_info$n)), ]

# species_info[species_info$group == "birds", ]
#species_info[grepl("villosus", species_info$species), ]

#atlas |>
#  filter(valid_scientific_name %in% c("Dryobates villosus", "Leuconotopicus villosus")) |>
#  count(valid_scientific_name) |>
#  collect()

#atlas |>
#  filter(grepl("Dryobates|Leuconotopicus", valid_scientific_name)) |>
#  count(valid_scientific_name) |>
#  collect()


species_vars <- list(
`Pseudacris triseriata` = c("wetland", "marais", "marecage", "geomflat"), 
`Hemidactylium scutatum` = c("tourbiere", "marais", "organique"), 
`Gyrinophilus porphyriticus` = c("elevation", "ruggedness", "forest", "geomflat", "twi"), 
`Desmognathus ochrophaeus` = c("elevation", "ruggedness", "forest", "geomflat", "twi"), 
`Emydoidea blandingii` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"),
`Glyptemys insculpta` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde", "sand"),
`Nerodia sipedon` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"),
`Lampropeltis triangulum` = c("wetland", "marais", "marecage", "geomflat", "water", "eau_peu_profonde"), 
`Aquila chrysaetos` = c("elevation", "ruggedness"), 
`Catharus bicknelli` = c("elevation", "ruggedness"), 
`Setophaga cerulea` = c("forest", "ph", "silt", "nitrogen"), 
`Coturnicops noveboracensis` = c("flat", "marais", "wetland", "prairie_humide"), 
`Ixobrychus exilis` = c("geomflat", "wetland", "marais", "eau_peu_profonde"), 
`Glaucomys volans` = c("forest")
)

#species_info$vars <- NA
#ma <- match(names(species_vars), species_info$species)
ma <- match(species_info$species, names(species_vars))
species_info$vars <- species_vars[ma]

species_info$vars <- lapply(species_info$vars, function(i){
  add <- c("urban", "cropland")
  if(!is.null(i)){
    c(i, add)
  } else {
    add
  }
})

set.seed(1234)
species <- sample(species_info$species[species_info$group %in% "birds"], 300)
#species <- sample(species_info$species[species_info$group %in% "trees"], 2)
#species <- c("Catharus guttatus", "Ammospiza leconteii")
#species <- c("Poecile atricapillus", "Dryobates pubescens")
#species <- c("Turdus migratorius", "Poecile atricapillus")
species <- c("Junco hyemalis")
print(species)
species_info <- species_info[species_info$species %in% species, ]

breeding_periods <- lapply(1:nrow(species_info), function(i){
  c(species_info$start[i], species_info$end[i])
})
names(breeding_periods) <- species_info$species

species_target_groups <- as.list(species_info$group)
names(species_target_groups) <- species_info$species

species_vars<- species_info$vars
names(species_vars) <- species_info$species

run_model <- 1:2

