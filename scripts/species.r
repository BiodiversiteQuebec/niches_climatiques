

source("scripts/prelim.r")

"https://data.canadensys.net/vascan/checklist?lang=en&habit=all&taxon=0&combination=anyof&province=BC&province=AB&province=SK&province=MB&province=ON&province=QC&province=NB&province=PE&province=NS&province=NL_N&province=NL_L&province=YT&province=NT&province=NU&status=native&status=introduced&status=ephemeral&status=doubtful&status=extirpated&status=excluded&rank=class&rank=order&rank=family&rank=genus&rank=species&nolimit=false&sort=taxonomically&criteria_panel=selection"

vascan <- read.csv("http://data.canadensys.net/downloads/vascan/TXT-b23b0de1-4c83-4136-88c3-7e1ce0918d5c.txt", sep = "\t")
vascan <- vascan[vascan$Rank == "Species", ]
trees <- vascan$Scientific.name[grepl("Tree", vascan$Habit)]
plants <- vascan$Scientific.name[!grepl("Tree", vascan$Habit)]


atlas <- duckdbfs::open_dataset("data/atlas_2025-03-17.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("data/gbif_2025-03-01.parquet")
ebird <- duckdbfs::open_dataset("data/ebd_relJan-2025_niches.parquet")

species_info <- atlas |>
  #head() |> collect()
  count(group_en, kingdom, phylum, class, order, genus, valid_scientific_name, vernacular_fr) |>
  arrange(-n) |> 
  collect() |>
  rename(species = valid_scientific_name) |>
  mutate(group = tolower(group_en)) |>
  mutate(group = recode(group, "amphibians" = "reptiles")) |>
  mutate(group = ifelse(species %in% trees, "trees", group)) |>
  mutate(group = ifelse(species %in% plants, "plants", group)) |>
  #count(group) |>
  filter(group %in% c("birds", "reptiles", "mammals", "trees", "plants")) |>
  as.data.frame()



species_target_groups <- species_info$group
names(species_target_groups) <- species_info$species




species <- c("Pseudacris triseriata", "Hemidactylium scutatum", "Gyrinophilus porphyriticus", "Desmognathus ochrophaeus", "Emydoidea blandingii", "Glyptemys insculpta", "Nerodia sipedon", "Lampropeltis triangulum", "Aquila chrysaetos", "Catharus bicknelli", "Setophaga cerulea", "Coturnicops noveboracensis", "Ixobrychus exilis", "Glaucomys volans")


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

 
