


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

 
