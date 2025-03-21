


species <- c("Pseudacris triseriata", "Hemidactylium scutatum", "Gyrinophilus porphyriticus", "Desmognathus ochrophaeus", "Emydoidea blandingii", "Glyptemys insculpta", "Nerodia sipedon", "Lampropeltis triangulum", "Aquila chrysaetos", "Catharus bicknelli", "Setophaga cerulea", "Coturnicops noveboracensis", "Ixobrychus exilis", "Glaucomys volans")


species_vars <- list(`Pseudacris triseriata` = c("wetland"), 
                            `Hemidactylium scutatum` = c("bog"), 
                            `Gyrinophilus porphyriticus` = c("flat", "forest"), 
                            `Desmognathus ochrophaeus` = c("flat", "forest"), 
                            `Emydoidea blandingii` = c("water"), 
                            `Glyptemys insculpta` = c("water"), 
                            `Nerodia sipedon` = c("water"), 
                            `Lampropeltis triangulum` = c(""), 
                            `Aquila chrysaetos` = c("truggedness", "flat"), 
                            `Catharus bicknelli` = c("elevation"), 
                            `Setophaga cerulea` = c("forest"), 
                            `Coturnicops noveboracensis` = c("flat"), 
                            `Ixobrychus exilis` = c("flat", "wetland"), 
                            `Glaucomys volans` = c("forest")
)


species_target_groups <- list(`Pseudacris triseriata` = c("retiles"), 
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

 
