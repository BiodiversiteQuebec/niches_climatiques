
species_common_predictors <- c("")

#sp_extra_predictors <- vector(mode = "list", length = length(sp))
#names(sp_extra_predictors) <- sp
#dput(sp_extra_predictors)

species_predictors <- list(`Pseudacris triseriata` = c("wetland"), 
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


#climate_vars <- names(p)[1:19]
#climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
#climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount", "mean_diurnal_air_temperature_range")
#climate_vars <- c("mean_annual_air_temperature", "mean_diurnal_air_temperature_range")
#other_vars <- names(predictors)[20:nlyr(predictors)]
other_vars <- c("geomfootslope_per", "geomflat_perc", "vrm", "elevation", "sand_0-5cm", "clay_0-5cm", "forest", "urban", "water", "wetland", "crop")



#vars <- c(names(p)[1:19], other_vars)
#vars <- c(climate_vars, other_vars)
#vars <- c(climate_vars[c(2, 3)], other_vars)
#vars <- c(climate_vars[1:2], other_vars[1:2])
#vars <- c(climate_vars[1:2])#, other_vars)
#vars <- c(climate_vars[c(1, 3)])
#vars <- c(other_vars[c(1, 2, 9, 10)])
#vars <- climate_vars[c(1, 3, 4)]
#vars <- c(other_vars)#, other_vars)
vars <- climate_vars[c(1)]
#vars <- c(climate_vars, other_vars)
#vars <- c(other_vars)


other_vars <- c("geomfootslope_per", "geomflat_perc", "vrm", "elevation", "sand_0-5cm", "clay_0-5cm", "forest", "urban", "water", "wetland", "crop")
pp <- p[[other_vars]]
name_vars <- c("% bas de pentes", "% plat", "relief accidenté", "élévation", "sable", "argile", "% forêt", "% urbain", "% eau", "% milieu humide", "% agricole")
names(pp) <- name_vars


#png("results/graphics/non_climate_predictors.png", width = 10, height = 8, units = "in", res = 300)
#plot(crop(pp[[name_vars]], qc, mask = TRUE), axes = FALSE)
#dev.off()
