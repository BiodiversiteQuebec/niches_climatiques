
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
climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
#climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount", "mean_diurnal_air_temperature_range")
#climate_vars <- c("mean_annual_air_temperature", "mean_diurnal_air_temperature_range")
#other_vars <- names(predictors)[20:nlyr(predictors)]
other_vars <- c("geomfootslope", "geomflat", "ruggedness", "elevation", "sand", "clay", "forest", "urban", "water", "wetland", "cropland")


#vars <- c(names(p)[1:19])
#vars <- c(names(p)[1:19], other_vars)
#vars <- c(climate_vars, other_vars)
#vars <- c(climate_vars[c(2, 3)], other_vars)
#vars <- c(climate_vars[1:2], other_vars[1:2])
#vars <- c(climate_vars[1:2])#, other_vars)
#vars <- c(climate_vars[c(1, 3)])
#vars <- c(other_vars[c(1, 2, 9, 10)])
#vars <- climate_vars[c(1, 3, 4)]
#vars <- c(other_vars)#, other_vars)
#vars <- climate_vars[c(1)]
#vars <- c(climate_vars[1], other_vars[c(2, 10)])
#vars <- c(other_vars)
#vars <- c(climate_vars[c(1, 2, 3, 4)])
#vars <- c(climate_vars[c(1:4)])
#vars <- c(climate_vars[c(1:4)], other_vars)
#vars <- c(other_vars)

other_vars <- c("geomfootslope", "geomflat", "ruggedness", "elevation", "sand", "clay", "forest", "urban", "water", "wetland", "cropland")
large_vars <- c("geomfootslope", "geomflat", "ruggedness", "elevation", "sand", "clay", "forest", "urban", "water", "wetland", "cropland")
#dput(grep("temperature|precipitation|isothermality", names(psmall), invert = TRUE, value = TRUE))
small_vars <-  c("alluvion", "anthropogenique", "bulk_density", "clay", "cropland", "depot","distance_to_roads", 
"eau_peu_profonde", "elevation", "eolien", "geomflat", "geomfootslope", 
"glaciaire", "glaciolacustre", "glaciomarin", "human_modification","indifferencie", "lacustre", "marais", "marecage", "marin", "nitrogen","organic_carbon_density", "organique", "ph", "quaternaire", "roche","ruggedness", "sand", "silt", "soil_organic_carbon", "till", "tourbiere_boisee", "tourbiere_indifferenciee", "tourbiere_minerotrophe", "tourbiere_ombrotrophe", "twi", "urban", "water", "wetland")
#pp <- p$large[[other_vars]]
#name_vars <- c("% bas de pentes", "% plat", "relief accidenté", "élévation", "sable", "argile", "% forêt", "% urbain", "% eau", "% milieu humide", "% agricole")
#names(pp) <- name_vars


models <- list(
  "climat" = climate_vars[c(1)],
  "gam" = climate_vars[c(1)],
  "habitat" = c(large_vars),
  "small" = c(small_vars)[sample(1:length(small_vars), 10)],
  "climat + habitat" = c(climate_vars[c(1)], large_vars),
  "climat (habitat)" = c(1, 3), 
  "climat (small)" = c(1, 4),
  "gam (habitat)" = c(2, 3),
  "gam (small)" = c(2, 4)
)




if(FALSE){

  library(vegan)
  r <- p$small
  climate <- grep("temperature|precipitation|isothermality", names(r), value = TRUE)
  rr <- aggregate(r[[climate]], 10, na.rm = TRUE)
  #screeplot(rda(values(rr), scale = TRUE))
  pca <- rda(values(rr), scale = TRUE)
  barplot(cumsum(eigenvals(pca)/sum(eigenvals(pca))))
  
  chosen <- climate
  chosen <- c("mean_annual_air_temperature", "annual_precipitation_amount", "annual_range_of_air_temperature")
  v <- values(rr) |> as.data.frame()
  cor(v[, chosen], use = "complete.obs")

  car::vif(lm(y ~ ., data = cbind(y = 1, v[, chosen])))

}