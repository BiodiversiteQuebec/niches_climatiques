

#climate_vars <- names(p)[1:19]
#climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
#climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
climate_vars <- c("mean_annual_air_temperature")
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
"eau_peu_profonde", "elevation", "eolien", "forest", "geomflat", "geomfootslope", 
"glaciaire", "glaciolacustre", "glaciomarin", "human_modification","indifferencie", "lacustre", "marais", "marecage", "marin", "nitrogen","organic_carbon_density", "organique", "ph", "quaternaire", "roche","ruggedness", "sand", "silt", "soil_organic_carbon", "till", "tourbiere_boisee", "tourbiere_indifferenciee", "tourbiere_minerotrophe", "tourbiere_ombrotrophe", "twi", "urban", "water", "wetland", "distance_to_lakes", "distance_to_rivers", "distance_to_streams", "distance_to_stlawrence")
#pp <- p$large[[other_vars]]
#name_vars <- c("% bas de pentes", "% plat", "relief accidenté", "élévation", "sable", "argile", "% forêt", "% urbain", "% eau", "% milieu humide", "% agricole")
#names(pp) <- name_vars
#use_small <- unique(c(c("urban", "geomflat"), sample(small_vars)))
#use_small <- use_small[1:min(c(length(use_small), 2))]
use_small <- unique(c(c("urban", "cropland", "alluvion", "clay", "till", "ph", "elevation", "geomflat", "water", "eau_peu_profonde","organique", "ruggedness", "distance_to_lakes", "distance_to_rivers", "distance_to_streams", "distance_to_stlawrence"), sample(small_vars)))
use_small <- use_small[1:min(c(length(use_small), 100))]


models <- list(
  "climat" = climate_vars,
  "gam" = climate_vars,
  "habitat" = c(large_vars),
  "small" = use_small,
  "climat + habitat" = c(climate_vars, large_vars),
  "climat (habitat)" = c(1, 3), 
  "gam (habitat)" = c(2, 3),
  "climat (small)" = c(1, 4),
  "gam (small)" = c(2, 4)
)

model_names <- list(
  "climat" = "climat",
  "gam" = "climatGAM",
  "habitat" = "habitatNA",
  "small" = "habitatQC",
  "climat + habitat" = "habitatNA + climat",
  "climat (habitat)" = "habitatNA (climat)", 
  "gam (habitat)" = "habitatNA (climatGAM)",
  "climat (small)" = "habitatQC (climat)",
  "gam (small)" = "habitatQC (climatGAM)"
)

models <- models[run_model]
model_names <- model_names[run_model]

#plot(p$large[[large_vars]], mar = c(0, 0, 1, 0), axes = FALSE, plg = list(inset = c(0.5, 0.5)))


st_bbox(p$small)
png(file.path("results/graphics/predictors.png"), width = 12, height = 18, units = "in", res = 300)
plot(p$small[[small_vars]], mar = c(0, 0, 2, 1), axes = FALSE, plg = list(x = st_bbox(p$small)$xmax - 2e05, y = st_bbox(p$small)$ymax - 2e05, xjust = 1, yjust = 1, size = c(0.25, 0.75)), maxnl = 1)
dev.off()






if(FALSE){

  library(vegan)
  r <- p$small
  climate <- grep("temperature|precipitation|isothermality", names(r), value = TRUE)
  rr <- aggregate(r[[climate]], 10, na.rm = TRUE)
  #screeplot(rda(values(rr), scale = TRUE))
  pca <- rda(values(rr), scale = TRUE)
  barplot(cumsum(eigenvals(pca)/sum(eigenvals(pca))))
  
  r <- p$small
  rr <- aggregate(r, 10, na.rm = TRUE)
  chosen <- climate
  chosen <- c("mean_annual_air_temperature", "annual_precipitation_amount", "annual_range_of_air_temperature")
  chosen <- names(rr)
  chosen <- use_small
  chosen <- setdiff(use_small, c("lacustre", "human_modification", "organique"))
  v <- values(rr) |> as.data.frame()
  cor(v[, chosen], use = "complete.obs")

  car::vif(lm(y ~ ., data = cbind(y = 1, v[, chosen])))

}