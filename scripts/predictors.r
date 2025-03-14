

predictors <- rast("data/predictors.tif")
predictors_proj <- rast("data/predictors_proj.tif")


p <- aggregate(predictors, 2, na.rm = TRUE)
p_proj <- aggregate(predictors_proj, 2, na.rm = TRUE)





