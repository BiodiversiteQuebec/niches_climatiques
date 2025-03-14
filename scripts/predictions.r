

predictions <- mask(predict(m, p, args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
predictions_proj <- mask(predict(m, p_proj, args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
#preds <- aggregate(predictions, 100, fun = "max", na.rm = TRUE)
preds<- predictions
preds_proj <- predictions_proj
dif <- preds_proj - preds
rr <- unlist(global(dif, "range", na.rm = TRUE)[1, ])

writeRaster(predictions, file.path("results/rasters", paste(gsub(" ", "_", sp), ".tif")), overwrite = TRUE)
writeRaster(predictions_proj, file.path("results/rasters", paste(gsub(" ", "_", sp), "_proj.tif")), overwrite = TRUE)

threshold <- 0.98
e <- extract(predictions, obs[qc, ])
e <- e[rev(order(e[,2])), ]
val <- e[round(threshold * nrow(e)), 2]

ran <- ifel(predictions > val, 1, 0)

polran <- ifel(ran == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()

ran_proj <- ifel(predictions_proj > val, 1, 0)

polran_proj <- ifel(ran_proj == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()

