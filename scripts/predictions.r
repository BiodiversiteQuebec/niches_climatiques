
# create tif/gpkg files for the first model, else append
if(i %in% c(1, 3)){ 
  overwrite <- TRUE
  gdal <- ""
  append <- FALSE
  delete_dsn <- TRUE
  delete_layer <- TRUE
} else {
  overwrite <- FALSE
  gdal <- "APPEND_SUBDATASET=YES"
  append <- TRUE
  delete_dsn <- FALSE
  delete_layer <- TRUE
}


file_sdm <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_sdm_", echelle, ".tif"))
file_sdm_proj <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_sdm_proj_", echelle, ".tif"))
file_range <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_range_", echelle, ".tif"))
file_range_proj <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_range_proj_", echelle, ".tif"))
file_pol <- gsub(".tif", ".gpkg", file_range)
file_pol_proj <- gsub(".tif", ".gpkg", file_range_proj)


if(is.character(models[[i]])){
  predictions <- mask(predict(m, p[[echelle]][[vars]], args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
  predictions_proj <- mask(predict(m, p_proj[[echelle]][[vars]], args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
} else { # this needs adapting for 2-scale model
  if(echelle == "large"){
    preds <- rast(file_sdm)[[names(models)[models[[i]]]]]
    predictions <- preds[[2]] * (preds[[1]] / global(preds[[1]], "max", na.rm = TRUE)[1, 1])
    preds <- rast(file_sdm_proj)[[names(models)[models[[i]]]]]
    predictions_proj <- preds[[2]] * (preds[[1]] / global(preds[[1]], "max", na.rm = TRUE)[1, 1])
  } else {

    hab <- rast(file_sdm)[[names(models)[models[[i]]][2]]]
    clim <- rast(gsub("_small", "_large", file_sdm))[["climat"]] |>
              project(hab) |> 
              mask(hab)
    predictions <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
    hab <- rast(file_sdm_proj)[[names(models)[models[[i]]][2]]]
    clim <- rast(gsub("_small", "_large", file_sdm_proj))[["climat"]] |>
              project(hab) |> 
              mask(hab)
    predictions_proj <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
  }
}


names(predictions) <- names(models[i])
writeRaster(predictions, file_sdm, overwrite = overwrite, gdal = gdal)

names(predictions_proj) <- names(models[i])
writeRaster(predictions_proj, file_sdm_proj, overwrite = overwrite, gdal = gdal)


threshold1 <- 0.99
threshold2 <- 0.95
e1 <- extract(predictions, obs[[echelle]][qc, ])
e2 <- extract(predictions, obs[[echelle]][st_difference(region, qc), ])
e <- rbind(e1)#, e2)
e <- e[rev(order(e[,2])), ]
val <- e[round(threshold1 * nrow(e)), 2]

ran <- ifel(predictions > val, 1, 0)
names(ran) <- names(models[i])
writeRaster(ran, file_range, overwrite = overwrite, gdal = gdal)

polran <- ifel(ran == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()

st_write(polran, file_pol, layer = names(models[i]), append = append, delete_dsn = delete_dsn, delete_layer = delete_layer)

ran_proj <- ifel(predictions_proj > val, 1, 0)
names(ran_proj) <- names(models[i])
writeRaster(ran_proj, file_range_proj, overwrite = overwrite, gdal = gdal)

polran_proj <- ifel(ran_proj == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()

st_write(polran_proj, file_pol_proj, layer = names(models[i]), append = append, delete_dsn = delete_dsn, delete_layer = delete_layer)



#st_layers(file_pol_proj)
#s <- st_read(file_pol_proj)


#ppp <- predictions
#names(ppp) <- "climat + habitat"
#ppp_proj <- predictions_proj
#names(ppp_proj) <- "climat (habitat)"

#writeRaster(ppp, file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")), overwrite = TRUE)

#writeRaster(ppp_proj, file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")),  gdal = "APPEND_SUBDATASET=YES")

#r <- rast(file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")))


