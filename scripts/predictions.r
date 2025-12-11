
# create tif/gpkg files for the first model, else append
if(i %in% c(1, 4)){ 
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
  if(!grepl("gam", names(models)[i])){
    predictions <- mask(predict(m, p[[echelle]][[vars]], args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
    predictions_proj <- lapply(scenarios, function(s) {mask(predict(m, p_proj[[echelle]][[s]][[vars]], args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))})
    names(predictions_proj) <- scenarios
  } else {
    preds <- predict(m, as.data.frame(cbind(values(p[[echelle]][[vars]]), eff = 1000)), type = "response")
    predictions <- p[[echelle]][[1]]
    predictions[] <- preds
    predictions <- mask(predictions, vect(region))
    predictions_proj <- lapply(scenarios, function(s) {
      preds <- predict(m, as.data.frame(cbind(values(p_proj[[echelle]][[s]][[vars]]), eff = 1000)), type = "response")
      pp <- p[[echelle]][[1]]
      pp[] <- preds
      mask(pp, vect(region))
    })
    names(predictions_proj) <- scenarios
 }
} else {
  if(echelle == "large"){
    preds <- rast(file_sdm)[[names(models)[models[[i]]]]]
    predictions <- preds[[2]] * (preds[[1]] / global(preds[[1]], "max", na.rm = TRUE)[1, 1])
    predictions_proj <- lapply(scenarios, function(s) {
      preds <- rast(file_sdm_proj)[[paste(names(models)[models[[i]]], s)]]
      predictions_proj <- preds[[2]] * (preds[[1]] / global(preds[[1]], "max", na.rm = TRUE)[1, 1])
      names(predictions_proj) <- paste(names(models[i]), s)
      predictions_proj
    })
    names(predictions_proj) <- scenarios
  } else {
    hab <- rast(file_sdm)[[names(models)[models[[i]]][2]]]
    clim <- rast(gsub("_small", "_large", file_sdm))[["climat"]] |>
              project(hab) |> 
              mask(hab)
    predictions <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
    predictions_proj <- lapply(scenarios, function(s) {
          hab <- rast(file_sdm_proj)[[paste(names(models)[models[[i]]][2], s)]]
          clim <- rast(gsub("_small", "_large", file_sdm_proj))[[paste("climat", s)]] |>
              project(hab) |> 
              mask(hab)
          predictions_proj <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
          names(predictions_proj) <- paste(names(models[i]), s)
          predictions_proj
    })
    names(predictions_proj) <- scenarios
  }
}

### produce predictions rasters for all scenarios
names(predictions) <- names(models[i])
writeRaster(predictions, file_sdm, overwrite = overwrite, gdal = gdal)


ns <- names(predictions_proj)
predictions_proj <- lapply(seq_along(predictions_proj), function(j){
  names(predictions_proj[[j]]) <- paste(names(models[i]), names(predictions_proj)[j])
  if(j == 1){
    o <- overwrite
    g <- gdal
  } else {
    o <- FALSE
    g <- "APPEND_SUBDATASET=YES"
  }
  writeRaster(predictions_proj[[j]], file_sdm_proj, overwrite = o, gdal = g)
  predictions_proj[[j]]
})
names(predictions_proj) <- ns
#r <- rast("results/rasters/Pseudacris_triseriata_sdm_proj_large.tif")


### threshold sdm from obs
threshold1 <- 0.98
threshold2 <- 0.98
e1 <- extract(predictions, obs[[echelle]][qc, ])
e2 <- extract(predictions, obs[[echelle]][st_difference(region, qc), ])
#e <- rbind(e1)#, e2)
e1 <- e1[rev(order(e1[,2])), ]
val1 <- e1[round(threshold1 * nrow(e1)), 2] # this needs to be rethoughted...
e2 <- e2[rev(order(e2[,2])), ]
val2 <- e2[round(threshold2 * nrow(e2)), 2]
val <- min(c(val1, val2))

ran <- ifel(predictions > val, 1, 0)
names(ran) <- names(models[i])
writeRaster(ran, file_range, overwrite = overwrite, gdal = gdal)

polran <- ifel(ran == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()

st_write(polran, file_pol, layer = names(models[i]), append = append, delete_dsn = delete_dsn, delete_layer = delete_layer)

pr_proj <- lapply(seq_along(predictions_proj), function(j){
  ran_proj <- ifel(predictions_proj[[j]] > val, 1, 0)
  names(ran_proj) <- paste(names(models[i]), names(predictions_proj)[j])
  if(j == 1){
    o <- overwrite
    g <- gdal
    a <- append
    dd <- delete_dsn
    dl <- delete_layer
  } else {
    o <- FALSE
    g <- "APPEND_SUBDATASET=YES"
    a <- TRUE  
    dd <- FALSE
    dl <- TRUE
  }
writeRaster(ran_proj, file_range_proj, overwrite = o, gdal = g)
polran_proj <- ifel(ran_proj == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()
st_write(polran_proj, file_pol_proj, layer = paste(names(models[i]), names(predictions_proj)[j]), append = a, delete_dsn = dd, delete_layer = dl)
list(ran_proj = ran_proj, polran_proj = polran_proj)
})

ran_proj <- lapply(pr_proj, "[[", 1)
names(ran_proj) <- scenarios

polran_proj <- lapply(pr_proj, "[[", 2)
names(polran_proj) <- scenarios





#st_layers(file_pol_proj)
#s <- st_read(file_pol_proj)


#ppp <- predictions
#names(ppp) <- "climat + habitat"
#ppp_proj <- predictions_proj
#names(ppp_proj) <- "climat (habitat)"

#writeRaster(ppp, file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")), overwrite = TRUE)

#writeRaster(ppp_proj, file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")),  gdal = "APPEND_SUBDATASET=YES")

#r <- rast(file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")))


