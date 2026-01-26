
# create tif/gpkg files for the first model, else append
if(i %in% c(1, 4)){ 
  overwrite <- TRUE
  gdal <- c("BIGTIFF=YES")
  append <- FALSE
  delete_dsn <- TRUE
  delete_layer <- TRUE
} else {
  overwrite <- FALSE
  gdal <- c("APPEND_SUBDATASET=YES", "BIGTIFF=YES")
  append <- TRUE
  delete_dsn <- FALSE
  delete_layer <- TRUE
}

#(sp<-species[5])
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
    hab <- rast(file_sdm)[[names(models)[models[[i]]][2]]]
    clim <- rast(file_range)[[names(models)[models[[i]]][1]]] # cut habitat with dichotomized climate distributions
    predictions <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
    #png("test.png", width = 12, height = 6, units = "in", res = 300); plot(crop(c(predictions, predictions2), qc, mask = TRUE)); dev.off()
    predictions_proj <- lapply(scenarios, function(s) {
      hab <- rast(file_sdm_proj)[[paste(names(models)[models[[i]]][2], s)]]
      clim <- rast(file_range_proj)[[paste(names(models)[models[[i]]][1], s)]]
      predictions_proj <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
      names(predictions_proj) <- paste(names(models[i]), s)
      predictions_proj
    })
    names(predictions_proj) <- scenarios
  } else {
    hab <- rast(file_sdm)[[names(models)[models[[i]]][2]]]
    clim <- rast(gsub("_small", "_large", file_range))[[names(models)[models[[i]]][1]]] |>
              project(hab) |> 
              mask(hab)
    predictions <- hab * (clim / global(clim, "max", na.rm = TRUE)[1, 1])
    predictions_proj <- lapply(scenarios, function(s) {
          hab <- rast(file_sdm_proj)[[paste(names(models)[models[[i]]][2], s)]]
          clim <- rast(gsub("_small", "_large", file_range_proj))[[paste(names(models)[models[[i]]][1], s)]] |>
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
    g <- c("APPEND_SUBDATASET=YES", "BIGTIFF=YES")
  }
  writeRaster(predictions_proj[[j]], file_sdm_proj, overwrite = o, gdal = g)
  predictions_proj[[j]]
})
names(predictions_proj) <- ns
#r <- rast("results/rasters/Pseudacris_triseriata_sdm_proj_large.tif")


### threshold sdm from obs
threshold1 <- 0.95
threshold2 <- 0.95
e1 <- extract(predictions, obs[[dataunc]][[echelle]][qc, ])
e2 <- extract(predictions, obs[[dataunc]][[echelle]][st_difference(region, qc), ])
#e <- rbind(e1)#, e2)
e1 <- e1[rev(order(e1[,2])), ]
val1 <- e1[round(threshold1 * nrow(e1)), 2] # this needs to be rethoughted...
e2 <- e2[rev(order(e2[,2])), ]
val2 <- e2[round(threshold2 * nrow(e2)), 2]
val <- min(c(val1, val2))


### threshold sdm from sens_spec in Quebec

cropzone <- na[na$NAME_1 %in% c("Québec","Ontario","New Brunswick","Nova Scotia","Prince Edward Island","Newfoundland and Labrador","Vermont","New Hampshire","Maine", "New York", "Massachusetts", "Rhode Island", "Connecticut", "Pennsylvania", "New Jersey", "Michigan", "Minnesota", "Wisconsin"), ]
#cropzone <- qc

zone <- crop(predictions, cropzone, mask = TRUE) # here restrict to Quebec
predvalues <- values(zone)[, 1]

presence <- rasterize(obs[[dataunc]][[echelle]], zone, fun = "count", background = 0)
presence <- ifel(presence > 0, 1, 0) |> mask(zone) |> values() |> _[,1]
usen <- sum(presence, na.rm = TRUE)
pp <- predvalues[sample(which(presence == 1), usen)]

absence <- rasterize(bg[[dataunc]][[echelle]], zone, fun = "count", background = 0)
absence <- ifel(absence == 0, 1, 0) |> mask(zone) |> values() |> _[,1]
aa <- predvalues[sample(which(absence == 1 & presence == 0), usen)]

e <- evaluate2(p = pp, a = aa)
val <- dismo::threshold(e)[['spec_sens']]

ran <- ifel(predictions > val, 1, 0)
names(ran) <- names(models[i])
writeRaster(ran, file_range, overwrite = overwrite, gdal = gdal)

polran <- ifel(ran == 1, 1, NA) |>
  as.polygons() |>
  st_as_sf()

#mean(lengths(st_intersects(obs[["small"]], polran)))


#png("st.png", width = 12, height = 12, units = "in", res = 300);plot(ran);plot(st_geometry(polran), add = TRUE);plot(st_geometry(na), add = TRUE);plot(st_geometry(obs[[dataunc]][[echelle]]), cex = 0.5, col = "orange2", add = TRUE);dev.off()
#png("st.png", width = 12, height = 12, units = "in", res = 300);plot(presence);plot(st_geometry(na), add = TRUE);dev.off()
#png("st.png", width = 12, height = 12, units = "in", res = 300);plot(absence);plot(st_geometry(na), add = TRUE);dev.off()





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
    g <- c("APPEND_SUBDATASET=YES", "BIGTIFF=YES")
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




if(FALSE){

  ### PA threshold

  #usen <- 200


  zone <- crop(predictions, qc, mask = TRUE)
  predvalues <- values(zone)[, 1]

  presence <- rasterize(obs[[echelle]], zone, fun = "count", background = 0)
  presence <- ifel(presence > 0, 1, 0) |> mask(zone) |> values() |> _[,1]
  usen <- sum(presence, na.rm = TRUE)
  pp <- predvalues[sample(which(presence == 1), usen)]

  absence <- rasterize(bg[[dataunc]][[echelle]], zone, fun = "count", background = 0)
  absence <- ifel(absence == 0, 1, 0) |> mask(zone) |> values() |> _[,1]
  aa <- predvalues[sample(which(absence == 1 & presence == 0), usen)]

  e <- evaluate2(p = pp, a = aa)
  threshold(e)

  ran2 <- ifel(predictions > threshold(e)[['spec_sens']], 1, 0)

  png("test.png",width=8,height=4,units="in",res=200)
  par(mfrow=c(1,2),mar=c(0,0,0,0))
  plot(crop(ran,qc))
  plot(st_geometry(qc),add=TRUE)
  plot(st_geometry(obs[[echelle]]), bg = adjustcolor("orange", 0.70), col = "black", pch = 21, cex = 0.4, lwd= 0.10, add = TRUE)
  plot(crop(ran2,qc))
  plot(st_geometry(qc),add=TRUE)
  plot(st_geometry(obs[[echelle]]), bg = adjustcolor("orange", 0.70), col = "black", pch = 21, cex = 0.4, lwd= 0.10, add = TRUE)
  dev.off()




  png("test.png",width=4,height=4,units="in",res=200);plot(mask(presence, predictions));dev.off()
  png("test.png",width=4,height=4,units="in",res=200);plot(e, 'ROC');dev.off()
  png("test.png",width=4,height=4,units="in",res=200);plot(e, 'TPR');dev.off()


  e <- evaluate2(p = plogis(rnorm(100, -20, 1)), a = plogis(rnorm(100, -30, 1)))
  threshold(e)








}



#st_layers(file_pol_proj)
#s <- st_read(file_pol_proj)


#ppp <- predictions
#names(ppp) <- "climat + habitat"
#ppp_proj <- predictions_proj
#names(ppp_proj) <- "climat (habitat)"

#writeRaster(ppp, file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")), overwrite = TRUE)

#writeRaster(ppp_proj, file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")),  gdal = "APPEND_SUBDATASET=YES")

#r <- rast(file.path("results/rasters", paste0(gsub(" ", "_", sp), "_test.tif")))


