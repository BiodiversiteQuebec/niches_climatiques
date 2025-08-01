
#source("scripts/prelim.r")

rm(predictions, predictions_proj)

#lf <- list.files("results/rasters", pattern = "_sdm_large", full = TRUE)
lf <- gsub("_sdm_small.tif", "_sdm_large.tif", file_sdm)

lapply(lf, function(xx){
    fn <- gsub("_sdm_large.tif", "_sdm_compare.png", gsub("/rasters/", "/graphics/", xx))
    #print(paste("fn", fn))
    png(fn, units = "in", height = 12, width = 10, res = 300)
    r1 <- rast(xx)#[[1:6]]
    r2 <- rast(gsub("_large", "_small", xx))
    r3 <- project(r1, r2)
    r4 <- c(r3, r2)
    r5 <- crop(r4, qc, mask = TRUE)
    #writeRaster(r5, "results/rasters/temp.tif", overwrite = TRUE)
    #r <- rast("results/rasters/temp.tif")
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    #par(mfrow = n2mfrow(nlyr(r)), mar = c(0, 0, 0, 0))
    #for(k in 1:nlyr(r)){
      #plot(r, axes = FALSE, col = sdm_cols, legend = FALSE)
    #}
    r5 <- r5[[names(model_names)]]
    plot(r5, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 3, fun = function(){plot_foreground(observations = TRUE, echelle = "small")}, main = unlist(model_names))
    #plot_foreground(observation = FALSE)
    dev.off()
    #rm(r)
    #rm(r1, r2, r3, r4)
    #graphics.off()
    #par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1,2.1))
})

lapply(lf, function(i){
    png(gsub("_sdm_large.tif", "_sdm_compare_localized.png", gsub("/rasters/", "/graphics/", i)), units = "in", height = 8, width = 10, res = 300)
    r1 <- rast(i)
    r2 <- rast(gsub("_large", "_small", i))
    r1 <- project(r1, r2)
    r <- c(r1, r2)
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    r <- r[[names(model_names)]]
    plot(crop(crop(r, st_buffer(obs$small, 100000), mask = FALSE), qc, mask = TRUE), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 3, fun = function(){plot_foreground(observations = TRUE, echelle = "small")}, main = unlist(model_names))
    #plot_foreground(observation = FALSE)
    dev.off()
    #graphics.off()
    #par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1,2.1))
})

#dev.list()
graphics.off()
#par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1,2.1))





if(FALSE){

  sp <- gsub(" ", "_", species[6])

  lf <- list.files("results/rasters", pattern = sprintf("%s_sdm_small.tif|%s_sdm_large.tif", sp, sp), full = TRUE)

  lg <- list.files("results/rasters", pattern = sprintf("%s_range_small.gpkg|%s_range_large.gpkg", sp, sp), full = TRUE)[1] |>
    st_read(layer = "climat + habitat") |>
    st_crop(qc) |>
    st_bbox() |>
    st_as_sfc() |>
    st_buffer(200000)

  lr <- lapply(lf, rast)
  lr[[1]] <- project(lr[[1]], lr[[2]])
  r <- rast(lr) |>
    crop(qc, mask = TRUE)

  #plot(st_geometry(lg))
  plot(crop(r[[1]], lg)) 
  plot(st_geometry(lg), add = TRUE, col = adjustcolor("black", 0.1), border = NA)

}



