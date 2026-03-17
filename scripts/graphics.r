
#source("scripts/prelim.r")

rm(predictions, predictions_proj)

add_range2 <- function(){
  if(!is.null(aire)){
    plot(st_geometry(aire), border = adjustcolor("black", 0.25), lwd = 2, add = TRUE)
  }
}

#############################################################################
### Compare actual models ###################################################
lf <- gsub("_sdm_small.tif", "_sdm_large.tif", file_sdm)

### QC wide
lapply(lf, function(xx){
    fn <- gsub("_sdm_large.tif", "_sdm_compare.png", gsub(path_raster, "results/graphics", xx))
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

### Localized
lapply(lf, function(i){
    png(gsub("_sdm_large.tif", "_sdm_compare_localized.png", gsub(path_raster, "results/graphics", i)), units = "in", height = 8, width = 10, res = 300)
    r1 <- rast(i)
    r2 <- rast(gsub("_large", "_small", i))
    r1 <- project(r1, r2)
    r <- c(r1, r2)
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    r <- r[[names(model_names)]]
    plot(crop(crop(r, st_buffer(obs[["habitat"]]$small, 150000), mask = FALSE), qc, mask = TRUE), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 3, fun = function(){plot_foreground(observations = TRUE, echelle = "small");add_range2()}, main = unlist(model_names))
    #plot_foreground(observation = FALSE)
    dev.off()
    #graphics.off()
    #par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1,2.1))
})

#dev.list()
graphics.off()
#par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1,2.1))


### Localized
lapply(lf, function(i){
    png(gsub("_sdm_large.tif", "_sdm_compare_localized_noobs.png", gsub(path_raster, "results/graphics", i)), units = "in", height = 8, width = 10, res = 300)
    r1 <- rast(i)
    r2 <- rast(gsub("_large", "_small", i))
    r1 <- project(r1, r2)
    r <- c(r1, r2)
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    r <- r[[names(model_names)]]
    plot(crop(crop(r, st_buffer(obs[["habitat"]]$small, 150000), mask = FALSE), qc, mask = TRUE), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 3, fun = function(){}, main = unlist(model_names))
    #plot_foreground(observation = FALSE)
    dev.off()
    #graphics.off()
    #par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1,2.1))
})



#######################################################################
### Compare projections models for climate only #######################
lf <- gsub("_sdm_proj_small.tif", "_sdm_proj_large.tif", file_sdm_proj)

display_model <- "climat (small)"
if(grepl("small", display_model)){
  lf <- gsub("large", "small", lf)
}
display_name <- model_names[[display_model]]
selected <- paste(display_model, scenarios)
selected <- c(display_model, selected)
display_names <- paste0(display_name, "\n", c("actuel", scenarios))

lapply(lf, function(xx){
    #print(paste("fn", fn))
    r1 <- rast(xx)#[[1:6]]
    r2 <- rast(gsub("_large", "_small", xx))[[1]]
    r0 <- rast(gsub("_proj", "", xx)) |> project(r2)
    r3 <- project(r1, r2)
    r4 <- c(r0, r3)#, r2)
    r5 <- crop(r4, qc, mask = TRUE)
    r5 <- r5[[names(r5) %in% selected]]
    nc <- n2mfrow(nlyr(r5), asp = 3/1)
    fn <- gsub("_sdm_proj_large.tif|_sdm_proj_small.tif", "_sdm_proj_compare.png", gsub(path_raster, "results/graphics", xx))
    png(fn, units = "in", height = nc[1] * 5, width = nc[2] * 3.5, res = 300)
    plot(r5, axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 3, 0), nc = nc[2], fun = function(){plot_foreground(observations = FALSE, echelle = "small")}, main = display_names)
    #plot_foreground(observation = FALSE)
    dev.off()
})

graphics.off()


#sp <- "Galucomys volans"
#path_raster

#lf <- list.files(path_raster, pattern = "range", full = TRUE)

#cp Pseudacris_triseriata_range_small.gpkg merged.gpkg && for f in Pseuda*.gpkg; do ogr2ogr -f GPKG -update merged.gpkg "$f"; done


#for species in Pseudacris_triseriata Aquila_chrysaetos Bombycilla_garrulus; do
#    cp ${species}_range_small.gpkg ${species}_merged.gpkg
#    for f in ${species}*.gpkg; do ogr2ogr -f GPKG -update ${species}_merged.gpkg "$f"; done
#done

#species_list <- c("Pseudacris_triseriata", "Aquila_chrysaetos", "Bombycilla_garrulus")

if(FALSE){
  species_list <- gsub(" ", "_", species)[11]
  for (spe in species_list) {
    spe <- file.path(path_raster, spe)

    cmd <- paste0(
      "cp ", spe, "_range_large.gpkg ", path_raster, "/merged.gpkg && ",
      #"for f in ", spe, "*.gpkg; do ",
      "for f in $(ls ", spe, "*.gpkg | grep -v observation); do ",
      "ogr2ogr -f GPKG -update -overwrite ", path_raster, "/merged.gpkg \"$f\"; done && ",
      "mv ", path_raster, "/merged.gpkg ", spe, "_all.gpkg"
    )
    #cat(cmd)
    system(cmd)
  }
}




if(FALSE){

    mo <- "climat"
    sc <- "ssp370"
    ye <- c("2030", "2060", "2090")
    sc_ye <- paste(sc, ye, sep = "_")
    cases <- expand.grid(mo, sc_ye) |>
      apply(1, paste, collapse = " ")

    layers <- st_layers("/scratch/frousseu/Glaucomys_volans_range_proj_large.gpkg")$name

    cols <- rev(c("grey20", "grey30", "grey40", "grey50"))
    png("plot.png", width = 10, height = 10, units = "in", res = 300)
    par(mar = c(0, 0, 0, 0))
    plot(st_geometry(na))
    plot(st_geometry(st_read("/scratch/frousseu/Pseudacris_triseriata_range_large.gpkg", layer = mo)), col = cols[1], border = NA, add = TRUE)
    lapply(seq_along(cases), function(i){
      polran <- st_read("/scratch/frousseu/Pseudacris_triseriata_range_proj_large.gpkg", layer = cases[i])
      plot(st_geometry(polran), col = cols[i+1], border = NA, add = TRUE)
    })
    dev.off()
    system("code plot.png")


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



  ### test cutoff threshold 
  i <- 6
  preds1 <- rast(file_range)[[names(models)[models[[i]]]]]
  preds2 <- rast(file_sdm)[[names(models)[models[[i]]]]]
  preds <- c(preds1[[1]], preds2[[2]])
  predictions <- preds[[2]] * (preds[[1]] / global(preds[[1]], "max", na.rm = TRUE)[1, 1])

  png("test.png", width = 6, height = 6, units = "in", res = 300)
  plot(predictions)
  plot(st_geometry(qc), add = TRUE)
  dev.off()
  

  st_layers(file.path(paste(sp, "_observations.gpkg")))




}



#library(magick)
#im <- image_read("results/graphics/niches/Gyrinophilus_porphyriticus_sdm_compare_localized.png") |>  
#  image_quantize(10) |>
#  image_write("quantize.png")

