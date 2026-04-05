
cat(paste(paste(format(Sys.time(), "%H:%M:%S %Y-%m-%d"), "running", sp, "graphics.r", sep = " - "), "\n"))

#source("scripts/prelim.r")

rm(predictions, predictions_proj)

add_range2 <- function(){
  if(!is.null(aire)){
    plot(st_geometry(aire), border = adjustcolor("black", 0.25), lwd = 2, add = TRUE)
  }
}

plot_lakes <- function(){
  plot(st_geometry(lakes), col = "white", lwd = 0.1, border = NA, add = TRUE)
  plot(st_geometry(rivers), col = "white", lwd = 0.1, add = TRUE)
}

plot_max <- function(x){
  plot(st_geometry(x), border = NA, col = "white") 
}


hydrolakes <- st_read("data/HydroLAKES_polys_v10.gdb")
lakes <- hydrolakes |> 
  filter(Continent == "North America") |>
  filter(Lake_area >= 10) |>
  st_transform(epsg) |>
  st_filter(na)

hydrorivers <- st_read("data/HydroRIVERS_v10.gdb")
rivers <- hydrorivers |> 
  filter(UPLAND_SKM >= 1000) |>
  st_transform(epsg) |>
  st_crop(na)

#png("plot.png", width = 10, height = 10, units = "in", res = 500)
#par(mar = c(0, 0, 0, 0))
#plot_background()
#plot(st_geometry(qc), col = "grey90", border = NA)
#plot(st_geometry(lakes), col = "white", border = NA, add = TRUE)
#plot(st_geometry(rivers), col = "white", lwd = 0.5, add = TRUE)
#dev.off()


keep <- c("Saguenay")
conditions <- paste(sprintf("TOPONYME LIKE '%%%s%%'", keep), collapse = " OR ")
query <- paste("SELECT * FROM rivers WHERE", conditions)
saguenay <- st_read("data/grhq.gpkg", query = query) |>
  st_transform(epsg)

#rivieres <- st_read("data/grhq.gpkg", query = "SELECT * FROM rivers WHERE ST_Area(Shape) > 200 * 200") |>
#  st_transform(6624)

lakes <- st_read("data/grhq.gpkg", query = "SELECT * FROM lakes WHERE ST_Area(Shape) > 10000000") |> 
  st_transform(epsg) |>
  rbind(saguenay)






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
### Compare projections sdm for climate only ##########################
lf <- gsub("_sdm_proj_small.tif", "_sdm_proj_large.tif", file_sdm_proj)

display_model <- "climatGAM (habitatQC)"
if(grepl("QC", display_model)){
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


#######################################################################
### Compare projections range for climate only ########################
lf <- gsub("_range_proj_small.tif", "_range_proj_large.tif", file_range_proj)

display_model <- "climatGAM (habitatQC)"
if(grepl("QC", display_model)){
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
    fn <- gsub("_range_proj_large.tif|_range_proj_small.tif", "_range_proj_compare.png", gsub(path_raster, "results/graphics", xx))
    png(fn, units = "in", height = nc[1] * 5, width = nc[2] * 3.5, res = 300)
    plot(r5, axes = FALSE, add = FALSE, plg = plg, col = c(sdm_cols[1], range_cols), legend = FALSE, mar = c(0, 0, 3, 0), nc = nc[2], fun = function(){plot_foreground(observations = FALSE, echelle = "small")}, main = display_names)
    #plot_foreground(observation = FALSE)
    dev.off()
})




##############################################################################
### Compare projections range for climate only ###############################
bregion <- qc

lf <- gsub("_range_proj_small.gpkg", "_range_proj_large.gpkg", file_pol_proj)

display_model <- "climatGAM (habitatQC)"
if(grepl("QC", display_model)){
  lf <- gsub("large", "small", lf)
}
display_name <- model_names[[display_model]]
selected <- paste(display_model, scenarios)
selected <- c(display_model, selected)
display_names <- paste0(display_name, "\n", scenarios)

current <- st_read(gsub("_proj_", "_", lf), layer = display_model)
maximum <- st_read(lf, layer = gsub("\n", " ", tail(display_names, 1)))
cols <- adjustcolor(c("tomato", "blue", "darkgreen"), 0.5)
fn <- gsub("_range_proj_large.gpkg|_range_proj_small.gpkg", "_range_proj_small.png", gsub(path_raster, "results/graphics", lf))
png(fn, units = "in", height = 12, width = 9, res = 500)
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0), oma = c(4, 4, 2.5, 0))
lapply(display_names, function(xx){
    if(grepl("_2041", xx)){
      plot_background()
      plot(st_geometry(current), col = cols[3], border = NA, add = TRUE)
      plot_lakes()
      mtext(outer = FALSE, side = 2, line = 1, text = unique(sort(sapply(strsplit(xx, "\n|_"), "[", 2))), xpd = TRUE, adj = 0.5, font = 2, col = "grey70", cex = 2.5)
      if(grepl("ssp585", xx)){
        mtext(outer = FALSE, side = 1, line = 2, text = "Actuel", xpd = TRUE, adj = 0.35, col = "grey70", cex = 2.5, font = 2)
      }
    } 
    plot_background()
    projected <- st_read(lf, layer = gsub("\n", " ", xx))
    plot(st_geometry(projected), col = cols[3], border = NA, add = TRUE)
    plot_lakes()
    if(grepl("ssp585", xx)){
      mtext(outer = FALSE, side = 1, line = 2, text = unique(sort(sapply(strsplit(xx, "\n|_"), "[", 3))), xpd = TRUE, adj = 0.35, col = "grey70", cex = 2.5, font = 2)
    } 
    #plot(st_geometry(st_bbox(qc) |> st_as_sfc() |> st_as_sf()), border = "red", xpd = TRUE, add = TRUE)
    #plot(st_geometry(bregion), axes = TRUE, xaxs = "i", yaxs = "i")
})
dev.off()




##############################################################################
### Compare projections range change for climate only ########################
bregion <- qc

lf <- gsub("_range_proj_small.gpkg", "_range_proj_large.gpkg", file_pol_proj)

display_model <- "climatGAM (habitatQC)"
if(grepl("QC", display_model)){
  lf <- gsub("large", "small", lf)
}
display_name <- model_names[[display_model]]
selected <- paste(display_model, scenarios)
selected <- c(display_model, selected)
display_names <- paste0(display_name, "\n", scenarios)

current <- st_read(gsub("_proj_", "_", lf), layer = display_model)
maximum <- st_read(lf, layer = gsub("\n", " ", tail(display_names, 1)))
cols <- adjustcolor(c("tomato", "blue", "darkgreen"), 0.5)
fn <- gsub("_range_proj_large.gpkg|_range_proj_small.gpkg", "_range_proj_small_change.png", gsub(path_raster, "results/graphics", lf))
png(fn, units = "in", height = 11, width = 9, res = 500)
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0), oma = c(4, 4, 2.5, 0))
lapply(display_names, function(xx){
    if(grepl("_2041", xx)){
      plot_background()
      plot(st_geometry(current), col = cols[3], border = NA, add = TRUE)
      plot_lakes()
      mtext(outer = FALSE, side = 2, line = 1, text = unique(sort(sapply(strsplit(xx, "\n|_"), "[", 2))), xpd = TRUE, adj = 0.5, font = 2, col = "grey70", cex = 2.5)
      if(grepl("ssp585", xx)){
        mtext(outer = FALSE, side = 1, line = 2, text = "Actuel", xpd = TRUE, adj = 0.35, col = "grey70", cex = 2.5, font = 2)
      }
    } 
    plot_background()
    projected <- st_read(lf, layer = gsub("\n", " ", xx))
    minus <- st_difference(current, projected)
    plus <- st_difference(projected, current)
    equal <- st_intersection(projected, current) |> st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON") |> st_union()
    plot(st_geometry(minus), col = cols[1], border = NA, add = TRUE)
    plot(st_geometry(plus), col = cols[2], border = NA, add = TRUE)
    plot(st_geometry(equal), col = cols[3], border = NA, add = TRUE)
    plot_lakes()
    if(grepl("ssp585", xx)){
      mtext(outer = FALSE, side = 1, line = 2, text = unique(sort(sapply(strsplit(xx, "\n|_"), "[", 3))), xpd = TRUE, adj = 0.35, col = "grey70", cex = 2.5, font = 2)
    } 
})
par(mfrow = c(1, 1), mar = c(3.5, 0, 0, 0), oma = c(0, 0, 0, 0), new = TRUE)
legend("top", inset = c(0, -0.03), pch = 15, pt.cex = 4, cex = 2.5, legend = c("Perte", "Gain", "Stable")[c(3, 2, 1)], col = cols[c(3, 2, 1)], bty = "n", xpd = NA, text.font = 2, text.col = "grey70", horiz = TRUE)

dev.off()





##############################################################################
### Compare projections range change for climate only ########################
bregion <- na

lf <- gsub("_range_proj_small.gpkg", "_range_proj_large.gpkg", file_pol_proj)

display_model <- "climatGAM (habitatNA)"
if(grepl("QC", display_model)){
  lf <- gsub("large", "small", lf)
}
display_name <- model_names[[display_model]]
selected <- paste(display_model, scenarios)
selected <- c(display_model, selected)
display_names <- paste0(display_name, "\n", scenarios)

current <- st_read(gsub("_proj_", "_", lf), layer = display_model)
maximum <- st_read(lf, layer = gsub("\n", " ", tail(display_names, 1)))
cols <- adjustcolor(c("tomato", "blue", "darkgreen"), 0.5)
fn <- gsub("_range_proj_large.gpkg|_range_proj_small.gpkg", "_range_proj_large_change.png", gsub(path_raster, "results/graphics", lf))
png(fn, units = "in", height = 11, width = 9, res = 500)
par(mfrow = c(3, 3), mar = c(0, 0, 0, 0), oma = c(4, 4, 2.5, 0))
lapply(display_names, function(xx){
    if(grepl("_2041", xx)){
      plot_background()
      plot(st_geometry(current), col = cols[3], border = NA, add = TRUE)
      plot_lakes()
      mtext(outer = FALSE, side = 2, line = 1, text = unique(sort(sapply(strsplit(xx, "\n|_"), "[", 2))), xpd = TRUE, adj = 0.5, font = 2, col = "grey70", cex = 2.5)
      if(grepl("ssp585", xx)){
        mtext(outer = FALSE, side = 1, line = 2, text = "Actuel", xpd = TRUE, adj = 0.35, col = "grey70", cex = 2.5, font = 2)
      }
    } 
    plot_background()
    projected <- st_read(lf, layer = gsub("\n", " ", xx))
    minus <- st_difference(current, projected)
    plus <- st_difference(projected, current)
    equal <- st_intersection(projected, current) |> st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON") |> st_union()
    plot(st_geometry(minus), col = cols[1], border = NA, add = TRUE)
    plot(st_geometry(plus), col = cols[2], border = NA, add = TRUE)
    plot(st_geometry(equal), col = cols[3], border = NA, add = TRUE)
    plot_lakes()
    if(grepl("ssp585", xx)){
      mtext(outer = FALSE, side = 1, line = 2, text = unique(sort(sapply(strsplit(xx, "\n|_"), "[", 3))), xpd = TRUE, adj = 0.35, col = "grey70", cex = 2.5, font = 2)
    } 
})
par(mfrow = c(1, 1), mar = c(3.5, 0, 0, 0), oma = c(0, 0, 0, 0), new = TRUE)
legend("top", inset = c(0, -0.03), pch = 15, pt.cex = 4, cex = 2.5, legend = c("Perte", "Gain", "Stable")[c(3, 2, 1)], col = cols[c(3, 2, 1)], bty = "n", xpd = NA, text.font = 2, text.col = "grey70", horiz = TRUE)

dev.off()


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

if(TRUE){
  #species_list <- gsub(" ", "_", species)
  species_list <- gsub(" ", "_", sp)
  for (ss in species_list) {
    spe <- file.path(path_raster, ss)
    temp <- substr(basename(spe), 1, 5) # the temp file so files do not overwrite themselves
    cmd <- paste0(
      "cp ", spe, "_range_large.gpkg ", path_raster, "/", temp, ".gpkg && ",
      #"for f in ", spe, "*.gpkg; do ",
      "for f in $(ls ", spe, "*.gpkg | grep -v observation | grep -v _all.gpkg); do ",
      "ogr2ogr -f GPKG -update -overwrite ", path_raster, "/", temp, ".gpkg \"$f\"; done && ",
      "mv ", path_raster, "/", temp, ".gpkg ", spe, "_all.gpkg"
    )
    cat(cmd, "\n")
    system(cmd)
  }
}




if(FALSE){

   ### check mean date of observations ####################################
   path_raster <- system("echo $SCRATCH", intern = TRUE) # results/rasters
   lf <- list.files(file.path(path_raster), full = TRUE, pattern = "observations")
   l <- lapply(lf, st_read, layer = "climate", quiet = TRUE) |>
     lapply((\(.){.[, c("date", "source")]})) |>
     do.call("rbind", args = _) |>
     filter(source != "ebird")

   c(mean(as.integer(substr(l$date, 1, 4)), na.rm = TRUE), median(as.integer(substr(l$date, 1, 4)), na.rm = TRUE))

   png("plot.png", width = 10, height = 8, units = "in", res= 300)
   hist(as.integer(substr(l$date, 1, 4)), breaks = seq(1800, 2030, by = 5), xlim = c(1950, 2030))
   dev.off()  

   ### more precise changes ###############################################
   rcl <- matrix(c(
    0.0, 0.1, 1,  # good
    0.1, 0.2, 2,  # bad
    0.3, 0.5, 3,  # ok
    0.5, 0.7, 4   # nice
   ), ncol = 3, byrow = TRUE)
   r_classified <- classify(my_raster, rcl)




    mo <- "climatX2"
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
    st_read(layer = "climatX2 + habitatNA") |>
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

