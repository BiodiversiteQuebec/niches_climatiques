
lf <- list.files("results/rasters", pattern = "_sdm_small|_sdm_large", full = TRUE)

lapply(lf, function(i){
    png(gsub("_sdm.tif", "_sdm_compare.png", gsub("/rasters/", "/graphics/", i)), units = "in", height = 6, width = 18, res = 300)
    r <- rast(i)
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    plot(crop(r, qc, mask = TRUE), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 4, fun = plot_foreground())
    #plot_foreground(observation = FALSE)
    dev.off()
})


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





