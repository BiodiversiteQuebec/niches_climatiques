
source("scripts/prelim.r")

lf <- list.files("results/rasters", pattern = "_sdm_large", full = TRUE)

lapply(lf, function(i){
    png(gsub("_sdm_large.tif", "_sdm_compare.png", gsub("/rasters/", "/graphics/", i)), units = "in", height = 12, width = 10, res = 300)
    r1 <- rast(i)
    r2 <- rast(sub("_large", "_small", i))
    r1 <- project(r1, r2)
    r <- c(r1, r2)
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    plot(crop(r, qc, mask = TRUE), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 3, fun = plot_foreground)
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





