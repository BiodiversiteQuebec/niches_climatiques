
lf <- list.files("results/rasters", pattern = "_sdm.tif", full = TRUE)

lapply(lf, function(i){
    png(gsub("_sdm.tif", "_sdm_compare.png", gsub("/rasters/", "/graphics/", i)), units = "in", height = 6, width = 18, res = 300)
    r <- rast(i)
    #par(mar = c(0, 0, 0, 8))
    #plot_background()
    plot(crop(r, qc, mask = TRUE), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, legend = FALSE, mar = c(0, 0, 2, 0), nc = 4, fun = plot_foreground())
    #plot_foreground(observation = FALSE)
    dev.off()
})










