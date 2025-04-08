
plot_background <- function(){
  plot(st_geometry(na), border = NA, col = "grey90") 
}

plot_foreground <- function(observations = FALSE){
  if(observations){
    plot(st_geometry(obs), col = adjustcolor("orange", 0.5), pch = 16, cex = 0.25, add = TRUE)
  }
  plot(st_geometry(na), lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
  plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
}


# file_sdm <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_sdm.tif"))
# file_sdm_proj <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_sdm_proj.tif"))
# file_range <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_range.tif"))
# file_range_proj <- file.path("results/rasters", paste0(gsub(" ", "_", sp), "_range_proj.tif"))
# file_pol <- gsub(".tif", ".gpkg", file_range)
# file_pol_proj <- gsub(".tif", ".gpkg", file_range_proj)

topng <- function(x){
  if(any(grepl(".tif", x))){
    gsub("/rasters/", "/graphics/", gsub(".tif", ".png", x))
  } else {
    gsub("/rasters/", "/graphics/", gsub(".gpkg", ".png", x))
  }
}

topng(file_range_proj)


plg <- list(size = c(0.5, 1.5), tic = "out")
#sdm_cols <- terrain.colors(200)
sdm_cols <- colo.scale(1:200, c("grey90", "palegreen3", "forestgreen", "darkgreen"))[1:175]
range_cols <- adjustcolor("forestgreen", 0.5)

png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_sdm.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot_background()
plot(crop(predictions, st_buffer(obs, 1500000)), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols)
plot_foreground()
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_predictions.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot_background()
plot(crop(predictions, st_buffer(obs, 1500000)), axes = FALSE, add = TRUE, plg = plg, col = sdm_cols)
#plot(st_geometry(polran), col = adjustcolor("black", 0.2), border = NA, add = TRUE)
plot_foreground(observations = TRUE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()

png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_range.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot_background()
plot(st_geometry(polran), col = range_cols, border = NA, add = TRUE)
plot_foreground(observations = TRUE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_predictions_proj.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot_background()
plot(crop(predictions_proj, st_buffer(obs, 1500000)), axes = FALSE, add = TRUE, plg = plg, col = sdm_cols)
#plot(st_geometry(polran_proj), col = adjustcolor("black", 0.2), border = NA, add = TRUE)
plot_foreground(observations = TRUE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_range_proj.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot_background()
plot(st_geometry(polran_proj), col = range_cols, border = NA, add = TRUE)
plot_foreground(observations = FALSE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_predictions_diff.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
dif <- crop(predictions, st_buffer(obs, 1500000)) - crop(predictions_proj, st_buffer(obs, 1500000))
se <- unlist(global(dif, range, na.rm = TRUE)[1, ])
cols <- adjustcolor(colo.scale(seq(min(se), max(se), length.out = 500), c("darkred", "tomato", "white", "blue", "navyblue"), center = TRUE), 0.5)
plot_background()
plot(dif, axes = FALSE, col = cols, add = TRUE, plg = plg)
plot_foreground()
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_diffrange.png")), units = "in", height = 6, width = 6, res = 300)
par(mar = c(0, 0, 0, 0))
minus <- st_difference(polran, polran_proj)
plus <- st_difference(polran_proj, polran)
equal <- st_intersection(polran_proj, polran)
plot_background()
cols <- adjustcolor(c("blue", "tomato", "darkgreen"), 0.5)
plot(st_geometry(minus), col = cols[1], border = NA, add = TRUE)
plot(st_geometry(plus), col = cols[2], border = NA, add = TRUE)
plot(st_geometry(equal), col = cols[3], border = NA, add = TRUE)
plot_foreground()
legend("bottomright", inset = c(0.1, 0.1), legend = c("Perte", "Gain", "Stable")[c(2, 3, 1)], pch = 15, pt.cex = 2, col = cols[c(2, 3, 1)], bty = "n", xjust = 1, xpd = TRUE)
dev.off()


png(file.path("results/graphics", paste0(gsub(" ", "_", sp), "_marginal_effects.png")), units = "in", height = 6, width = 8, res = 300)
e1 <- extract(p[[vars]], obs)
e2 <- extract(p[[vars]], bg)
reg <- st_buffer(st_convex_hull(obs),500000)
g <- global(crop(p[[vars]], region, mask = TRUE), mean, na.rm = TRUE)
gr <- global(crop(p[[vars]], region, mask = TRUE), range, na.rm = TRUE)
par(mfrow = n2mfrow(nrow(g)))
ran <- invisible(range(unlist(sapply(1:nrow(g), function(i){
    brks <- 500
    newdata <- t(g) |> as.data.frame()
    newdata <- newdata[rep(1, brks), , drop = FALSE]
    v <- seq(gr[i, 1], gr[i, 2], length.out = brks)
    newdata[ ,i] <- v
    pred <- predict(m, newdata, args = c("doClamp=FALSE"))
    range(pred, na.rm = TRUE)
}))))
invisible(lapply(1:nrow(g), function(i){
    brks <- 500
    newdata <- t(g) |> as.data.frame()
    newdata <- newdata[rep(1, brks), , drop = FALSE]
    v <- seq(gr[i, 1], gr[i, 2], length.out = brks)
    newdata[ ,i] <- v
    par(mar = c(2, 2, 1, 1))
    pred <- predict(m, newdata, args = c("doClamp=FALSE"))
    plot(v, pred, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, max(ran, na.rm = TRUE)))
    axis(1, mgp = c(0, -0.10, 0), tcl = -0.2, cex.axis = 0.5)
    axis(2, mgp = c(1, 0.25, 0), tcl = -0.2, cex.axis = 0.5, las = 2)
    grid()
    box(col = "grey90")
    mtext(side = 1, line = 1, text = rownames(g)[i], cex = 0.35)
    mtext(side = 2, line = 1, text = "prediction", cex = 0.5)
    hbrks <- range(c(e1[ , rownames(g)[i]], e2[ , rownames(g)[i]]), na.rm = TRUE) 
    hbrks <- seq(min(hbrks), max(hbrks), length.out = 50)
    h1 <- hist(e1[ , rownames(g)[i]], breaks = hbrks, plot = FALSE)
    h2 <- hist(e2[ , rownames(g)[i]], breaks = hbrks, plot = FALSE)
    h <- h1
    h$counts <- scales::rescale(h1$counts + h2$counts, to = c(0, par("usr")[4] * 0.35))
    h$density <- h1$density / h2$density
    h$density <- scales::rescale(h$density, to = c(0, par("usr")[4] * 0.35))
    invisible(lapply(seq_along(h$mids), function(j){
      rect(xleft = h$breaks[j], ybottom = 0, xright = h$breaks[j + 1], ytop = h$density[j], col = adjustcolor("black", 0.15), border = NA)    
    }))
    lines(h$mids, h$counts, col = adjustcolor("black", 0.25))
}))
dev.off()
