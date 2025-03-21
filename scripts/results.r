

png(file.path("results/graphics", paste(gsub(" ", "_", sp), "_sdm.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot(crop(predictions, st_buffer(obs, 1500000)), axes = FALSE)
plot(st_geometry(na), lwd = 0.5, border = adjustcolor("black", 0.5), add = TRUE)
plot(st_geometry(obs), col = adjustcolor("black", 0.25), pch = 16, cex = 0.25, add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()

png(file.path("results/graphics", paste(gsub(" ", "_", sp), "_predictions.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot(crop(predictions, st_buffer(obs, 1500000)), axes = FALSE)
plot(st_geometry(na), lwd = 0.5, border = adjustcolor("black", 0.5), add = TRUE)
plot(st_geometry(obs), col = adjustcolor("black", 0.25), pch = 16, cex = 0.25, add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
plot(st_geometry(polran), col = adjustcolor("black", 0.2), border = NA, add = TRUE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()

png(file.path("results/graphics", paste(gsub(" ", "_", sp), "_predictions_proj.png")), units = "in", height = 6, width = 8, res = 300)
par(mar = c(0, 0, 0, 8))
plot(crop(predictions_proj, st_buffer(obs, 1500000)), axes = FALSE)
plot(st_geometry(na), lwd = 0.5, border = adjustcolor("black", 0.5), add = TRUE)
plot(st_geometry(obs), col = adjustcolor("black", 0.25), pch = 16, cex = 0.25, add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
plot(st_geometry(polran_proj), col = adjustcolor("black", 0.2), border = NA, add = TRUE)
legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()


png(file.path("results/graphics", paste(gsub(" ", "_", sp), "_marginal_effects.png")), units = "in", height = 6, width = 8, res = 300)
e1 <- extract(p[[vars]], obs)
e2 <- extract(p[[vars]], bg)
reg <- st_buffer(st_convex_hull(obs),500000)
g <- global(crop(p[[vars]], reg, mask = TRUE), mean, na.rm = TRUE)
gr <- global(crop(p[[vars]], reg, mask = TRUE), range, na.rm = TRUE)
par(mfrow = n2mfrow(nrow(g)))
invisible(lapply(1:nrow(g), function(i){
    brks <- 500
    newdata <- t(g) |> as.data.frame()
    newdata <- newdata[rep(1, brks), , drop = FALSE]
    v <- seq(gr[i, 1], gr[i, 2], length.out = brks)
    newdata[ ,i] <- v
    par(mar = c(2, 2, 1, 1))
    pred <- predict(m, newdata)
    plot(v, pred, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, max(pred, na.rm = TRUE)))
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
