

## Choose scenario to produce image from
display_scenario <- scenarios[2]

predictions_proj <- predictions_proj[[display_scenario]]
ran_proj <- ran_proj[[display_scenario]]
polran_proj <- polran_proj[[display_scenario]]

#buffd <- 200*1000

if(echelle == "large"){
  bregion <- st_intersection(na, region)
} else {
  bregion <- st_intersection(qc, region)
}

plot_background <- function(){
  plot(st_geometry(bregion), border = NA, col = "grey90") 
}

plot_foreground <- function(observations = FALSE, echelle = "large"){
  if(observations){
    points(st_geometry(obs[[echelle]]), bg = adjustcolor("orange", 0.90), col = "black", pch = 21, cex = 0.4, lwd= 0.10)
  }
  plot(st_geometry(na), lwd = 0.1, border = adjustcolor("black", 0.75), add = TRUE)
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
    res <- gsub("/rasters/", "/graphics/", gsub(".tif", ".png", x))
  } else {
    res <- gsub("/rasters/", "/graphics/", gsub(".gpkg", ".png", x))
  }
  spf <- gsub(" ", "_", sp)
  gsub(spf, paste(spf, names(models)[i], sep = "_"), res)
}

#topng(file_range_proj)

# add scenario to figures
add_scenario <- function(x){
  text(par("usr")[1], par("usr")[4], adj = c(-0.01, 1.01), label = paste("Scénario de projection: ", display_scenario), cex = 1.25, col = "grey70", xpd = TRUE)
}


plg <- list(size = c(0.33, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out")
#plg <- list(size = c(0.5, 1.5))#, tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#ccc", tic = "out")
#sdm_cols <- terrain.colors(200)
sdm_cols <- coloScale(1:200, c("grey90", "palegreen3", "forestgreen", "darkgreen","black"))[1:170]
range_cols <- adjustcolor("forestgreen", 0.75)


png(topng(file_sdm), units = "in", height = 6, width = 7.5, res = 300)
#par(mar = c(0, 0, 0, 8))
#plot_background()
plot(crop(predictions, bregion), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 0))
#plot(crop(predictions, st_buffer(obs[[echelle]], buffd)), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 0))
#plot(predictions, axes = FALSE, plg = plg, col = sdm_cols)
plot_foreground(observation = TRUE, echelle = echelle)
dev.off()


png(topng(file_range), units = "in", height = 6, width = 6.5, res = 300)
par(mar = c(0, 0, 0, 0))
plot_background()
plot(st_geometry(polran), col = range_cols, border = NA, add = TRUE)
plot_foreground(observations = TRUE, echelle = echelle)
legend("topright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = range_cols, bty = "n", xjust = 1, xpd = TRUE)
dev.off()

png(topng(file_sdm_proj), units = "in", height = 6, width = 7.5, res = 300)
#par(mar = c(0, 0, 0, 8))
#plot_background()
plot(crop(predictions_proj, bregion), axes = FALSE, add = FALSE, plg = plg, col = sdm_cols, mar = c(0, 0, 0, 0))
#plot(st_geometry(polran_proj), col = adjustcolor("black", 0.2), border = NA, add = TRUE)
plot_foreground(observations = TRUE, echelle = echelle)
#legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = range_cols, bty = "n", xjust = 1, xpd = TRUE)
add_scenario()
dev.off()


png(topng(file_range_proj), units = "in", height = 6, width = 6.5, res = 300)
par(mar = c(0, 0, 0, 0))
plot_background()
plot(st_geometry(polran_proj), col = range_cols, border = NA, add = TRUE)
plot_foreground(observations = FALSE, echelle = echelle)
legend("topright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = range_cols, bty = "n", xjust = 1, xpd = TRUE)
add_scenario()
dev.off()


png(topng(gsub("_sdm", "_sdm_diff", file_sdm)), units = "in", height = 6, width = 7.5, res = 300)
#par(mar = c(0, 0, 0, 8))  
dif <- crop(predictions, bregion) - crop(predictions_proj, bregion)
se <- unlist(global(dif, range, na.rm = TRUE)[1, ])
if(all(se == 0)){ # when no diff cause habitat only model
  cols <- "white"
  dif <- setValues(dif, runif(ncell(dif)))# temp fix for plg terra prob when a single value raster
} else {
  cols <- adjustcolor(coloScale(seq(min(se), max(se), length.out = 500), c("darkred", "tomato", "white", "blue", "navyblue"), center = TRUE), 0.5)
}
#plot_background()
plot(dif, axes = FALSE, add = FALSE, plg = plg, col = cols, mar = c(0, 0, 0, 0))
plot_foreground(echelle = echelle)
#legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
add_scenario()
dev.off()


png(topng(gsub("_range", "_range_diff", file_range)), units = "in", height = 6, width = 6.5, res = 300)
par(mar = c(0, 0, 0, 0))
minus <- st_difference(polran, polran_proj)
plus <- st_difference(polran_proj, polran)
equal <- st_intersection(polran_proj, polran)
plot_background()
cols <- adjustcolor(c("blue", "tomato", "darkgreen"), 0.5)
plot(st_geometry(minus), col = cols[1], border = NA, add = TRUE)
plot(st_geometry(plus), col = cols[2], border = NA, add = TRUE)
plot(st_geometry(equal), col = cols[3], border = NA, add = TRUE)
plot_foreground(echelle = echelle)
legend("topright", inset = c(0.1, 0.1), legend = c("Perte", "Gain", "Stable")[c(2, 3, 1)], pch = 15, pt.cex = 2, col = cols[c(2, 3, 1)], bty = "n", xjust = 1, xpd = TRUE)
add_scenario()
dev.off()



if(is.character(models[[i]])){
  e1 <- extract(p[[echelle]][[vars]], obs[[echelle]])
  e2 <- terra::extract(p[[echelle]][[vars]], bg[[echelle]])
  #e1 <- extract(crop(p[[echelle]][[vars]], region, mask = TRUE), obs[[echelle]])
  #e2 <- extract(crop(p[[echelle]][[vars]], region, mask = TRUE), bg[[echelle]])
  reg <- st_buffer(st_convex_hull(obs[[echelle]]),500000)
  g <- global(crop(p[[echelle]][[vars]], region, mask = TRUE), mean, na.rm = TRUE)
  gr <- global(crop(p[[echelle]][[vars]], region, mask = TRUE), range, na.rm = TRUE)
  png(file.path("results/graphics", paste(gsub(" ", "_", sp), names(models)[i], "marginal_effects.png", sep = "_")), units = "in", height = ceiling(nrow(g)/3) * 1.5, width = 8, res = 300)
  par(mfrow = c(ceiling(nrow(g)/3), 3), oma = c(0, 2, 0, 0))
  ran <- invisible(range(unlist(sapply(1:nrow(g), function(j){
      brks <- 500
      newdata <- t(g) |> as.data.frame()
      newdata <- newdata[rep(1, brks), , drop = FALSE]
      #v <- seq(gr[j, 1], gr[j, 2], length.out = brks)
      v <- seq(gr[j, 1], quantile(e2[[rownames(g)[j]]], probs = c(0.99), na.rm = TRUE), length.out = brks)
      newdata[ ,j] <- v
      if(!grepl("gam", names(models)[i])){
        pred <- predict(m, newdata, args = c("doClamp=FALSE"))
      } else {
        pred <- predict(m, cbind(newdata, eff = 1000), type = "response")
      }
      range(pred, na.rm = TRUE)
  }))))
  invisible(lapply(1:nrow(g), function(j){
      brks <- 500
      newdata <- t(g) |> as.data.frame()
      newdata <- newdata[rep(1, brks), , drop = FALSE]
      #v <- seq(gr[j, 1], gr[j, 2], length.out = brks)
      v <- seq(gr[j, 1], quantile(e2[[rownames(g)[j]]], probs = c(0.99), na.rm = TRUE), length.out = brks)
      newdata[ ,j] <- v
      par(mar = c(2, 2, 0.5, 2))
      if(!grepl("gam", names(models)[i])){
        pred <- predict(m, newdata, args = c("doClamp=FALSE"))
      } else {
        pred <- predict(m, cbind(newdata, eff = 1000), type = "response")
      }
      ylim <- ran # common scale
      #ylim <- pred
      #xlim <- if(rownames(g)[j] == "distance_to_streams"){c(0, 1500)} else {range(v, na.rm = TRUE)}
      plot(v, pred, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, max(ylim, na.rm = TRUE)), lwd = 1.5, bty = "n")
      axis(1, mgp = c(0, -0.10, 0), tcl = -0.2, cex.axis = 0.5, lwd = 0)
      axis(2, mgp = c(1, 0.25, 0), tcl = -0.2, cex.axis = 0.5, las = 2, lwd = 0)
      grid(lwd = 0.5)
      box(col = "grey80", lwd = 0.5)
      mtext(side = 1, line = 0.5, outer = FALSE, text = rownames(g)[j], cex = 0.5)
      mtext(side = 2, line = 0.75, outer = TRUE, text = "Relative occurrence rate (ROC)", cex = 0.5)
      #hbrks <- range(c(e1[ , rownames(g)[j]], e2[ , rownames(g)[j]]), na.rm = TRUE) 
      hbrks <- range(v, na.rm = TRUE)
      hbrks <- seq(min(hbrks), max(hbrks), length.out = 30)
      ee1 <- e1[ , rownames(g)[j]]
      ee2 <- e2[ , rownames(g)[j]]
      h1 <- hist(ee1[ee1 >= min(hbrks) & ee1 <= max(hbrks)], breaks = hbrks, plot = FALSE)
      h2 <- hist(ee2[ee2 >= min(hbrks) & ee2 <= max(hbrks)], breaks = hbrks, plot = FALSE)
      h <- h1
      #h$counts <- scales::rescale(h1$counts + h2$counts, to = c(0, par("usr")[4] * 0.75)) # not sure why I was adding h1
      h$counts <- scales::rescale(h2$counts, to = c(0, par("usr")[4] * 0.75))
      h$density <- h1$density / h2$density
      h$density <- scales::rescale(h$density, to = c(0, par("usr")[4] * 0.75))
      invisible(lapply(seq_along(h$mids), function(j){
        rect(xleft = h$breaks[j], ybottom = 0, xright = h$breaks[j + 1], ytop = h$density[j], col = adjustcolor("forestgreen", 0.65), border = NA)    
      }))
      #lines(h$mids, h$counts, col = adjustcolor("black", 0.25))

      invisible(lapply(seq_along(h$mids), function(j){
        rect(xleft = h$breaks[j], ybottom = 0, xright = h$breaks[j + 1], ytop = h$counts[j], col = adjustcolor("black", 0.20), border = NA)    
      }))
      #polygon(c(h$mids, rev(h$mids)), c(h$counts, rep(0, length(h$mids))), col = adjustcolor("black", 0.20), border = NA)
      
      par(new = TRUE)
      plot(v, pred, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", ylim = c(0, max(pred, na.rm = TRUE)), lwd = 1, bty = "n", col = adjustcolor("black", 0.3))
      #axis(1, mgp = c(0, -0.10, 0), tcl = -0.2, cex.axis = 0.5, lwd = 0)
      axis(4, mgp = c(1, 0.25, 0), tcl = -0.2, cex.axis = 0.5, las = 2, lwd = 0, col.axis = adjustcolor("black", 0.5))

      if(j == 1){
        legend("topleft", inset = c(0.025, 0), legend = c("Prédictions (échelle commune, gauche)", "Prédictions (échelle individuelle, droite)", "Observations / (Observations + Background)", "Observations + Background"), cex = 0.5, bty = "n", lwd = c(1.5, 1, NA, NA), pch = c(NA, NA, 15, 15), col = c("black", adjustcolor("black", 0.3), adjustcolor("forestgreen", 0.65), adjustcolor("black", 0.20)), pt.cex = c(NA, NA, 1, 1))
      }

  }))
  dev.off()

}



if(FALSE){

  lf <- list.files("results/rasters", pattern = "sdm_large|sdm_small", full = TRUE)

  large <- rast(lf[1])
  small <- rast(lf[2])

  r <- c(project(large, small), small)


### EROSION

  #r1 <- crop(predictions, qc, mask = TRUE)
#r2 <- crop(predictions_proj, qc, mask = TRUE)
r1 <- predictions
r2 <- predictions_proj

ma <- unlist(global(r1, max, na.rm = TRUE)[1, ])
r1 <- r1 / ma
r2 <- r2 / ma


dif <- (r2 - r1) * 100

se <- unlist(global(dif, range, na.rm = TRUE)[1, ])
cols <- adjustcolor(coloScale(seq(min(se), max(se), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "darkblue")), center = TRUE), 0.5)

#plot_background()
plot(dif, axes = FALSE, add = FALSE, col = cols, mar = c(0, 0, 0, 0))
plot_foreground(echelle = echelle)

png("misc/erosion.png", units = "in", height = 6, width = 6, res = 200)
plg <- list(x = 7e+05, y = 2.1e+06, size = c(0.4, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out", title = "Changement (%)", title.cex = 1.25)
plot(crop(dif, qc, mask = TRUE), axes = FALSE, add = FALSE, col = cols, mar = c(0, 0, 0, 0), legend = TRUE, plg = plg)
plot(st_geometry(qc), lwd = 0.5, border = adjustcolor("black", 0.5), add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)
dev.off()
library(magick)
image_read("misc/erosion.png") |>
  image_scale("x500") |>
  image_write("misc/erosion.png")




sdm <- list.files("results/rasters", pattern = "_sdm_large", full = TRUE)[-9]
#sdm_proj <- list.files("results/rasters", pattern = "_sdm_proj_large", full = TRUE)[-9]

l <- lapply(sdm, function(i){
  r <- rast(gsub("sdm", "sdm_proj", i))[["climat (habitat)"]] / rast(i)[["climat (habitat)"]]
  #dif <- rast(i)[["climat (habitat)"]] - rast(gsub("sdm", "sdm_proj", i))[["climat (habitat)"]]
  r <- crop(r, qc, mask = TRUE)
  #se <- unlist(global(dif, range, na.rm = TRUE)[1, ])
  #if(all(se == 0)){ # when no diff cause habitat only model
  #  cols <- "white"
  #  dif <- setValues(dif, runif(ncell(dif)))# temp fix for plg terra prob when a single value raster
  #} else {
  #cols <- adjustcolor(coloScale(seq(min(se), max(se), length.out = 500), c("darkred", "tomato", "white", "blue", "navyblue"), center = TRUE), 0.5)
  #}
  #log(r)
}) 







r <- log(l[[1]])
r <- sum(rast(l))
ra <- global(r, range, na.rm = TRUE)
cols <- adjustcolor(coloScale(seq(min(ra), max(ra), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
plot(r, col = cols)


zlim <- range(global(rast(l), range, na.rm = TRUE))
cols <- adjustcolor(coloScale(seq(min(zlim), max(zlim), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
#plot(r, col = cols)
plot(rast(l), range = zlim, col = cols)
zlim <- range(global(mean(rast(l)), range, na.rm = TRUE))
cols <- adjustcolor(coloScale(seq(min(zlim), max(zlim), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
plot(mean(rast(l)), col = cols)




sdm <- list.files("results/rasters", pattern = "_sdm_large", full = TRUE)

l <- lapply(sdm, function(i){
  print(i)
  model <- "climat"
  r2 <- rast(gsub("sdm", "sdm_proj", i))[[model]]
  r1 <- rast(i)[[model]]  
  ma <- global(r1, max, na.rm = TRUE)[1, 1]
  r1 <- r1 / ma
  r2 <- r2 / ma
  r <- (r2 - r1) * 100
  r
}) 
r <- mean(rast(l))
r <- crop(mean(rast(l)), qc, mask = TRUE)

zlim <- range(global(r, range, na.rm = TRUE))
cols <- adjustcolor(coloScale(seq(min(zlim), max(zlim), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
plot(r, col = cols)

zlim <- range(global(rast(l), range, na.rm = TRUE))
cols <- adjustcolor(coloScale(seq(min(zlim), max(zlim), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
plot(rast(l), col = cols)




#par(mar = c(0, 0, 0, 8))  
dif <- crop(predictions, bregion) - crop(predictions_proj, bregion)
#ma <-
#se <- unlist(global(dif, range, na.rm = TRUE)[1, ])
if(all(se == 0)){ # when no diff cause habitat only model
  cols <- "white"
  dif <- setValues(dif, runif(ncell(dif)))# temp fix for plg terra prob when a single value raster
} else {
  cols <- adjustcolor(coloScale(seq(min(se), max(se), length.out = 500), c("darkred", "tomato", "white", "blue", "navyblue"), center = TRUE), 0.5)
}
#plot_background()
plot(dif, axes = FALSE, add = FALSE, plg = plg, col = cols, mar = c(0, 0, 0, 0))
plot_foreground(echelle = echelle)
#legend("bottomright", inset = c(0.1, 0.1), legend = "Range", pch = 15, pt.cex = 2, col = adjustcolor("black", 0.2), bty = "n", xjust = 1, xpd = TRUE)
dev.off()










}