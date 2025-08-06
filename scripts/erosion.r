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
cols <- adjustcolor(colo.scale(seq(min(zlim), max(zlim), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
plot(r, col = cols)

zlim <- range(global(rast(l), range, na.rm = TRUE))
cols <- adjustcolor(colo.scale(seq(min(zlim), max(zlim), length.out = 500), rev(c("darkred", "tomato", "white", "blue", "navyblue")), center = TRUE), 0.5)
plot(rast(l), col = cols)




sdm <- grep(".gpkg", list.files("results/rasters", pattern = "_range_large", full = TRUE), value = TRUE) 

l <- lapply(sdm, function(i){
    print(i)
    model <- "climat"
    r2 <- st_read(gsub("range", "range_proj", i), layer = model)
    r1 <- st_read(i, layer = model) 


    minus <- st_difference(r1, r2)
    plus <- st_difference(r2, r1)
    equal <- st_intersection(r2, r1)
    #cols <- adjustcolor(c("blue", "tomato", "darkgreen"), 0.5)
    #plot(st_geometry(na))
    #plot(st_geometry(minus), col = cols[1], border = NA, add = TRUE)
    #plot(st_geometry(plus), col = cols[2], border = NA, add = TRUE)
    #plot(st_geometry(equal), col = cols[3], border = NA, add = TRUE)

    #legend("topright", inset = c(0.1, 0.1), legend = c("Perte", "Gain", "Stable")[c(2, 3, 1)], pch = 15, pt.cex = 2, col = cols[c(2, 3, 1)], bty = "n", xjust = 1, xpd = TRUE)
    list(minus = minus, plus = plus, equal = equal)


})

# Filter(Negate(is.null), my_list)

r <- rast(ext = ext(na), resolution = 5000, crs = crs(na))
minus <- do.call("rbind", lapply(l, function(i){i$minus})) |>
  rasterize(r, fun = "count", background = 0) |>
  crop(qc, mask = TRUE)
plus <- do.call("rbind", lapply(l, function(i){i$plus})) |>
  rasterize(r, fun = "count", background = 0) |>
  crop(qc, mask = TRUE)
equal <- do.call("rbind", lapply(l, function(i){i$equal})) |>
  rasterize(r, fun = "count", background = 0) |>
  crop(qc, mask = TRUE)


tot <- plus - minus
plot(tot)

