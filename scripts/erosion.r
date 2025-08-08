

library(rcoleo)
library(geodata)
library(dplyr)
library(sf)
library(rmapshaper)
library(FRutils)
library(rnaturalearth)

source("https://raw.githubusercontent.com/frousseu/FRutils/refs/heads/master/R/colo.scale.R")

epsg <- 6624 #32618

# Downloads polygons using package geodata
# Downloads polygons using package geodata
can <- gadm("CAN", level = 2, path = "data") |> st_as_sf()
usa <- gadm("USA", level = 2, path = "data") |> st_as_sf()
na <- rbind(can, usa)
na <- st_transform(na, epsg)
na <- na[!na$NAME_1 %in% c("Hawaii"), ]
na <- na[-grep("Aleutians", na$NAME_2), ]
qc <- na[na$NAME_1 %in% c("Québec"), ] |>
  st_transform(epsg) |>
  ms_simplify(0.01)
na <- st_union(na) |>
  ms_simplify(0.01) |>
  st_as_sf()

# lakes
lakes <- ne_download(scale = "medium", type = "lakes",destdir = "data", category = "physical", returnclass = "sf") |> st_transform(epsg)
lakes <- st_filter(lakes, qc)  


# oiseau <- coleo_request_data(survey_type = "acoustique_oiseaux", view = "long")
# chiro <- coleo_request_data(survey_type = "acoustique_chiropteres", view = "long")
# anoure <- coleo_request_data(survey_type = "acoustique_anoures", view = "long")
# ortho <- coleo_request_data(survey_type = "acoustique_orthopteres", view = "long")
# mamm <- coleo_request_data(survey_type = "mammiferes", view = "long") # retriré car données extra bruyante - contient toutes catégories de faune observée, ex oiseau, anoure, etc ...
# pap <- coleo_request_data(survey_type = "papilionides", view = "long")
# odo <- coleo_request_data(survey_type = "odonates", view = "long")
# ins <- coleo_request_data(survey_type = "insectes_sol", view = "long")
# zoop <- coleo_request_data(survey_type = "zooplancton", view = "long")
# benth <- coleo_request_data(survey_type = "benthos", view = "long")
# vege <- coleo_request_data(survey_type = "vegetation", view = "long")



x <- coleo_request_data(survey_type = "acoustique_oiseaux", view = "short") |>
  rename(site_code = "Code du site") |>
  rename(species = "Nom scientifique valide") |>
  rename(date = "Date de l'inventaire") |>
  mutate(day = substr(date, 6, 10)) |>
  filter(day >= "06-01" & day <= "08-01") |>
  as.data.frame()

all_sites <- coleo_request_general("sites", response_as_df = TRUE, schema = "public") |>
  mutate(lon = sapply(geom$coordinates, "[", 1)) |>
  mutate(lat = sapply(geom$coordinates, "[", 2)) |>
  as.data.frame() |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(epsg)

sites <- all_sites[all_sites$site_code %in% x$site_code, ]

sp <- "Melospiza lincolnii"
sites$pres <- as.integer(sites$site_code %in% x$site_code[which(x$species == sp)])

par(mar = c(0, 0, 0, 0))
plot(st_geometry(qc), border = NA, col = "grey90")
points(sites, pch = 21, bg = adjustcolor("forestgreen", 0.8), col = adjustcolor("black", 0.5), lwd = 0.5)
points(sites[sites$pres == 1, ], pch = 21, bg = adjustcolor("red", 0.8), col = adjustcolor("black", 0.5), lwd = 0.5)


####################################################
####################################################

#source("scripts/prelim.r")


base_url <- "https://object-arbutus.cloud.computecanada.ca"
bucket <- "bq-io/niches_climatiques/summer_blitz" 
sdm <- sprintf('s5cmd --no-sign-request ls --exclude "*range_proj*" s3://%s/', bucket) |>
system(intern = TRUE) |>
strsplit(" ") |>
sapply(function(i){i[length(i)]}) |>
(\(.) file.path(base_url, bucket, .))()


sdm <- grep(".gpkg", list.files("results/rasters", pattern = "_range_large", full = TRUE), value = TRUE) 


l <- lapply(sdm, function(i){
    print(i)
    model <- "climat"
    r2 <- st_read(gsub("range", "range_proj", i), layer = model)
    r1 <- st_read(i, layer = model) 


    minus <- st_difference(r1, r2)
    plus <- st_difference(r2, r1)
    equal <- st_intersection(r2, r1)
    if(FALSE){
      cols <- adjustcolor(c("blue", "tomato", "darkgreen"), 0.5)
      plot(st_geometry(na))
      plot(st_geometry(minus), col = cols[1], border = NA, add = TRUE)
      plot(st_geometry(plus), col = cols[2], border = NA, add = TRUE)
      plot(st_geometry(equal), col = cols[3], border = NA, add = TRUE)

      legend("topright", inset = c(0.1, 0.1), legend = c("Perte", "Gain", "Stable")[c(2, 3, 1)], pch = 15, pt.cex = 2, col = cols[c(2, 3, 1)], bty = "n", xjust = 1, xpd = TRUE)
      mtext(side = 3, text = i, line = -2, cex = 2)
      Sys.sleep(2)
    }
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
#equal <- do.call("rbind", lapply(l, function(i){i$equal})) |>
#  rasterize(r, fun = "count", background = 0) |>
#  crop(qc, mask = TRUE)
plus <- ((plus) / length(sdm)) * 100
minus <- ((-minus) / length(sdm)) * 100
tot <- plus - minus

plg <- list(x = 5e+05, y = 1.8e+06, size = c(0.4, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out", title = "Changement \n(% des espèces)", title.cex = 1.25)
div_cols <- rev(c("darkred", "tomato", "grey90", "blue", "navyblue"))
foreground <- function(){
  plot(st_geometry(qc), add = TRUE, border = adjustcolor("white", 0.01))
  plot(st_geometry(lakes), border = NA, col = "white", add = TRUE)
  points(all_sites, pch = 16, col = adjustcolor("black", 0.8))
}


png("results/graphics/erosion.png", units = "in", height = 4, width = 10, res = 300)

par(mfrow = c(1, 3), oma = c(0.5, 0, 0, 0))

plg$title <- "Colonisation \n(% des espèces)\n"
zlim <- range(global(plus, range, na.rm = TRUE))
cols <- adjustcolor(colo.scale(seq(min(zlim), max(zlim), length.out = 500), div_cols, center = TRUE), 0.75)
plot(plus, col = cols, mar = c(0, 0, 0, 2), axes = FALSE, plg = plg)
foreground()

plg$title <- "Disparition \n(% des espèces)\n"
zlim <- range(global(minus, range, na.rm = TRUE))
cols <- adjustcolor(colo.scale(seq(min(zlim), max(zlim), length.out = 500), div_cols, center = TRUE), 0.75)
plot(minus, col = cols, mar = c(0, 0, 0, 2), axes = FALSE, plg = plg)
foreground()

plg$title <- "Changement \n(% des espèces)\n"
zlim <- range(global(tot, range, na.rm = TRUE))
cols <- adjustcolor(colo.scale(seq(min(zlim), max(zlim), length.out = 500), c("darkgreen", "forestgreen", "grey90", "forestgreen", "darkgreen"), center = TRUE), 0.75)
plot(tot, col = cols, mar = c(0, 0, 0, 2), axes = FALSE, plg = plg)
foreground()

dev.off()


#################################
#################################
#################################
#################################
#################################

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




