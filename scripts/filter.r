





if(sp == "Pseudacris triseriata"){
  
# wget -O Pseudacris_triseriata.kml https://www.inaturalist.org/taxa/24267/range.kml"
# wget -O Pseudacris_maculata.kml https://www.inaturalist.org/taxa/24255/range.kml

pt <- st_read("data/Pseudacris_triseriata.kml") |>
  ms_explode()

pm <- st_read("data/Pseudacris_maculata.kml") |>
  ms_explode()

pm <- pm[-which.max(st_area(pm)), ]

x <- rbind(pt, pm) |>
  st_buffer(10000) |>
  st_union() |>
  st_as_sf() |>
  st_transform(epsg)

o <- !(as.logical(lengths(st_intersects(obs, na[na$NAME_1 %in% c("Québec"), ]))) & !as.logical(lengths(st_intersects(obs, st_buffer(aire, 50000)))))

obs <- obs[o, ]

o <- !(!as.logical(lengths(st_intersects(obs, na[na$NAME_1 %in% c("Québec"), ]))) & !as.logical(lengths(st_intersects(obs, x))))

obs <- obs[o, ]

#x <- gbif |>
#  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
#  filter(species == "Pseudacris triseriata") |>
#  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
#  mutate(dataset_name = institutioncode) |>
#  count(dataset_name) |>
#  arrange(-n) |>
#  as.data.frame()

#x <- gbif |>
#  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
#  filter(species == "Pseudacris triseriata") |>
#  mutate(coordinate_uncertainty = as.numeric(coordinateuncertaintyinmeters)) |>
#  mutate(dataset_name = institutioncode) |>
#  filter(dataset_name == "iNaturalist") |>
#  as.data.frame() |>
#  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
#  st_transform(epsg)

png("filter.png", width = 9, height = 9, units = "in", res = 300)
par(mar = c(0, 0, 0, 0))
plot(st_geometry(na))
#plot(st_geometry(na[na$NAME_1 %in% c("Ontario"), ]))
plot(st_geometry(aire), border = NA, col = adjustcolor("black", 0.10), add = TRUE)
plot(st_geometry(obs), add = TRUE)
#plot(st_geometry(obs[o, ]), pch = 16, col = "red", add = TRUE)
#plot(st_geometry(x), pch = 16, col = "forestgreen", add = TRUE)
dev.off()


}