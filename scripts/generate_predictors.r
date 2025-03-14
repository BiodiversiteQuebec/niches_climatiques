

### This script gathers predictors for modeling the distribution of emv species for the finance indicators

library(terra)
library(ranger)
#library(mapview)
library(rmapshaper)
#library(knitr)
#library(gdalcubes)
library(rstac)
library(stars)
library(FNN)
library(data.table)
library(knitr)
library(gdalcubes)
library(vegan)
library(FNN)

options(width = 150)

#terraOptions(memfrac = 0.8)
#terraOptions(verbose = TRUE)
#tmpFiles(TRUE, TRUE, TRUE, TRUE)

#soil <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/soilgrids/silt_0-5cm.tif")

io <- stac("https://io.biodiversite-quebec.ca/stac/")

collections <- io |> collections() |> get_request()

df<-data.frame(id=character(),title=character(),description=character())
for (c in collections[['collections']]){
  df<-rbind(df,data.frame(id=c$id,title=c$title,description=c$description))
}
kable(df)

it_obj <- io |>
  stac_search(collections = "chelsa-clim") |>
  post_request() |> items_fetch()
it_obj


df<-data.frame(id=character(),datetime=character(), description=character())
for (f in it_obj[['features']]){
  df<-rbind(df,data.frame(id=f$id,datetime=f$properties$datetime,description=f$properties$description))
}
kable(df)


##########################################
##########################################
##########################################

#p <- rast(ext(region), resolution = 1000, crs = crs(region))
ex <- c(xmin = -3828412.27173732, xmax = 1182937.28491704, ymin = -1949713.76131365, ymax = 4123691.94410149)
p <- rast(ext(ex), resolution = 1000, crs = crs("EPSG:6624"))

#plot(st_geometry(region))
#plot(ext(p), add = TRUE)

##########################################
### CHELSA CLIM ##########################
##########################################

# Create data frame
chelsa_vars <- data.frame(
  name = c(
    "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", 
    "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", 
    "bio14", "bio15", "bio16", "bio17", "bio18", "bio19"
  ),
  longname = c(
    "mean annual air temperature",
    "mean diurnal air temperature range",
    "isothermality",
    "temperature seasonality",
    "mean daily maximum air temperature of the warmest month",
    "mean daily minimum air temperature of the coldest month",
    "annual range of air temperature",
    "mean daily mean air temperatures of the wettest quarter",
    "mean daily mean air temperatures of the driest quarter",
    "mean daily mean air temperatures of the warmest quarter",
    "mean daily mean air temperatures of the coldest quarter",
    "annual precipitation amount",
    "precipitation amount of the wettest month",
    "precipitation amount of the driest month",
    "precipitation seasonality",
    "mean monthly precipitation amount of the wettest quarter",
    "mean monthly precipitation amount of the driest quarter",
    "mean monthly precipitation amount of the warmest quarter",
    "mean monthly precipitation amount of the coldest quarter"
  ),
  units = c(
    "°C", "°C", "°C", "°C/100", "°C", "°C", "°C", "°C", 
    "°C", "°C", "°C", "kg m-2 year-1", "kg m-2 month-1", 
    "kg m-2 month-1", "kg m-2", "kg m-2 month-1", 
    "kg m-2 month-1", "kg m-2 month-1", "kg m-2 month-1"
  ),
  scale = c(
    0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 
    0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1
  ),
  offset = c(
    -273.15, 0, 0, 0, -273.15, -273.15, 0, -273.15, -273.15, 
    -273.15, -273.15, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  description = c(
    "Mean annual daily mean air temperatures averaged over 1 year",
    "Mean diurnal range of temperatures averaged over 1 year",
    "Ratio of diurnal variation to annual variation in temperatures",
    "Standard deviation of the monthly mean temperatures",
    "The highest temperature of any monthly daily mean maximum temperature",
    "The lowest temperature of any monthly daily mean maximum temperature",
    "The difference between the maximum temperature of the warmest month and the minimum temperature of the coldest month",
    "The wettest quarter of the year is determined (to the nearest month)",
    "The driest quarter of the year is determined (to the nearest month)",
    "The warmest quarter of the year is determined (to the nearest month)",
    "The coldest quarter of the year is determined (to the nearest month)",
    "Accumulated precipitation amount over 1 year",
    "The precipitation of the wettest month",
    "The precipitation of the driest month",
    "The coefficient of variation of monthly precipitation expressed as a percentage of the mean",
    "The wettest quarter of the year is determined (to the nearest month)",
    "The driest quarter of the year is determined (to the nearest month)",
    "The warmest quarter of the year is determined (to the nearest month)",
    "The coldest quarter of the year is determined (to the nearest month)"
  )
)

chelsa_vars$longname <- gsub(" ", "_", chelsa_vars$longname)
chelsa_vars[, c("name", "longname")]


ids <- io |>
  stac_search(collections = "chelsa-clim") |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

#ids <- ids[1:2]
variables <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio12", "bio15")
variables <- paste0("bio", 1:19)
w <- which(ids %in% variables)

chelsa <- lapply(w, function(i){
  url <- io |>
    stac_search(collections = "chelsa-clim") |>
    post_request() |> 
    items_fetch() |>
    _$features[[i]]$assets[[1]]$href
  print(url)
  paste0('/vsicurl/', url) |>
    rast() |>
    project(p)
}) |> rast()

names(chelsa) <- chelsa_vars$longname[match(ids[w], chelsa_vars$name)]

rr <- aggregate(chelsa, 10, na.rm = TRUE)

screeplot(rda(values(rr), scale = TRUE))
pca <- rda(values(rr), scale = TRUE)

barplot(cumsum(eigenvals(pca)/sum(eigenvals(pca))))

#######################################
### EarthEnv geom #####################
#######################################

coll <- "earthenv_topography_derived"

ids <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

#ids <- ids[1:2]
variables <- c("geomflat_perc", "geomfootslope_per")
w <- which(ids %in% variables)

geom <- lapply(w, function(i){
  url <- io |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features[[i]]$assets[[1]]$href
  print(url)
  paste0('/vsicurl/', url) |>
    rast() |>
    project(p)
}) |> rast()

names(geom) <- ids[w]

#######################################
### EarthEnv topo #####################
#######################################

coll <- "earthenv_topography"

ids <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

#ids <- ids[1:2]
variables <- c("elevation", "vrm")
w <- which(ids %in% variables)

topo <- lapply(w, function(i){
  url <- io |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features[[i]]$assets[[1]]$href
  print(url)
  paste0('/vsicurl/', url) |>
    rast() |>
    project(p)
}) |> rast()

names(topo) <- ids[w]


#######################################
### Soilgrids #########################
#######################################

coll <- "soilgrids"

ids <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

#ids <- ids[1:2]
variables <- c("sand_0-5cm", "clay_0-5cm", "silt_0-5cm")
w <- which(ids %in% variables)

soils <- lapply(w, function(i){
  url <- io |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features[[i]]$assets[[1]]$href
  print(url)
  paste0('/vsicurl/', url) |>
    rast() |>
    project(p)
}) |> rast()

names(soils) <- ids[w]


#######################################
### CEC ###############################
#######################################

coll <- "cec_land_cover_percentage"

ids <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

#ids <- ids[1:2]
variables <- paste0("cec_land_cover_percent_class_", c(1, 2, 5, 6, 17, 18, 14, 15))
w <- which(ids %in% variables)

cec <- lapply(w, function(i){
  url <- io |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features[[i]]$assets[[1]]$href
  print(url)
  paste0('/vsicurl/', url) |>
    rast() |>
    project(p)
}) |> rast()

names(cec) <- ids[w]
forest <- sum(cec[[grep("class_6$|class_5$|class_2$|class_1$", names(cec), value = TRUE)]]) 
names(forest) <- "forest"
urban <- cec[[grep("class_17$", names(cec), value = TRUE)]]
names(urban) <- "urban"
water <- cec[[grep("class_18$", names(cec), value = TRUE)]]
names(water) <- "water"
wetland <- cec[[grep("class_14$", names(cec), value = TRUE)]]   
names(wetland) <- "wetland"
crop <- cec[[grep("class_15$", names(cec), value = TRUE)]]   
names(crop) <- "crop"

cec <- c(forest, urban, water, wetland, crop)


################################################
### Chelsa proj ################################
################################################

coll <- "chelsa-clim-proj"

ids <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

table(sapply(strsplit(ids, "_"), "[", 2))
lapply(strsplit(ids, "_"), "[", 3:4) |>
  do.call("rbind", args = _) |>
  as.data.table() |>
  setnames(c("model", "ssp")) |>
  _[ , (n = .N), by = .(model, ssp)]
invisible(lapply(2:4, function(i){
  dput(unique(sapply(strsplit(ids, "_"), "[", i)))
}))

timeperiod <- c("2071-2100", "2041-2070", "2011-2040")[2]
model <- c("ukesm1-0-ll", "mri-esm2-0", "mpi-esm1-2-hr", "ipsl-cm6a-lr", "gfdl-esm4")[4]
ssp <- c("ssp585", "ssp370", "ssp126")[3]

variables <- expand.grid(timeperiod = timeperiod, model = model, ssp = ssp) |>
      apply(1, function(i){paste(i, collapse = "_")})
w <- which(sub("^[^_]*_", "", ids) %in% variables)      

      
proj <- lapply(w, function(i){
  url <- io |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features[[i]]$assets[[1]]$href
  print(url)
  paste0('/vsicurl/', url) |>
    rast() |>
    project(p)
}) |> rast()

names(proj) <- chelsa_vars$longname[match(sub("_.*$", "", ids[w]), chelsa_vars$name)]




################################################
### FULL #######################################
################################################

predictors <- c(chelsa, geom, topo, soils, cec)

predictors_proj <- c(proj, geom, topo, soils, cec)

#plot(predictors)

# predictors <- rast("predictors.tif")
# predictors_proj <- rast("predictors_proj.tif")

################################################
### Fill missing values ########################
################################################

### Nearest neighbour filling (memory limited...)
raster_nafill <- function(p){
  xy <- xyFromCell(p, 1:ncell(p))
  xy <- cbind(xy, values(p))
  xynotna <- xy |> as.data.table() |> na.omit() |> as.matrix()
  nn <- knnx.index(xynotna[, 1:2], xy[, 1:2], k = 1)
  rm(xy); gc(); gc()
  pfill <- setValues(p, xynotna[nn, -(1:2)])
  rm(xynotna, nn)
  gc(); gc()
  pfill
}

predictors <- raster_nafill(predictors)
predictors_proj <- raster_nafill(predictors_proj)


writeRaster(predictors, "predictors.tif", overwrite = TRUE)
writeRaster(predictors_proj, "predictors_proj.tif", overwrite = TRUE)

#####################################
#####################################
#####################################
#####################################
#####################################
#####################################


it_obj <- io |>
  collections("chelsa-clim") |> items() |>
  get_request() |> items_fetch()
it_obj

ids <- io |>
  stac_search(collections = "chelsa-clim") |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

url <- io |>
  stac_search(collections = "chelsa-clim") |>
  post_request() |> 
  items_fetch() |>
  _$features[[which(ids == "bio1")]]$assets[[1]]$href

soil <- read_stars(paste0('/vsicurl/', url), proxy = TRUE) |>
  rast() |>
  project(p[[1]])

names(soil) <- "sand"

p <- c(p, soil)



for (i in 1:length(it_obj$features)){
  it_obj$features[[i]]$assets$data$roles='data'
}

st <- stac_image_collection(it_obj$features, asset_names=c('bio9'), property_filter = function(f){f[['class']] %in% c('1','2','3','4')},srs='EPSG:4326')

st <- stac_image_collection(it_obj$features)

bbox <- st_bbox(c(xmin = -483695, xmax = -84643, ymin = 112704 , ymax = 684311), crs = st_crs(32198))
v <- cube_view(srs = "EPSG:32198", extent = list(t0 = "2000-01-01", t1 = "2000-01-01", left = bbox$xmin, right =bbox$xmax, top = bbox$ymax, bottom = bbox$ymin), dx=1000, dy=1000, dt="P1D", aggregation = "sum", resampling = "mean")
lc_cube <- raster_cube(st, v)
lc_cube |> write_tif('~/',prefix='lc2',creation_options=list('COMPRESS'='DEFLATE'))
lc_cube |> plot(zlim=c(0,100),col=pal(10))




it_obj <- s_obj |>
  stac_search(collections = "chelsa-monthly", datetime="2010-06-01T00:00:00Z/2019-08-01T00:00:00Z") |> get_request() |> items_fetch()

v <- cube_view(srs = "EPSG:32198", extent = list(t0 = "2010-06-01", t1 = "2019-08-31",
                                                 left = bbox$xmin, right =bbox$xmax, top = bbox$ymax, bottom = bbox$ymin),
               dx=1000, dy=1000, dt="P10Y",
               aggregation = "mean",
               resampling = "bilinear")

for (i in 1:length(it_obj$features)){
  names(it_obj$features[[i]]$assets)='data'
  it_obj$features[[i]]$assets$data$roles='data'
}
anames=unlist(lapply(it_obj$features,function(f){f['id']}))
st <- stac_image_collection(it_obj$features, asset_names = 'data',  property_filter = function(f){f[['variable']] == 'tas' & (f[['month']] %in% c(6,7,8)) })
c_cube <- raster_cube(st, v)
c_cube |> plot(col=heat.colors)

c_cube |> write_tif('~/',prefix='chelsa-monthly',creation_options=list('COMPRESS'='DEFLATE'))









##########################################################
### CEC Landcover ########################################

source("scripts/sdm_utils.R")
source("scripts/sdm_inputs.R")
source("scripts/sdm_prelim.R")


if(TRUE){
  
  #cec <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/CEC_land_cover/NA_NALCMS_landcover_2020_30m.tif")
  
  cec <- rast("data/NA_NALCMS_landcover_2020_30m.tif")
  cec <- terra::crop(cec, st_transform(region, st_crs(cec)))
  
  r <- rast(resolution = 300, ext = terra::ext(region), crs = crs(region))
  
  
  cec_classes <- c("conifers", "taiga", "evergreen", "tropical", "deciduous", "mixed", "tropical_shrubland", "temperate_shrubland", "tropical_grassland", "temperate_grassland", "polar_shrubland", "polar_grassland", "polar_barren", "wetland", "cropland", "barren", "urban", "water", "snow")
  
  chosen <- cec_classes[c(1:2, 5:6, 8, 10:19)]
  #chosen <- cec_classes[c(1, 2, 8, 10:19)]
  
  p <- lapply(chosen, function(i){
    subst(cec, match(i, cec_classes), 1, 0) |>
      project(r)
  })
  p <- rast(p)
  names(p) <- chosen
  
  writeRaster(p, "data/cec300.tif", overwrite = TRUE, filetype = "COG")
  
  p <- rast("data/cec300.tif")
  
  
  ##########################################################
  ### Distance au Fleuve St-Laurent ########################
  
  ### tunr CEC into single layer water raster %
  system(sprintf(
    'cd %s
    gdal_translate -projwin %s NA_NALCMS_landcover_2020_30m.tif cropped.tif
    #cats=$cats
    gdal_calc.py -A cropped.tif --outfile=cat_cropped.tif --calc="%s" --format=GTiff --overwrite
    rm cropped.tif
    gdal_translate -ot Float32 cat_cropped.tif num_cropped.tif
    rm cat_cropped.tif
    gdalwarp -tr %s --config -wm 8000 -wo 8 -r average -overwrite num_cropped.tif output.tif
    rm num_cropped.tif
    # https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF
    # compress to COG
    gdal_translate output.tif %s.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW
    rm output.tif
   ', 
    "data", 
    "1750000 650000 2250000 250000", 
    "(A==18)", 
    "30 30", 
    "fsl"
  ))
  
  #fsl <- rast("/home/frousseu/Downloads/utilisation_territoire_2020/utilisation_territoire_2020/fsl.tif")
  fsl <- rast("data/fsl.tif")
  fsl <- aggregate(fsl, 2)
  fsl <- ifel(fsl >= 0.75, 1, NA)
  plot(fsl)
  
  pols <- as.polygons(fsl, values = FALSE) |> st_as_sf() |> ms_explode()
  plot(st_geometry(pols), col = "cyan", lwd = 0.1)
  o <- rev(order(st_area(pols)))[1:3000]
  sl <- pols[o, ]
  sl <- st_buffer(sl, 150) |> st_union() |> st_buffer(-150) |> st_as_sf() |> ms_explode()
  sl <- sl[rev(order(st_area(sl)))[1], ]
  
  p2 <- aggregate(p, 5)
  coo <- xyFromCell(p2, 1:ncell(p2)) |>
    as.data.frame() |>
    st_as_sf(coords = c("x", "y"), crs = st_crs(p)) |>
    st_transform(st_crs(sl))
  
  dis <- st_distance(coo, ms_simplify(sl, 0.01)) |> as.matrix()
  coo$distfl <- dis[, 1]
  coo <- st_transform(coo, st_crs(p))
  
  distfl <- setValues(p2[[1]], coo$distfl)
  dfl <- project(distfl, p[[1]])
  th <- 10
  dfl[dfl < th] <- th
  dfl <- subst(dfl, NA, th)
  names(dfl) <- "distfsl"
  
  #plot(crop(log(dfl), st_transform(st_buffer(sl, 20), crs = st_crs(p))))
  #plot(st_geometry(st_transform(sl, crs = st_crs(p2))), add = TRUE)
  #test <- c(p, log(dfl))
  #plot(mask(test, st_transform(region, st_crs(p)))[[3]])
  
  p <- c(p, log(dfl))
  
  
  ##########################################################
  ### WorldClim ############################################
  
  wc <- worldclim_country("CAN", path = "data", var = "bio")
  wc <- wc[[c(1, 4, 5, 7, 10, 12)]]
  wc <- project(wc, p[[1]])
  names(wc) <- c("tmean", "tseason", "tmax", "trange", "twarm", "prec")
  wc <- wc[[c("tmean", "tmax", "prec")]]
  p <- c(p, wc)
  
  
  
  ##########################################################
  ### Topography ###########################################
  
  flat <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/earthenv/topography_derived/geomflat_1KMperc_GMTEDmd.tif")
  flat <- terra::crop(flat, st_transform(region, st_crs(flat)))
  
  flat <- project(flat, p[[1]])
  names(flat) <- "geomflat"
  
  p <- c(p, flat)
  
  ##########################################################
  ### Elevation ############################################
  
  el <- elevation_30s("CAN", "data", mask=FALSE)
  el <- terra::crop(el, st_transform(region, st_crs(el)))
  el <- project(el, p[[1]])
  names(el) <- "elevation"
  
  p <- c(p, el)
  
  ##########################################################
  ### Distance to road #####################################
  
  #roads <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/distance_to_roads/GRIP_Roads_Dist_300.tif")
  
  io <- stac("https://io.biodiversite-quebec.ca/stac/")
  
  url <- io |>
    stac_search(collections = "distance_to_roads") |>
    post_request() |> 
    items_fetch() |>
    _$features[[1]]$assets$data$href
  
  roads <-read_stars(paste0('/vsicurl/', url), proxy = TRUE) |>
    rast() |>
    project(p[[1]])
  
  names(roads) <- "distroads"
  
  #roads <- terra::crop(roads, st_transform(region, st_crs(roads)))
  
  mind <- values(roads)[,1]       
  mind[mind == 0] <- NA
  mind <- min(mind, na.rm = TRUE)
  
  p <- c(p, log(roads+mind)) # add minimum non-zero value to avoid log(0)
  
  
  
  
  ##########################################################
  ### Soils ################################################
  
  #soil <- rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/soilgrids/silt_0-5cm.tif")
  
  ids <- io |>
    stac_search(collections = "soilgrids") |>
    post_request() |> 
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i){i$id})
  
  url <- io |>
    stac_search(collections = "soilgrids") |>
    post_request() |> 
    items_fetch() |>
    _$features[[which(ids == "sand_0-5cm")]]$assets[[1]]$href
  
  soil <-read_stars(paste0('/vsicurl/', url), proxy = TRUE) |>
    rast() |>
    project(p[[1]])
  
  names(soil) <- "sand"
  
  p <- c(p, soil)
  
  ##########################################################
  ### lat/lon ##############################################
  
  y <- init(p[[1]], "y")/100000
  names(y) <- "y"
  x <- init(p[[1]], "x")/100000
  names(x) <- "x"
  p <- c(p, x, y)
  
  
  
  ##########################################################
  ### Scale, fill, mask and write ##########################
  
  ### Nearest neighbour filling (memory limited...)
  xy <- xyFromCell(p, 1:ncell(p))
  xy <- cbind(xy, values(p))
  xynotna <- xy |> as.data.table() |> na.omit() |> as.matrix()
  nn <- knnx.index(xynotna[, 1:2], xy[, 1:2], k = 1)
  rm(xy);gc();gc()
  p2 <- setValues(p, xynotna[nn, -(1:2)])
  rm(xynotna, nn)
  gc();gc()
  
  p <- mask(p2, st_transform(region, st_crs(p)))
  
  writeRaster(p, "data/predictors_300.tif", overwrite = TRUE)
  
  
}

#options(terra.pal=colo.scale(1:200, c("grey95", "grey80","grey60","grey40","grey20", "lavenderblush", "brown", "grey5")))
p <- rast("data/predictors_300.tif")


#r <- ifel(!is.na(r), 1, NA)

# check if distfsl is well computed in relatin to water, nn filling and region. 

#r <- rast(matrix(1:25, ncol = 5))

#p <- crop(p, region, mask = TRUE)

#colSums(is.na(values(p)))

#plot(p$sand)
#plot(st_geometry(region), add = TRUE)
#plot(st_geometry(lakes), col = "cyan", add = TRUE)






library(gdalcubes)
library(rstac)

s_obj <- stac("https://io.biodiversite-quebec.ca/stac/")

collections <- s_obj |> collections() |> get_request()

library(knitr)
df<-data.frame(id=character(),title=character(),description=character())
for (c in collections[['collections']]){
  df<-rbind(df,data.frame(id=c$id,title=c$title,description=c$description))
}
kable(df)

it_obj <- s_obj |>
  stac_search(collections = "earthenv_landcover") |>
  post_request() |> items_fetch()
it_obj

it_obj <- s_obj |>
  collections("earthenv_landcover") |> items() |>
  get_request() |> items_fetch()
it_obj

it_obj[['features']][[1]]$properties

df<-data.frame(id=character(),datetime=character(), description=character())
for (f in it_obj[['features']]){
  df<-rbind(df,data.frame(id=f$id,datetime=f$properties$datetime,description=f$properties$description))
}
kable(df)

library(stars)
lc1<-read_stars(paste0('/vsicurl/',it_obj[['features']][[12]]$assets$data$href), proxy = TRUE)
plot(lc1)

bbox<-st_bbox(c(xmin = -76, xmax = -70, ymax = 54, ymin = 50), crs = st_crs(4326))
lc2 <- lc1 |> st_crop(bbox)

pal <- colorRampPalette(c("black","darkblue","red","yellow","white"))
plot(lc2,breaks=seq(0,100,10),col=pal(10))

write_stars(lc2,'~/lc3.tif',driver='COG',options=c('COMPRESS=DEFLATE'))

lc1 |> st_crop(bbox) |> write_stars('~/lc1.tif',driver='COG',RasterIO=c('resampling'='mode'),options=c('COMPRESS=DEFLATE','OVERVIEW_RESAMPLING=MODE','LEVEL=6','OVERVIEW_COUNT=8','RESAMPLING=MODE','WARP_RESAMPLING=MODE','OVERVIEWS=IGNORE_EXISTING'))



###########################
###########################
###########################

it_obj <- io |>
  collections("earthenv_landcover") |> items() |>
  get_request() |> items_fetch()
it_obj

for (i in 1:length(it_obj$features)){
  it_obj$features[[i]]$assets$data$roles='data'
}

st <- stac_image_collection(it_obj$features, asset_names = c('data'), property_filter = function(f){f[['class']] %in% c('1', '2', '3', '4')}, srs = 'EPSG:4326')
st

bbox <- st_bbox(c(xmin = -80, xmax = -58, ymin = 42 , ymax = 60), crs = st_crs(4326)) |> 
          st_as_sfc() |>
          st_as_sf() |>
          st_transform(6624) |>
          st_bbox()

v <- cube_view(srs = "EPSG:6624", extent = list(t0 = "2000-01-01", t1 = "2000-01-01", left = bbox$xmin, right =bbox$xmax, top = bbox$ymax, bottom = bbox$ymin), dx = 5000, dy = 5000, dt="P1D", aggregation = "sum", resampling = "mean")

v <- cube_view(srs = "EPSG:6624", extent = list(t0 = "2000-01-01", t1 = "2000-01-01", left = bbox$xmin, right =bbox$xmax, top = bbox$ymax, bottom = bbox$ymin), dx = 5000, dy = 5000, dt="P1D", resampling = "mean")

lc_cube <- raster_cube(st, v)

lc_cube |> write_tif(".", prefix = 'lc3', creation_options =list('COMPRESS' = 'DEFLATE'))

r <- rast("lc32000-01-01.tif")
plot(r)



###########################
###########################
###########################


it_obj <- io |>
  collections("chelsa-clim") |> items() |>
  get_request() |> items_fetch()
it_obj

for (i in 1:length(it_obj$features)){
  it_obj$features[[i]]$assets$data$roles='data'
}

it_obj$features[[i]]
it_obj$features[[i]]$properties

st <- stac_image_collection(it_obj$features, asset_names=c('bio1', 'bio2'), property_filter = function(f){f[['model']] %in% c('past')},srs='EPSG:4326')
st

bbox <- st_bbox(c(xmin = -85, xmax = -58, ymin = 40 , ymax = 63), crs = st_crs(4326)) |> 
  st_as_sfc() |>
  st_as_sf() |>
  st_transform(6624) |>
  st_bbox()

v <- cube_view(srs = "EPSG:6624", extent = list(t0 = "2000-01-01", t1 = "2000-01-01", left = bbox$xmin, right =bbox$xmax, top = bbox$ymax, bottom = bbox$ymin), dx=1000, dy=1000, dt="P1D", aggregation = "sum", resampling = "mean")

lc_cube <- raster_cube(st, v)

lc_cube |> write_tif(".", prefix = 'lc2', creation_options =list('COMPRESS' = 'DEFLATE'))

r <- rast("lc22000-01-01.tif")
plot(r)


###
it_obj[['features']]
lc1<-rast(paste0('/vsicurl/',it_obj[['features']][[1]]$assets$data$href), proxy = TRUE)


