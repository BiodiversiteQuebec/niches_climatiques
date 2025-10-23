
### This script gathers predictors for modeling the distribution of emv species for the finance indicators

library(terra)
library(sf)
library(data.table)
library(rstac)
library(geodata)
library(doParallel)
library(foreach)

#library(terra)
#r <- rast("data/predictors.tif")
#r <- rast("data/predictors_100m_band1.tif")
#plot(r)

tmpath <- "/home/frousseu/data2/na"
setwd(dirname(tmpath))
epsg <- 6624

options(width = 150)
terraOptions(tempdir = tmpath, memfrac = 0.8)

get_ids <- function(coll, stac){
  stac |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i){i$id})
}

get_urls <- function(coll, ids, stac){
  gids <- get_ids(coll, stac)
  w <- which(gids %in% ids)
  w <- w[order(match(gids[w], ids))]
  items <- stac |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() 
  sapply(w, function(i){
    items$features[[i]]$assets[[1]]$href
  })
}

# Downloads polygons using package geodata
can <- gadm("CAN", level = 2, path = tmpath) |> st_as_sf()
usa <- gadm("USA", level = 2, path = tmpath) |> st_as_sf()
na <- rbind(can, usa)
na <- st_transform(na, epsg)
na <- na[!na$NAME_1 %in% c("Hawaii"), ]
na <- na[-grep("Aleutians", na$NAME_2), ]
na <- st_union(na)
region <- na
st_write(region, file.path(tmpath, "NA.gpkg"), append = FALSE)
x <- st_read(file.path(tmpath, "NA.gpkg"))


io <- stac("https://io.biodiversite-quebec.ca/stac/")
coll <- "chelsa-clim"

###############################################
### add simple collections ####################

collections <- list(
  "earthenv_topography_derived" = c(
    "geomflat_perc", "geomflat", 
    "geomfootslope_per", "geomfootslope"
  ),
  "earthenv_topography" = c(
    "elevation", "elevation",
    "vrm", "ruggedness"
  ),
  "soilgrids" = c(
    "sand_0-5cm", "sand",
    "clay_0-5cm", "clay",
    "silt_0-5cm", "silt",
    "phh2o_0-5cm", "ph",
    "nitrogen_0-5cm", "nitrogen",
    "bdod_0-5cm", "bulk_density",
    "soc_0-5cm", "soil_organic_carbon",
    "ocd_0-5cm", "organic_carbon_density"
  ),
  "distance_to_roads" = c(
    "distance_to_roads", "distance_to_roads"
  ),
  "silvis" = c(
   "NDVI16_cumulative", "ndvi",
   "LAI8_cumulative", "lai"
  ),
  "ghmts" = c(
    "GHMTS", "human_modification"
  )
)

collections <- lapply(collections, function(i){
  list(
    var = i[seq(1, length(i), by = 2)],
    name = i[seq(2, length(i), by = 2)]   
  )
})




###############################################
### add collections with automated names ######

collections[["chelsa-clim"]]$var <- paste0("bio", 1:19)
collections[["chelsa-clim"]]$name <- gsub(" ", "_", c(
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
  ))

collections[["cec_land_cover_percentage"]]$var <- paste0("cec_land_cover_percent_class_", 1:19)
collections[["cec_land_cover_percentage"]]$name <- c("coniferous", "taiga", "tropical_evergreen", "tropical_deciduous", "temperate_deciduous", "mixed", "tropical_shrub", "temperate_shrub", "tropical_grass", "temperate_grass", "polar_shrub", "polar_grass", "polar_lichen", "wetland", "cropland", "barren", "urban", "water", "snow")


###############################################
### add collections with more complex names ###
coll <- "chelsa-clim-proj"

ids <- get_ids(coll, io)

table(sapply(strsplit(ids, "_"), "[", 2))
lapply(strsplit(ids, "_"), "[", 3:4) |>
  do.call("rbind", args = _) |>
  as.data.table() |>
  setnames(c("model", "ssp")) |>
  _[ , (n = .N), by = .(model, ssp)]
invisible(lapply(2:4, function(i){
  dput(unique(sapply(strsplit(ids, "_"), "[", i)))
}))

timeperiod <- c("2071-2100", "2041-2070", "2011-2040")[1:3]
model <- c("ukesm1-0-ll", "mri-esm2-0", "mpi-esm1-2-hr", "ipsl-cm6a-lr", "gfdl-esm4")[5]
ssp <- c("ssp585", "ssp370", "ssp126")[2]

variables <- expand.grid(timeperiod = timeperiod, model = model, ssp = ssp) |>
      apply(1, function(i){paste(i, collapse = "_")})
ids <- ids[which(sub("^[^_]*_", "", ids) %in% variables)]      

#chelsavars <- variables[variables$coll == "chelsa-clim", ]
idsname <- ids
for(i in seq_along(collections[["chelsa-clim"]]$var)){
  idsname <- gsub(collections[["chelsa-clim"]]$var[i], collections[["chelsa-clim"]]$name[i], idsname)
} 

collections[["chelsa-clim-proj"]]$var <- ids
collections[["chelsa-clim-proj"]]$name <- idsname



variables <- data.frame(coll = rep(names(collections), times = sapply(collections, function(i){length(i$var)})), var = unlist(lapply(collections, function(i){i$var}), use.names = FALSE), name = unlist(lapply(collections, function(i){i$name}), use.names = FALSE))


urls <- lapply(seq_along(collections), function(i){
  get_urls(names(collections)[[i]], collections[[i]]$var, io)
})

variables$url <- unlist(urls, use.names = FALSE)
variables$url <- URLencode(variables$url)

#variables <- variables[c(1, 5, 14, 17, 55), ]

if(TRUE){
  desc <- variables
  names(desc) <- c("collection", "var", "variable", "url")
  desc <- desc[, c("collection", "variable", "url")]
  desc$url <- file.path("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na", paste0(desc$variable, ".tif"))
  write.csv(desc, file.path(tmpath, "description.csv"), row.names = FALSE)
  #system(sprintf("s5cmd --numworkers 8 cp -acl public-read --sp '%s/*.csv' s3://bq-io/sdm_predictors/na/", tmpath))
  #x <- read.csv("https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/description.csv")
}

cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
cmd <- sprintf('gdalwarp -overwrite -cutline %s/NA.gpkg -crop_to_cutline -dstnodata -9999.0 -r average -tr 100 100 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 /vsicurl/%s %s/%s.tif', tmpath, variables$url[i], tmpath, variables$name[i])
system(cmd)
#system(sprintf('cp %s/%s.tif %s/%s_original.tif', tmpath, variables$name[i], tmpath, variables$name[i])) # keep snapshot of original for precise masking
py_cmd <- sprintf("from osgeo import gdal; gdal.UseExceptions(); ds = gdal.Open('%s/%s.tif', gdal.GA_Update); ds.GetRasterBand(1).SetDescription('%s'); ds = None", tmpath, variables$name[i], variables$name[i])
system2("/usr/bin/python3", args = c("-c", shQuote(py_cmd)))
}
stopCluster(cl)


##############################################################
### Fill NAs by interpolation ################################

# fill by interpolation
rfill <- data.frame(var = variables$name[variables$coll %in% c("soilgrids", "ghmts")], fill = "inv_dist")
# fill with 0
rfill0 <- data.frame(var = variables$name[variables$coll %in% c("silvis")], fill = 0)

rfill <- rbind(rfill, rfill0)

# make mask
cmd <- sprintf('gdal_calc.py -A %s/mean_annual_air_temperature.tif --outfile=%s/mask.tif --calc="1*(A!=-9999)" --NoDataValue=none --type=Byte --co="COMPRESS=DEFLATE" --overwrite', tmpath, tmpath)
system(cmd)


cl <- makeCluster(min(c(nrow(rfill), 7)))
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(rfill)) %dopar% {

  if(rfill$fill[i] == "inv_dist"){
    cmd <- sprintf('gdal_fillnodata.py -md 850 -si 0 -co COMPRESS=DEFLATE -co BIGTIFF=YES -co NUM_THREADS=ALL_CPUS %s/%s.tif %s/%s_filled.tif', tmpath, rfill$var[i], tmpath, rfill$var[i])
    system(cmd)
  }else{
    cmd <- sprintf('gdal_calc.py -A %s/%s.tif --outfile=%s/%s_filled.tif --calc="A*(A!=-9999)" --NoDataValue=none --co="COMPRESS=DEFLATE" --co="BIGTIFF=YES" --co="NUM_THREADS=ALL_CPUS" --overwrite', tmpath, rfill$var[i], tmpath, rfill$var[i])
    system(cmd)
  }
  cmd <- sprintf('gdal_calc.py -A %s/%s_filled.tif -B %s/mask.tif --outfile=%s/%s_masked.tif --calc="A*(B!=0) + (-9999)*(B==0)" --NoDataValue=-9999 --co="COMPRESS=DEFLATE" --co="BIGTIFF=YES" --co="NUM_THREADS=ALL_CPUS" --overwrite', tmpath, rfill$var[i], tmpath, tmpath, rfill$var[i])
  system(cmd)
  cmd <- sprintf('rm %s/%s_filled.tif

  mv %s/%s_masked.tif %s/%s.tif
  ', tmpath, rfill$var[i], tmpath, rfill$var[i], tmpath, rfill$var[i])
  system(cmd)
  py_cmd <- sprintf("from osgeo import gdal;  gdal.UseExceptions(); ds = gdal.Open('%s/%s.tif', gdal.GA_Update); ds.GetRasterBand(1).SetDescription('%s'); ds = None", tmpath, rfill$var[i], rfill$var[i])
  system2("/usr/bin/python3", args = c("-c", shQuote(py_cmd)))

}
stopCluster(cl)

cmd <- sprintf('rm %s/mask.tif', tmpath)
system(cmd)


##########################################################
### turn every single tif to a cog #######################

cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
cmd <- sprintf('gdal_translate -of COG -r average -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/%s.tif %s/%s_cog.tif', tmpath, variables$name[i], tmpath, variables$name[i])
system(cmd)
cmd <- sprintf('rm %s/%s.tif

mv %s/%s_cog.tif %s/%s.tif
', tmpath, variables$name[i], tmpath, variables$name[i], tmpath, variables$name[i])
system(cmd)
#py_cmd <- sprintf("from osgeo import gdal; gdal.UseExceptions(); ds = gdal.Open('%s/%s.tif', gdal.GA_Update); ds.GetRasterBand(1).SetDescription('%s'); ds = None", tmpath, variables$name[i], variables$name[i])
#system2("/usr/bin/python3", args = c("-c", shQuote(py_cmd)))
}
stopCluster(cl)

quit(save = "no")


##############################################################
### little add-on to produce low res predictors ############## 

cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
cmd <- sprintf('gdal_translate -of COG -r average -tr 1000 1000 -co COMPRESS=DEFLATE %s/%s.tif %s/%s_lowres.tif', tmpath, variables$name[i], tmpath, variables$name[i])
system(cmd)
}

input_dir <- tmpath
vrt_file <- file.path(tmpath, "stacked.vrt")


py_script <- sprintf("
import os
from osgeo import gdal

input_dir = r'%s'
vrt_filename = r'%s'

tif_files = sorted([os.path.join(input_dir, f) for f in os.listdir(input_dir) if f.endswith('_lowres.tif')])
#tif_files = tif_files[:10]
band_names = [os.path.splitext(os.path.basename(f))[0] for f in tif_files]
band_names = [f.replace('_lowres', '') for f in band_names]

gdal.BuildVRT(vrt_filename, tif_files, separate=True)

vrt_ds = gdal.Open(vrt_filename, gdal.GA_Update)
for i, name in enumerate(band_names):
    vrt_ds.GetRasterBand(i + 1).SetDescription(name)
vrt_ds = None
", input_dir, vrt_file)

system2("/usr/bin/python3", args = c("-c", shQuote(py_script)))


cmd <- sprintf('bash -c "

  source /home/frousseu/miniconda3/etc/profile.d/conda.sh 

  conda activate gdal-env
  
  gdalinfo --version

  gdal_translate -of COG -r average -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_1000_NA.tif"', tmpath, tmpath, tmpath, tmpath)
system(cmd)




##########################################################################
### the stacking of all files does not seem to work or end so putting this on ice for now...

if(FALSE){

input_dir <- tmpath
vrt_file <- file.path(tmpath, "stacked.vrt")


py_script <- sprintf("
import os
from osgeo import gdal

input_dir = r'%s'
vrt_filename = r'%s'

tif_files = sorted([os.path.join(input_dir, f) for f in os.listdir(input_dir) if f.endswith('.tif')])
#tif_files = tif_files[:10]
band_names = [os.path.splitext(os.path.basename(f))[0] for f in tif_files]

gdal.BuildVRT(vrt_filename, tif_files, separate=True)

vrt_ds = gdal.Open(vrt_filename, gdal.GA_Update)
for i, name in enumerate(band_names):
    vrt_ds.GetRasterBand(i + 1).SetDescription(name)
vrt_ds = None
", input_dir, vrt_file)

system2("/usr/bin/python3", args = c("-c", shQuote(py_script)))


cmd <- sprintf('bash -c "

  source /home/frousseu/miniconda3/etc/profile.d/conda.sh 

  conda activate gdal-env
  
  gdalinfo --version

  gdal_translate -of COG -r average -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_100_NA.tif"', tmpath, tmpath, tmpath, tmpath)
system(cmd)



#r <- rast("/home/frousseu/data2/na/sand.tif")
#plot(aggregate(r, 2, na.rm = TRUE))

# x <- st_read(file.path(tmpath, "invalid.gpkg")); plot(st_geometry(x))


cmd <- 'gdalwarp -overwrite -r average -tr 100 100 -co COMPRESS=DEFLATE -wo NUM_THREADS=ALL_CPUS /home/frousseu/data2/qc/mhc.tif /home/frousseu/data2/qc/mhc_agg.tif'
system(cmd)
#system('gdal_edit.py -a_nodata 9999 /home/frousseu/data2/qc/mhc_agg.tif')
cmd <- 'gdalwarp -overwrite -r average -tr 100 100 -co COMPRESS=DEFLATE -wo NUM_THREADS=ALL_CPUS /home/frousseu/data2/qc/mean_annual_air_temperature.tif /home/frousseu/data2/qc/mean_annual_air_temperature_agg.tif'
system(cmd)
#system('gdal_edit.py -a_nodata 9999 /home/frousseu/data2/qc/mean_annual_air_temperature_agg.tif')

cmd <- 'gdal_calc.py -A /home/frousseu/data2/qc/mean_annual_air_temperature_agg.tif --outfile=/home/frousseu/data2/qc/mask.tif --calc="1*(A!=-9999)" --NoDataValue=none --type=Byte --co="COMPRESS=DEFLATE" --overwrite'
system(cmd)

#system('gdal_calc.py -A /home/frousseu/data2/qc/mhc_agg.tif --outfile=/home/frousseu/data2/qc/mhc_agg0.tif --calc="A*(A!=-9999) + 400*(A==-9999)" --NoDataValue=None')

cmd <- 'gdal_fillnodata.py -md 250 -si 3 -co COMPRESS=DEFLATE /home/frousseu/data2/qc/mhc_agg.tif /home/frousseu/data2/qc/mhc_filled.tif'
system(cmd)
system('gdal_calc.py -A /home/frousseu/data2/qc/mhc_filled.tif -B /home/frousseu/data2/qc/mask.tif --outfile=/home/frousseu/data2/qc/mhc_masked.tif --calc="A*(B!=0) + (-9999)*(B==0)" --NoDataValue=-9999 --overwrite')
#system('gdal_edit.py -a_nodata 9999 /home/frousseu/data2/qc/mhc_filled.tif')

#r <- rast("/home/frousseu/data2/qc/mhc.tif");plot(r)
#r <- rast("/home/frousseu/data2/qc/mhc_agg.tif");plot(r)
#r <- rast("/home/frousseu/data2/qc/mask.tif");plot(r)
#r <- rast("/home/frousseu/data2/qc/sand_filled.tif");plot(r)
#r <- rast("/home/frousseu/data2/qc/sand_masked.tif");plot(r)








r <- rast("/home/frousseu/data2/qc/sand_agg.tif")
r <- rast("/home/frousseu/data2/qc/mean_annual_air_temperature_agg.tif")
r <- rast("/home/frousseu/data2/qc/mask.tif")
r <- rast("/home/frousseu/data2/qc/sand_filled.tif")
r <- rast("/home/frousseu/data2/na/sand_close.tif")
r <- aggregate(r, 2, na.rm = TRUE)

r <- rast("/home/frousseu/data2/tmp/vrm.tif")
global(r, "range", na.rm = TRUE)

r <- rast("/home/frousseu/data/sdm_method_explorer/data/predictors_300.tif")
global(r, "range", na.rm = TRUE)

r <- rast("/home/frousseu/data2/tmp/predictors_100_NA.tif")
r <- rast("/home/frousseu/data2/tmp/geomflat_perc.tif")


get_ids <- function(coll, stac){
  stac |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i){i$id})
}

ids <- get_ids(coll, io)
ids
variables <- c("bio1", "bio2")


get_urls <- function(coll, ids, stac){
  w <- which(ids %in% get_ids(coll, stac))
  sapply(w, function(i){
    stac |>
      stac_search(collections = coll) |>
      post_request() |> 
      items_fetch() |>
      _$features[[i]]$assets[[1]]$href
  })
}
urls <- get_urls(coll, variables, io)







# Downloads polygons using package geodata
can <- gadm("CAN", level = 2, path = tmpath) |> st_as_sf()
usa <- gadm("USA", level = 2, path = tmpath) |> st_as_sf()
na <- rbind(can, usa)
na <- st_transform(na, epsg)
na <- na[!na$NAME_1 %in% c("Hawaii"), ]
na <- na[-grep("Aleutians", na$NAME_2), ]
#na <- na[!na$NAME_1 %in% c("Hawaii", "Alaska"), ]

# keep Québec and bordering provinces/states as a buffer
#region <- na[na$NAME_1 %in% c("Québec", "New Brunswick", "Maine", "Vermont", "New Hampshire", "New York", "Ontario", "Nova Scotia", "Prince Edward Island", "Massachusetts", "Connecticut", "Rhode Island"),]
region <- na
#region <- na[!na$NAME_1 %in% c("Yukon", "British Columbia", "Washington", "Oregon", "California", "Arizona", "Nevada", "Idaho", "Utah"), ]

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
ex <- c(xmin = -4828412.27173732, xmax = 1182937.28491704, ymin = -1949713.76131365, ymax = 4123691.94410149)
p <- rast(ext(region), resolution = 100, crs = crs(paste0("EPSG:", epsg)))

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
    project(p, tempdir = tmpath)
}) |> rast()

names(chelsa) <- chelsa_vars$longname[match(ids[w], chelsa_vars$name)]

#rr <- aggregate(chelsa, 10, na.rm = TRUE)
#screeplot(rda(values(rr), scale = TRUE))
#pca <- rda(values(rr), scale = TRUE)
#barplot(cumsum(eigenvals(pca)/sum(eigenvals(pca))))

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
    project(p, tempdir = tmpath)
}) |> rast()

names(geom) <- ids[w]

geom <- mask(geom, vect(region))

writeRaster(geom, file.path(tmpath, "geom.tif"), gdal = c("COMPRESS=DEFLATE"))

st_write(region, file.path(tmpath, "NA.gpkg"))

#gdalwarp -overwrite -cutline NA.gpkg -crop_to_cutline -dstnodata 32767 -tr 100 100 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 '/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/CHELSA/climatologies/CHELSA_bio9_1981-2010_V.2.1.tif' chelsa.tif

r <- rast(file.path(tmpath, "chelsa.tif"))
plot(r)

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
### Milieux humides 2003 ######################
################################################

coll <- "mhp2023"

ids <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features |>
  sapply(X = _, function(i){i$id})

url <- io |>
  stac_search(collections = coll) |>
  post_request() |> 
  items_fetch() |>
  _$features[[1]]$assets[[1]]$href

mhp <- rast(paste0("/vsicurl/", url))
mhp <- rast(url)

#r <- ifel(mhp == 1, 1, 0)

for i in {1..9}; do
  gdal_calc.py -A input.tif --outfile=class_${i}_mask.tif --calc="(A==${i})" --NoDataValue=0 --overwrite
done

gdal_calc.py -A input.tif --outfile=class_1_mask.tif --calc="(A==${i})" --NoDataValue=0 --type=Byte --overwrite

for i in {1..2}; do
  gdal_calc.py -A input.tif --outfile=tmp_class_${i}.tif --calc="(A==${i})" --NoDataValue=0 --type=Byte --quiet
  gdalwarp -tr 30 30 -te xmin ymin xmax ymax -r average tmp_class_${i}.tif class_${i}_pct.tif
  rm tmp_class_${i}.tif
done


library(terra)
lf <- list.files("data", pattern = "class_", full = TRUE)
r <- lapply(lf, rast) |>
  rast()

r <- rast("data/mh2023.tif")



r <- rast("tmp/CEC_land_cover_percent_class1_2020-01-01.tif")
plot(r)




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

library(terra)
library(data.table)
library(FNN)
r <- rast("/home/frousseu/data2/na/sand.tif")
r <- aggregate(r, 5, na.rm = TRUE)
p <- raster_nafill(r)

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






}


##########################################################
### CEC Landcover ########################################

source("scripts/sdm_utils.R")
source("scripts/sdm_inputs.R")
source("scripts/sdm_prelim.R")


if(FALSE){
  
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


####################
####################
if(FALSE){
  twi <- lapply(c("21E05NO", "21E05SO"),
  #twi <- lapply(c("31H08NO", "31H08NE", "31H08SO", "31H08SE"), 
  function(i){
    r <- rast(file.path("/vsicurl/https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/IMAGERIE/Produits_derives_LiDAR/Hydrographie/Indice_humidite_topographique/3-Donnees", substr(i, 1, 3), i, paste0("TWI_", i, ".tif")))
  }) 
  twi <- merge(twi[[1]], twi[[2]], twi[[3]], twi[[4]])
  twi2 <- aggregate(twi, 4)

  mhc <- lapply(c("21E05NO", "21E05SO"),
  #mhc <- lapply(c("31H08NO", "31H08NE", "31H08SO", "31H08SE"), 
  function(i){
    r <- rast(file.path("/vsicurl/https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/IMAGERIE/Produits_derives_LiDAR", substr(i, 1, 3), i, paste0("MHC_", i, ".tif")))
  }) 
  mhc <- merge(mhc[[1]], mhc[[2]], mhc[[3]], mhc[[4]])
  mhc2 <- aggregate(mhc, 4)


  r <- rast(resolution = 5, ext = ext(qc), crs = crs(qc))
  r <- crop(r, c(-265700, -263500, 157000, 160000))
  r <- project(twi[[1]], r, threads = TRUE)
  plot(trim(r))
}



library(terra)
r <- rast("mhp2023_8_Tourbière ouverte minérotrophe.tif")
r <- aggregate(r, 50)
plot(r)
