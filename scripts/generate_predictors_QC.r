

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
qc <- can[can$NAME_1 %in% c("Québec"), ]
qc <- st_transform(qc, epsg)
region <- qc
st_write(region, file.path(tmpath, "QC.gpkg"), append = FALSE)



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
  ),
  "twi" = c(
    "twi", "twi" 
  ),
  "mhc" = c(
    "mhc", "mhc"
  ),
  "sigeom_zones_morphosedimentologiques_percentage" = c(
    "Alluvion_1", "alluvion",
    "Dépôt_2", "depot",
    "Éolien_3", "eolien",
    "Glaciaire_4", "glaciaire",
    "Anthropogénique_5", "anthropogenique",
    "Lacustre_6", "lacustre",
    "Glaciolacustre_7", "glaciolacustre",
    "Marin_8", "marin",
    "Glaciomarin_9", "glaciomarin",
    "Organique_10", "organique",
    "Quaternaire_11", "quaternaire",
    "Roche_12", "roche",
    "Till_13", "till"
  ),
  "mhp2023_percentage" = c(
    "Eau_peu_profonde_1", "eau_peu_profonde",
    "Marais_2", "marais",
    "Marécage_3", "marecage",
    "Milieu_humide_indifférencié_4", "indifferencie",
    "Prairie_humide_5", "prairie_humide",
    "Tourbière_boisée_6", "tourbiere_boisee",
    "Tourbière_ouverte_indifférenciée_7", "tourbiere_indifferenciee",
    "Tourbière_ouverte_minérotrophe_8", "tourbiere_minerotrophe",
    "Tourbière_ouverte_ombrotrophe_9", "tourbiere_ombrotrophe"
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
collections[["cec_land_cover_percentage"]]$name <- c("coniferous", "taiga", "tropical_evergreen", "tropical_deciduous", "deciduous", "mixed", "tropical_shrub", "temperate_shrub", "tropical_grass", "temperate_grass", "polar_shrub", "polar_grass", "lichen", "wetland", "cropland", "barren", "urban", "water", "snow")


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

timeperiod <- c("2071-2100", "2041-2070", "2011-2040")[2]
model <- c("ukesm1-0-ll", "mri-esm2-0", "mpi-esm1-2-hr", "ipsl-cm6a-lr", "gfdl-esm4")[4]
ssp <- c("ssp585", "ssp370", "ssp126")[3]

variables <- expand.grid(timeperiod = timeperiod, model = model, ssp = ssp) |>
      apply(1, function(i){paste(i, collapse = "_")})
ids <- ids[which(sub("^[^_]*_", "", ids) %in% variables)]      

#collections[["chelsa-clim-proj"]]$var <- ids
#collections[["chelsa-clim-proj"]]$name <- ids


variables <- data.frame(coll = rep(names(collections), times = sapply(collections, function(i){length(i$var)})), var = unlist(lapply(collections, function(i){i$var}), use.names = FALSE), name = unlist(lapply(collections, function(i){i$name}), use.names = FALSE))


urls <- lapply(seq_along(collections), function(i){
  get_urls(names(collections)[[i]], collections[[i]]$var, io)
})

variables$url <- unlist(urls, use.names = FALSE)
variables$url <- URLencode(variables$url)

variables <- variables[-grep("tropical", variables$name), ]

#variables <- variables[grep("alluvion|till", variables$name), ]
#variables <- variables[grep("depot", variables$name), ]
#variables <- variables[1:5, ]

cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
cmd <- sprintf('gdalwarp -overwrite -cutline %s/QC.gpkg -crop_to_cutline -dstnodata -9999.0 -r average -tr 100 100 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 /vsicurl/%s %s/%s.tif', tmpath, variables$url[i], tmpath, variables$name[i])
system(cmd)
py_cmd <- sprintf("from osgeo import gdal; ds = gdal.Open('%s/%s.tif', gdal.GA_Update); ds.GetRasterBand(1).SetDescription('%s'); ds = None", tmpath, variables$name[i], variables$name[i])
system2("python3", args = c("-c", shQuote(py_cmd)))
}
stopCluster(cl)


input_dir <- tmpath
vrt_file <- file.path(tmpath, "stacked.vrt")

# Escape any backslashes (Windows) and quotes
#input_dir <- normalizePath(input_dir, winslash = "/", mustWork = FALSE)

py_script <- sprintf("
import os
from osgeo import gdal

input_dir = r'%s'
vrt_filename = r'%s'

tif_files = sorted([os.path.join(input_dir, f) for f in os.listdir(input_dir) if f.endswith('.tif')])
band_names = [os.path.splitext(os.path.basename(f))[0] for f in tif_files]

gdal.BuildVRT(vrt_filename, tif_files, separate=True)

vrt_ds = gdal.Open(vrt_filename, gdal.GA_Update)
for i, name in enumerate(band_names):
    vrt_ds.GetRasterBand(i + 1).SetDescription(name)
vrt_ds = None
", input_dir, vrt_file)

system2("python3", args = c("-c", shQuote(py_script)))


cmd <- sprintf('
  #gdalbuildvrt -separate %s/stacked.vrt %s/*.tif

  gdal_translate -of COG -r average -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_100_QC.tif', tmpath, tmpath, tmpath, tmpath)
system(cmd)




if(FALSE){

    r <- rast("/home/frousseu/data2/tmp/vrm.tif")
    global(r, "range", na.rm = TRUE)

    r <- rast("/home/frousseu/data/sdm_method_explorer/data/predictors_300.tif")
    global(r, "range", na.rm = TRUE)

    r <- rast("/home/frousseu/data2/tmp/predictors_100_NA.tif")
    r <- rast("/home/frousseu/data2/tmp/geomflat_perc.tif")

    grassDir <- "/usr/bin/grass"
    faster(grassDir = grassDir, cores=8)
    
    r <- rast("/home/frousseu/data2/qc/till.tif")
    names(r) <- "TESTING"
    writeRaster(r, "/home/frousseu/data2/qc/testing.tif")
    r <- rast("/home/frousseu/data2/qc2/predictors_100_QC.tif")

    r <- rast("/home/frousseu/data2/qc/stacked.vrt")


    r <- rast("/home/frousseu/data2/qc/predictors_100_QC.tif")
    r <- r$till
    rr <- aggregate(r, 50, na.rm = TRUE)

    r <- rast("/home/frousseu/data2/qc/till.tif")
    rr <- aggregate(r, 50, na.rm = TRUE)

    library(terra)

    r <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/predictors_100_QC.tif")
     
    plot(r$till)  

    r <- rast("/home/frousseu/data2/sigeom/coarse5.tif")



}