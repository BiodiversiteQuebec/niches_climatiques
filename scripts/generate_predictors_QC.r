

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

tmpath <- "/home/frousseu/data2/qc"
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
can <- gadm("CAN", level = 2, path = "/home/frousseu/data2/na") |> st_as_sf()
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
#variables <- variables[grep("twi|mhc|ndvi|mean_annual_air_temperature", variables$name), ]
#variables <- variables[grep("twi|mhc", variables$name), ]
#variables <- variables[grep("ndvi", variables$name), ]

#variables <- variables[grep("depot", variables$name), ]
#variables <- variables[1:5, ]

if(FALSE){
  desc <- variables
  names(desc) <- c("collection", "var", "variable", "url")
  desc <- desc[, c("collection", "variable")]
  #desc$url <- file.path("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc", paste0(desc$variable, ".tif"))
  write.csv(desc, file.path(tmpath, "description.csv"), row.names = FALSE)
  system(sprintf("s5cmd --numworkers 8 cp -acl public-read --sp '%s/*.csv' s3://bq-io/sdm_predictors/qc/", tmpath))
  x <- read.csv("https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/description.csv")
}




cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
cmd <- sprintf('gdalwarp -overwrite -cutline %s/QC.gpkg -crop_to_cutline -dstnodata -9999.0 -r average -tr 100 100 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 /vsicurl/%s %s/%s.tif', tmpath, variables$url[i], tmpath, variables$name[i])
system(cmd)
system(sprintf('cp %s/%s.tif %s/%s_original.tif', tmpath, variables$name[i], tmpath, variables$name[i])) # keep snapshot of original for precise masking
py_cmd <- sprintf("from osgeo import gdal; gdal.UseExceptions(); ds = gdal.Open('%s/%s.tif', gdal.GA_Update); ds.GetRasterBand(1).SetDescription('%s'); ds = None", tmpath, variables$name[i], variables$name[i])
system2("/usr/bin/python3", args = c("-c", shQuote(py_cmd)))
}
stopCluster(cl)



### Fill NAs by interpolation ################################

# fill by interpolation
rfill <- data.frame(var = variables$name[variables$coll %in% c("soilgrids", "mhc", "twi", "ghmts")], fill = "inv_dist")
# fill with 0
rfill0 <- data.frame(var = variables$name[variables$coll %in% c("silvis")], fill = 0)

rfill <- rbind(rfill, rfill0)
#rfill <- rfill[10:12, ]

# make mask
cmd <- sprintf('gdal_calc.py -A %s/mean_annual_air_temperature.tif --outfile=%s/mask.tif --calc="1*(A!=-9999)" --NoDataValue=none --type=Byte --co="COMPRESS=DEFLATE" --overwrite', tmpath, tmpath)
system(cmd)


cl <- makeCluster(min(c(nrow(rfill), 10)))
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(rfill)) %dopar% {

  if(rfill$fill[i] == "inv_dist"){
    cmd <- sprintf('gdal_fillnodata.py -md 500 -si 0 -co COMPRESS=DEFLATE %s/%s.tif %s/%s_filled.tif', tmpath, rfill$var[i], tmpath, rfill$var[i])
    system(cmd)
  }else{
    cmd <- sprintf('gdal_calc.py -A %s/%s.tif --outfile=%s/%s_filled.tif --calc="A*(A!=-9999)" --NoDataValue=none --co="COMPRESS=DEFLATE" --overwrite', tmpath, rfill$var[i], tmpath, rfill$var[i])
    system(cmd)
  }
  cmd <- sprintf('gdal_calc.py -A %s/%s_filled.tif -B %s/mask.tif --outfile=%s/%s_masked.tif --calc="A*(B!=0) + (-9999)*(B==0)" --NoDataValue=-9999 --co="COMPRESS=DEFLATE" --overwrite', tmpath, rfill$var[i], tmpath, tmpath, rfill$var[i])
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


########################################################
### Further mask incomplete coverage ###################

# mhc, twi don't have the same covergage, so needs to be specific

custom_mask <- function(v){
  r <- rast(file.path(tmpath, paste0(v,"_original.tif")))
  r[!is.na(r)] <- 1
  maskr <- r |> 
    as.polygons(aggregate = TRUE) |>
    st_as_sf() |>
    st_geometry() |>
    st_cast("POLYGON") |>
    lapply(function(i){
      st_multipolygon(list(i[1]))
    }) |>
    st_sfc(crs = st_crs(r)) |>
    st_as_sf() |>
    st_union()


  st_write(maskr, file.path(tmpath, "maskr.gpkg"), append = FALSE)

  cmd <- sprintf('gdalwarp -overwrite -cutline %s/maskr.gpkg -co COMPRESS=DEFLATE %s/%s.tif %s/%s_masked.tif', tmpath, tmpath, v, tmpath, v)
  system(cmd)

  cmd <- sprintf('rm %s/%s.tif

    mv %s/%s_masked.tif %s/%s.tif', 
    tmpath, v, tmpath, v, tmpath, v)
  system(cmd)

  py_cmd <- sprintf("from osgeo import gdal;  gdal.UseExceptions(); ds = gdal.Open('%s/%s.tif', gdal.GA_Update); ds.GetRasterBand(1).SetDescription('%s'); ds = None", tmpath, v, v)
  system2("/usr/bin/python3", args = c("-c", shQuote(py_cmd)))

}

custom_mask("mhc")
custom_mask("twi")

system(sprintf('rm %s/maskr.gpkg', tmpath))
system(sprintf('rm %s/*_original.tif', tmpath))


#r <- rast(file.path(tmpath, "twi.tif"))
#plot(r)

#r <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif")


#r <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif")
#plot(aggregate(r$tourbiere_minerotrophe, 10, na.rm = TRUE))

#r <- rast("/home/frousseu/data2/qc/predictors_100_QC.tif")
#plot(aggregate(r$mean_monthly_precipitation_amount_of_the_wettest_quarter, 10, na.rm = TRUE))
#plot(r$annual_precipitation_amount)

#r <- rast("/home/frousseu/data2/qc/till.tif")
#plot(aggregate(r, 10, na.rm = TRUE))

#r <- rast("/home/frousseu/data2/qc/till_cog.tif")
#plot(aggregate(r, 20, na.rm = TRUE))


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
#tif_files = tif_files[31:51]
band_names = [os.path.splitext(os.path.basename(f))[0] for f in tif_files]

gdal.BuildVRT(vrt_filename, tif_files, separate=True)

vrt_ds = gdal.Open(vrt_filename, gdal.GA_Update)
for i, name in enumerate(band_names):
    vrt_ds.GetRasterBand(i + 1).SetDescription(name)
vrt_ds = None
", input_dir, vrt_file)

system2("/usr/bin/python3", args = c("-c", shQuote(py_script)))

# run through conda to get latest gdal which supports INTERLEAVE=BAND COG which is much faster for aggregations
cmd <- sprintf('bash -c "

  source /home/frousseu/miniconda3/etc/profile.d/conda.sh 

  conda activate gdal-env
  
  gdalinfo --version

  gdal_translate -of COG -r average -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_100_QC.tif"', tmpath, tmpath, tmpath, tmpath)
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

    r <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif")

    test <- fast("/home/frousseu/data2/qc/test.tif")
     
    plot(r$till)  
1+1
    r <- rast("/home/frousseu/data2/sigeom/coarse5.tif")


    target <- "/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif"
    target <- "/home/frousseu/data2/qc/predictors_100_QC.tif"
    out <- "test.tif"
    path <- "/home/frousseu/data2/qc"
    cmd <- sprintf('
        gdal_translate -tr 200 200 -of COG -r average -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s %s/%s', target, path, out)
    system(cmd)


    target <- "/home/frousseu/data2/na/*ion.tif"
    out <- "test.tif"
    vrt <- "stacked.vrt"
    path <- "/home/frousseu/data2/na"
    cmd <- sprintf('
        gdalbuildvrt -separate %s/%s %s

        gdal_translate -tr 200 200 -r average -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/%s %s/%s', path, vrt, target, path, vrt, path, out)
    system(cmd)
 
 
    r <- rast("/home/frousseu/data2/na/test.tif")



}