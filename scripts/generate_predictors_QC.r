

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
    "geomflat_perc", "geomflat", "% de terrains plats",
    "geomfootslope_per", "geomfootslope", "% de bas de pentes"
  ),
  "earthenv_topography" = c(
    "elevation", "elevation", "Élévation",
    "vrm", "ruggedness", "Indice de rugosité topographique"
  ),
  "soilgrids" = c(
    "sand_0-5cm", "sand", "% de sable",
    "clay_0-5cm", "clay", "% d'argile",
    "silt_0-5cm", "silt", "% de limon",
    "phh2o_0-5cm", "ph", "pH",
    "nitrogen_0-5cm", "nitrogen", "Azote",
    "bdod_0-5cm", "bulk_density", "Densité volumique",
    "soc_0-5cm", "soil_organic_carbon", "Carbone organique du sol",
    "ocd_0-5cm", "organic_carbon_density", "Densité de carbone organique"
  ),
  "distance_to_roads" = c(
    "distance_to_roads", "distance_to_roads", "Distance aux routes"
  ),
  "silvis" = c(
   "NDVI16_cumulative", "ndvi", "NDVI",
   "LAI8_cumulative", "lai", "Index de surface foliaire"
  ),
  "ghmts" = c(
    "GHMTS", "human_modification", "Modifications humaines"
  ),
  "twi" = c(
    "twi", "twi", "Indice d'humidité topograhique"
  ),
  "mhc" = c(
    "mhc", "mhc", "Hauteur de la canopée"
  ),
  "sigeom_zones_morphosedimentologiques_percentage" = c(
    "Alluvion_1", "alluvion", "% de terrains d'alluvions",
    "Dépôt_2", "depot", "% de terrains de dépôts",
    "Éolien_3", "eolien", "% de dépôts éoliens",
    "Glaciaire_4", "glaciaire", "% de dépôts glaciaires",
    "Anthropogénique_5", "anthropogenique", "% de dépôts anthropogéniques",
    "Lacustre_6", "lacustre", "% de dépôts lacustres",
    "Glaciolacustre_7", "glaciolacustre", "% de dépôts glaciolacustres",
    "Marin_8", "marin", "% de dépôts marins",
    "Glaciomarin_9", "glaciomarin", "% de dépôts glaciomarins",
    "Organique_10", "organique", "% de dépôts organiques",
    "Quaternaire_11", "quaternaire", "% de dépôts quaternaires",
    "Roche_12", "roche", "% de dépôt rocheux",
    "Till_13", "till", "% de dépôts de till"
  ),
  "mhp2023_percentage" = c(
    "Eau_peu_profonde_1", "eau_peu_profonde", "% d'eau peu profonde",
    "Marais_2", "marais", "% de marais",
    "Marécage_3", "marecage", "% de marécages",
    "Milieu_humide_indifférencié_4", "indifferencie", "% de zones humides indifférenciées",
    "Prairie_humide_5", "prairie_humide", "% de prairies humides",
    "Tourbière_boisée_6", "tourbiere_boisee", "% de tourbières boisées",
    "Tourbière_ouverte_indifférenciée_7", "tourbiere_indifferenciee", "% de tourbières indifférenciées",
    "Tourbière_ouverte_minérotrophe_8", "tourbiere_minerotrophe", "% de tourbières ouverte minérotrophe",
    "Tourbière_ouverte_ombrotrophe_9", "tourbiere_ombrotrophe", "% de tourbières ouverte ombrotrophe"
  ),
  "GRHQ" = c(
    "lakes", "distance_to_lakes", "Distance aux lacs",
    "rivers", "distance_to_rivers", "Distance aux rivières",
    "streams", "distance_to_streams", "Distance aux cours d'eau",
    "stlawrence", "distance_to_stlawrence", "Distance au Saint Laurent",
    "coast_stlawrence", "distance_to_coaststlawrence", "Distance à à côte et à l'axe du St-Laurent"
  ),
  "cop-dem-glo" = c(
    "elevation", "elevation", "Élévation",
    "ruggedness", "ruggedness", "Indice de rugosité topographique",
    "distance_to_cliffs", "distance_to_cliffs", "Distance aux falaises"
  ),
  "geomorphons_percentages" = c(
    "flat", "flat", "% de terrains plats"
  )
)

collections <- lapply(collections, function(i){
  list(
    var = i[seq(1, length(i), by = 3)],
    name = i[seq(2, length(i), by = 3)],
    fr = i[seq(3, length(i), by = 3)]    
  )
})


###############################################
### add collections with automated names ######

climvars <- c(
  "mean annual air temperature", "Température moyenne annuelle",
  "mean diurnal air temperature range", "Amplitude de la température moyenne annuelle ",
  "isothermality", "Isothermalité",
  "temperature seasonality", "Saisonnalité de la température",
  "mean daily maximum air temperature of the warmest month", "Température maximale quotidienne moyenne du mois le plus chaud",
  "mean daily minimum air temperature of the coldest month", "Température minimale quotidienne moyenne du mois le plus froid",
  "annual range of air temperature", "Amplitude annuelle de la température",
  "mean daily mean air temperatures of the wettest quarter", "Température moyenne quotidienne du trimestre le hlus humide",
  "mean daily mean air temperatures of the driest quarter", "Température moyenne quotidienne du trimestre le plus sec",
  "mean daily mean air temperatures of the warmest quarter", "Température moyenne quotidienne du trimestre le plus chaud",
  "mean daily mean air temperatures of the coldest quarter", "Température moyenne quotidienne du trimestre le plus froid",
  "annual precipitation amount", "Précipitations annuelles",
  "precipitation amount of the wettest month", "Précipitations du mois le plus humide",
  "precipitation amount of the driest month", "Précipitations du mois le plus sec",
  "precipitation seasonality", "Saisonnalité des précipitations",
  "mean monthly precipitation amount of the wettest quarter", "Précipitations mensuelles moyennes du trimestre le plus humide",
  "mean monthly precipitation amount of the driest quarter", "Précipitations mensuelles moyennes du trimestre le plus sec",
  "mean monthly precipitation amount of the warmest quarter", "Précipitations mensuelles moyennes du trimestre le plus chaud",
  "mean monthly precipitation amount of the coldest quarter", "Précipitations mensuelles moyennes du trimestre le plus froid"
)


collections[["chelsa-clim"]]$var <- paste0("bio", 1:19)
collections[["chelsa-clim"]]$name <- gsub(" ", "_", climvars[seq(1, length(climvars), by = 2)])
collections[["chelsa-clim"]]$fr <- climvars[seq(2, length(climvars), by = 2)]

cecvars <- c(
  "coniferous", "% de forêts conifériennes",
  "taiga", "% de forêts de taïga",
  "tropical_evergreen", "% de forêts tropicales sempervirentes",
  "tropical_deciduous", "% de forêts feuillues tropicales",
  "deciduous", "% de forêts feuillues",
  "mixed", "% de forêts mixtes", 
  "tropical_shrub", "% d'arbustaies tropicales", 
  "temperate_shrub", "% d'arbustaies tempérées", 
  "tropical_grass", "% de prairies tropicales", 
  "temperate_grass", "% de prairies tempérées", 
  "polar_shrub", "% d'arbustaies polaires", 
  "polar_grass", "% de prairies polaires", 
  "lichen", "% de lichens", 
  "wetland", "% de milieux humides", 
  "cropland", "% de milieux cultivés ou agricoles", 
  "barren", "% de milieux dénudés", 
  "urban", "% de milieux urbains", 
  "water", "% de d'eau", 
  "snow", "% de surfaces enneigées"
)

collections[["cec_land_cover_percentage"]]$var <- paste0("cec_land_cover_percent_class_", 1:19)
collections[["cec_land_cover_percentage"]]$name <- cecvars[seq(1, length(cecvars), by = 2)]
collections[["cec_land_cover_percentage"]]$fr <- cecvars[seq(2, length(cecvars), by = 2)]


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


variables <- data.frame(coll = rep(names(collections), times = sapply(collections, function(i){length(i$var)})), var = unlist(lapply(collections, function(i){i$var}), use.names = FALSE), name = unlist(lapply(collections, function(i){i$name}), use.names = FALSE), fr = unlist(lapply(collections, function(i){i$fr}), use.names = FALSE))


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

if(TRUE){
  desc <- variables
  names(desc) <- c("collection", "var", "variable", "fr", "url")
  #desc <- desc[, c("collection", "variable")]
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

  gdal_translate -of COG -r average -co OVERVIEW_RESAMPLING=AVERAGE -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_100_QC.tif"', tmpath, tmpath, tmpath, tmpath)
system(cmd)



##############################################################
### little add-on to produce low res predictors ############## 

cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
#cmd <- sprintf('gdal_translate -of COG -r average -tr 500 500 -co COMPRESS=DEFLATE %s/%s.tif %s/%s_lowres.tif', tmpath, variables$name[i], tmpath, variables$name[i])
cmd <- sprintf('gdalwarp -r average -tr 500 500 -srcnodata -9999 -dstnodata -9999 -ovr NONE -co COMPRESS=DEFLATE %s/%s.tif %s/%s_lowres.tif', tmpath, variables$name[i], tmpath, variables$name[i]) # do not use overview in resampling and no need to produce COG here
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

  gdal_translate -of COG -r average -co OVERVIEW_RESAMPLING=AVERAGE -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_500_QC.tif"', tmpath, tmpath, tmpath, tmpath)
system(cmd)


cmd <- sprintf('rm %s/*_lowres.tif', tmpath)
system(cmd)

#quit(save = "no")

#s5cmd --numworkers 8 sync -acl public-read '/home/frousseu/data2/qc/*.tif' s3://bq-io/sdm_predictors/qc/
#s5cmd --numworkers 8 sync -acl public-read '/home/frousseu/data2/qc/*.csv' s3://bq-io/sdm_predictors/qc/

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