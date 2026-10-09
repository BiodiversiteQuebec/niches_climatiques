

### This script gathers predictors for modeling the distribution of emv species for the finance indicators or for climate niches. It is meant to be run on a VM or NOT on Rorqual!

## The producing of QC.gpkg can howver be done on rorqual on login node

library(terra)
library(sf)
library(data.table)
library(rstac)
library(geodata)
library(doParallel)
library(foreach)
library(ratlas)
library(rmapshaper)


#library(terra)
#r <- rast("data/predictors.tif")
#r <- rast("data/predictors_100m_band1.tif")
#plot(r)

tmpath <- "/home/frousseu/data2/qc" # data
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

get_fr <- function(coll, stac){ # should pool with get_ids, but a lot of coll do not have fr names yet...
  stac |>
    stac_search(collections = coll) |>
    post_request() |> 
    items_fetch() |>
    _$features |>
    sapply(X = _, function(i){i$properties[["description:fr"]]})
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
can <- gadm("CAN", level = 1, path = tmpath) |> st_as_sf()
qc_gadm <- can[can$NAME_1 %in% c("Québec"), ]
qc_gadm <- st_transform(qc_gadm, epsg)

qc_atlas <- ratlas::db_read_table(table_name = "regions", type = "admin", scale = 1, output_geometry = TRUE) |>
  sf::st_as_sf() |>
  sf::st_transform(epsg)

pols <- st_difference(qc_atlas, qc_gadm) |>
  ms_explode() |>
  st_buffer(0.5) # to avoid tiny alignements problems
pols$area <- as.numeric(st_area(pols))
pols <- pols[rev(order(pols$area)), ]


g <- st_intersection(qc_atlas, qc_gadm)

# uses gadm land precise map, cut it out with official map to remove offshore islands and add back Labarador parts from the official, buffer it slightly and union it with cut
qc <- rbind(g[, "geometry"], pols[c(2, 4), "geometry"]) |>
  st_union()

region <- qc
st_write(region, file.path(tmpath, "QC.gpkg"), append = FALSE)

# png("plot.png", width = 5, height = 5, units = "in", res = 300); plot(st_geometry(qc)); dev.off()

### STAC information
stac_old <- "https://io.biodiversite-quebec.ca/stac/"
stac_bq <- "https://io.biodiversite-quebec.ca/stac2/"
stac_biab <- "https://stac.geobon.org/"
coll <- "chelsa-clim"

###############################################
### add simple collections ####################

collections <- list(
  "earthenv_topography_derived" = c(
    "geomflat_perc", "earthenv_geomflat", "% de terrains plats", "stac_biab",
    "geomfootslope_per", "earthenv_geomfootslope", "% de bas de pentes", "stac_biab"
  ),
  "earthenv_topography" = c(
    "elevation", "earthenv_elevation", "Élévation", "stac_biab",
    "vrm", "earthenv_ruggedness", "Indice de rugosité topographique", "stac_biab"
  ),
  "soilgrids" = c(
    "sand_0-5cm", "sand", "% de sable", "stac_biab",
    "clay_0-5cm", "clay", "% d'argile", "stac_biab",
    "silt_0-5cm", "silt", "% de limon", "stac_biab",
    "phh2o_0-5cm", "ph", "pH", "stac_biab",
    "nitrogen_0-5cm", "nitrogen", "Azote", "stac_biab",
    "bdod_0-5cm", "bulk_density", "Densité volumique", "stac_biab",
    "soc_0-5cm", "soil_organic_carbon", "Carbone organique du sol", "stac_biab",
    "ocd_0-5cm", "organic_carbon_density", "Densité de carbone organique", "stac_biab"
  ),
  "distance_to_roads" = c(
    "distance_to_roads", "distance_to_roads", "Distance aux routes", "stac_biab"
  ),
  "silvis" = c(
   "NDVI16_cumulative", "ndvi", "NDVI", "stac_biab",
   "LAI8_cumulative", "lai", "Index de surface foliaire", "stac_biab"
  ),
  "ghmts" = c(
    "GHMTS", "human_modification", "Modifications humaines", "stac_biab"
  ),
  "twi" = c(
    "twi", "twi", "Indice d'humidité topograhique", "stac_bq"
  ),
  "mhc" = c(
    "mhc", "mhc", "Hauteur de la canopée", "stac_bq"
  ),
  "sigeom_zones_morphosedimentologiques_percentage" = c(
    "Alluvion_1", "alluvion", "% de dépôts d'alluvions", "stac_old",
    "Dépôt_2", "versant", "% de dépôts de versants", "stac_old",
    "Éolien_3", "eolien", "% de dépôts éoliens", "stac_old",
    "Glaciaire_4", "glaciaire", "% de dépôts glaciaires", "stac_old",
    "Anthropogénique_5", "anthropogenique", "% de dépôts anthropogéniques", "stac_old",
    "Lacustre_6", "lacustre", "% de dépôts lacustres", "stac_old",
    "Glaciolacustre_7", "glaciolacustre", "% de dépôts glaciolacustres", "stac_old",
    "Marin_8", "marin", "% de dépôts marins", "stac_old",
    "Glaciomarin_9", "glaciomarin", "% de dépôts glaciomarins", "stac_old",
    "Organique_10", "organique", "% de dépôts organiques", "stac_old",
    "Quaternaire_11", "quaternaire", "% de dépôts quaternaires", "stac_old",
    "Roche_12", "roche", "% de dépôts rocheux", "stac_old",
    "Till_13", "till", "% de dépôts de till", "stac_old"
  ),
  "mhp2023_percentage" = c(
    "Eau_peu_profonde_1", "eau_peu_profonde", "% d'eau peu profonde", "stac_bq",
    "Marais_2", "marais", "% de marais", "stac_bq",
    "Marécage_3", "marecage", "% de marécages", "stac_bq",
    "Milieu_humide_indifférencié_4", "indifferencie", "% de zones humides indifférenciées", "stac_bq",
    "Prairie_humide_5", "prairie_humide", "% de prairies humides", "stac_bq",
    "Tourbière_boisée_6", "tourbiere_boisee", "% de tourbières boisées", "stac_bq",
    "Tourbière_ouverte_indifférenciée_7", "tourbiere_indifferenciee", "% de tourbières indifférenciées", "stac_bq",
    "Tourbière_ouverte_minérotrophe_8", "tourbiere_minerotrophe", "% de tourbières ouverte minérotrophe", "stac_bq",
    "Tourbière_ouverte_ombrotrophe_9", "tourbiere_ombrotrophe", "% de tourbières ouverte ombrotrophe", "stac_bq"
  ),
  "GRHQ" = c(
    "lakes", "distance_to_lakes", "Distance aux lacs", "stac_bq",
    "rivers", "distance_to_rivers", "Distance aux rivières", "stac_bq",
    "streams", "distance_to_streams", "Distance aux cours d'eau", "stac_bq",
    "stlawrence", "distance_to_stlawrence", "Distance au Saint-Laurent", "stac_bq",
    "coast_stlawrence", "distance_to_coaststlawrence", "Distance à la côte et à l'axe du Saint-Laurent", "stac_bq"
  ),
  "cop-dem-glo" = c(
    "elevation", "elevation", "Élévation", "stac_bq",
    "ruggedness", "ruggedness", "Indice de rugosité topographique", "stac_bq",
    "distance_to_cliffs", "distance_to_cliffs", "Distance aux falaises", "stac_bq"
  ),
  "geomorphons_percentages" = c(
    "flat", "flat", "% de terrains plats", "stac_old"
  ),
  "carte_eco_code_terrain_depot" = c(
    "distance_aux_habitats_tortues", "distance_aux_habitats_tortues", "Distance aux habitats de tortues", "stac_bq"
  ),
  "geologie_du_socle" = c(
    "basique", "basique", "% de roches basiques", "stac_bq",
    "calcaire", "calcaire", "% de roches calcaires", "stac_bq", 
    "dolomie", "dolomie", "% de roches dolomitiques", "stac_bq", 
    "marbre", "marbre", "% de marbre", "stac_bq", 
    "serpentine", "serpentine", "% de roches serpentineuses", "stac_bq", 
    "mafique" , "mafique", "% de roches mafiques", "stac_bq",
    "ignees_volcaniques", "ignees_volcaniques", "% de roches ignées volcaniques", "stac_bq",
    "ignees_intrusives", "ignees_intrusives", "% de roches ignées intrusives", "stac_bq",
    "metamorphiques", "metamorphiques", "% de roches métamorphiques", "stac_bq",
    "sedimentaires", "sedimentaires", "% de roches ignées sédimentaires", "stac_bq"
  )
)

collections <- lapply(collections, function(i){
  list(
    var = i[seq(1, length(i), by = 4)],
    name = i[seq(2, length(i), by = 4)],
    fr = i[seq(3, length(i), by = 4)],
    stac = i[seq(4, length(i), by = 4)]     
  )
})


###############################################
### add collections with automated names ######

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
collections[["cec_land_cover_percentage"]]$stac <- rep("stac_bq", length(collections[["cec_land_cover_percentage"]]$var))

### Ouranos
coll <- "ouranos_past_climate_period"

ids <- get_ids(coll, stac(get("stac_bq")))
fr <- get_fr(coll, stac(get("stac_bq")))

collections[[coll]]$var <- ids
collections[[coll]]$name <- ids
collections[[coll]]$fr <- fr
collections[[coll]]$stac <- rep("stac_bq", length(ids))

###############################################
### add collections with more complex names ###

### Ouranos ####################################
coll <- "ouranos_climate_projections"

ids <- get_ids(coll, stac(get("stac_old")))
fr <- get_fr(coll, stac(get("stac_old")))
 
grep("2030|2060|2090", ids, value = TRUE) |>
 strsplit("_") |>
 lapply("[", 3:5) |>
   do.call("rbind", args = _) |>
   as.data.table() |>
   setnames(c("ssp", "timeperiod", "model")) |>
   _[ , (n = .N), by = .(model, ssp)]
invisible(lapply(3:5, function(i){
  dput(sort(unique(sapply(strsplit(ids, "_"), "[", i))))
}))

timeperiod <- c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020", 
"2030", "2040", "2050", "2060", "2070", "2080", "2090")[5:15]
model <- c("mean", "perc10", "perc50", "perc90")[3]
ssp <- c("ssp245", "ssp370", "ssp585")[1:3]

variables <- expand.grid(ssp = ssp, timeperiod = timeperiod, model = model) |>
      apply(1, function(i){paste(i, collapse = "_")})
kids <- ids[which(sub("^([^_]*_){2}", "", ids) %in% variables)] |>
  grep("P1_", x = _, value = TRUE)     
kfr <- fr[match(kids, ids)]  

#collections[[coll]]$var <- kids
#collections[[coll]]$name <- sub("(_|\\s)[^_\\s]*$", "", kids)
#collections[[coll]]$fr <- sub("(_|\\s)[^_\\s]*$", "", kfr)


################################################
#### Put vars together #########################

variables <- data.frame(coll = rep(names(collections), times = sapply(collections, function(i){length(i$var)})), stac = unlist(lapply(collections, function(i){i$stac}), use.names = FALSE), var = unlist(lapply(collections, function(i){i$var}), use.names = FALSE), name = unlist(lapply(collections, function(i){i$name}), use.names = FALSE), fr = unlist(lapply(collections, function(i){i$fr}), use.names = FALSE))


urls <- lapply(seq_along(collections), function(i){
  get_urls(names(collections)[[i]], collections[[i]]$var, stac(get(collections[[i]]$stac[1])))
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

#r <- rast("/home/frousseu/data2/qc/P1_AnnMeanTemp.tif")
#png("plot.png", width = 10, height = 10, units = "in", res = 300);plot(r, mar = c(0,0,0,0));dev.off();system("code plot.png")

cl <- makeCluster(10)
registerDoParallel(cl)
getDoParWorkers()
foreach(i = 1:nrow(variables[1:nrow(variables), ])) %dopar% {
if(grepl("ouranos", variables$coll[i])){meth <- "bilinear"} else {meth <- "average"}  
cmd <- sprintf('gdalwarp -overwrite -cutline %s/QC.gpkg -crop_to_cutline -dstnodata -9999.0 -r %s -tr 100 100 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS -wo CUTLINE_ALL_TOUCHED=TRUE --config GDAL_CACHEMAX 4096 /vsicurl/%s %s/%s.tif', tmpath, meth, variables$url[i], tmpath, variables$name[i])
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
cmd <- sprintf('gdal_calc.py -A %s/P1_AnnMeanTemp.tif --outfile=%s/mask.tif --calc="1*(A!=-9999)" --NoDataValue=none --type=Byte --co="COMPRESS=DEFLATE" --overwrite', tmpath, tmpath)
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
custom_mask("distance_aux_habitats_tortues")

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
foreach(i = 1:nrow(variables[1:nrow(variables), ][1:2,])) %dopar% {
#cmd <- sprintf('gdal_translate -of COG -r average -tr 500 500 -co COMPRESS=DEFLATE %s/%s.tif %s/%s_lowres.tif', tmpath, variables$name[i], tmpath, variables$name[i])
cmd <- sprintf('gdalwarp -r average -tr 200 200 -srcnodata -9999 -dstnodata -9999 -ovr NONE -co COMPRESS=DEFLATE %s/%s.tif %s/%s_lowres.tif', tmpath, variables$name[i], tmpath, variables$name[i]) # do not use overview in resampling and no need to produce COG here
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

  gdal_translate -of COG -r average -co OVERVIEW_RESAMPLING=AVERAGE -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/stacked.vrt %s/predictors_200_QC.tif"', tmpath, tmpath, tmpath, tmpath)
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