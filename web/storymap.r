

library(sf)
library(dplyr)



path_raster <- system("echo $SCRATCH", intern = TRUE) # results/rasters
setwd(path_raster)

display_model <- "climatX2 (habitatQC)"
pat <- "_range_small.gpkg"
files <- "_range_proj_" #"c("_range_", rep("_range_proj_", 2))
f <- list.files(pattern = pat)

species <- sub("^([^_]+_[^_]+)_.*", "\\1", f)
scenarios <- c("ssp245", "ssp370", "ssp585")
timeperiod <- c("2041-2070", "2071-2100")

eg <- expand.grid(scenarios = scenarios, timeperiod = timeperiod)

x <- cbind(eg, files = files, model = display_model) |>
  rbind(data.frame(scenarios = "ssp000", timeperiod = "1991-2020", files = "_range_", model = display_model))

cases <- lapply(species, function(i){
   cbind(species = i, x) 
}) |> do.call("rbind", args = _)

cases$file <- paste(cases$species, cases$files, "small.gpkg", sep = "")
cases$layer <- paste(cases$model, " ", cases$scenarios, "_", cases$timeperiod, sep = "")
cases$layer <- ifelse(grepl("1991", cases$layer), cases$model, cases$layer)
cases$newfile <- paste(paste(cases$species, cases$scenarios, cases$timeperiod, sep = "_"), ".pmtiles", sep = "")



for(i in 1:nrow(cases)){
  print(cases$file[i])  
  system(
    sprintf("ogr2ogr -f PMTiles %s %s %s -nln model", cases$newfile[i], cases$file[i], shQuote(cases$layer[i]))
  )
}

system("./s5cmd --dry-run --numworkers 8 cp -acl public-read --sp '/scratch/frousseu/*.pmtiles' s3://bq-io/niches_climatiques/storymap/")


### Turn each tif into a COG for displaying actuel sdm in story map
species <- list.files(pattern = "sdm_small.tif")

for(i in species){
  #print(i)  
  o <- sub("^([^_]+_[^_]+)_.*", "\\1", i)
  print(o)
  system(
    sprintf('gdalwarp -t_srs EPSG:4326 -of COG -co COMPRESS=DEFLATE -srcnodata nan -dstnodata nan -co RESAMPLING=AVERAGE -wo UNIFIED_SRC_NODATA=YES GTIFF_DIR:$(for i in $(seq 1 3); do gdalinfo GTIFF_DIR:$i:%s | grep -q "%s" && echo $i; done):%s %s_storymap_sdm.tif', i, display_model, i, o)
  )
}

system("~/s5cmd --numworkers 8 cp -acl public-read --sp '/scratch/frousseu/*storymap_sdm.tif' s3://bq-io/niches_climatiques/storymap/")



### Range Maps

vert <- st_read("/home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/data/vertébrés.gpkg") |>
  filter(species %in% gsub("_", " ", !!species)) |>
  select(species, geom) |>
  st_transform(4326)

emvs <- st_read("/home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/data/emvs_dq.gpkg") |>
  mutate(species = SNAME) |>
  filter(species %in% c("Catharus bicknelli", "Coturnicops noveboracensis", "Ixobrychus exilis", "Setophaga cerulea", "Aquila chrysaetos")) |>
  st_buffer(20000) |>
  #st_as_sf() |>
  rename(geom = Shape) |>
  st_set_geometry("geom") |>
  select(species, geom) |>
  group_by(species) |>
  summarise(geom = st_union(geom)) |>
  as.data.frame() |>
  st_as_sf() |>
  st_transform(4326)

aires <- rbind(vert, emvs)

st_write(aires, "storymap_ranges.gpkg", append = TRUE)

system("ogr2ogr -f PMTiles storymap_ranges.pmtiles storymap_ranges.gpkg storymap_ranges")
 
system("~/s5cmd --numworkers 8 cp -acl public-read --sp '/scratch/frousseu/storymap_ranges.pmtiles' s3://bq-io/niches_climatiques/storymap/") 