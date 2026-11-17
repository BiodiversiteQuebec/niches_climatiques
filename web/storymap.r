

library(sf)
library(dplyr)

# module load StdEnv/2023 gcc/12.3 r/4.5.0 geos/3.12.0 gdal/3.9.1 udunits/2.2.28 gsl/2.7 jags/4.3.2
# module load StdEnv/2026 gcc/15.3.0 r/4.6.1 geos/3.14.1 gdal/3.13.3 udunits/2.2.28 gsl/2.8

# gdal vector pipeline read Aquila_chrysaetos_ssp245_2041-2070_change.gpkg ! reproject --dst-crs EPSG:6624 ! geom explode-collections ! write --overwrite tmp.gpkg && gdal vector pipeline --progress read tmp.gpkg ! geom simplify --tolerance 100 ! geom buffer --distance 2000 --quadrant-segments 4 ! write --overwrite out.gpkg && ogr2ogr -overwrite dissolved.gpkg out.gpkg -dialect SQLite -sql 'SELECT ST_Union(geom) AS geom FROM "Aquila_chrysaetos_ssp245_2041-2070_change"' -nlt MULTIPOLYGON && gdal vector pipeline --progress read dissolved.gpkg ! geom simplify --tolerance 100 ! geom buffer --distance 10000 --quadrant-segments 8 ! geom buffer --distance -10000 --quadrant-segments 8 ! reproject --dst-crs EPSG:4326 ! write --overwrite smoothed.gpkg

smooth_range <- function(file, layer, out = sub("\\.gpkg$", "_smoothed.gpkg", file)) {
  if (normalizePath(out, mustWork = FALSE) == normalizePath(file, mustWork = FALSE))
    stop("`out` must differ from `file`")

  tmp <- tempfile(fileext = c("_explode.gpkg", "_buffer.gpkg", "_dissolve.gpkg"))
  q <- shQuote
  sql <- sprintf('SELECT ST_Union(geom) AS geom FROM "%s"', layer)

  cmd <- sprintf(paste(
    "gdal vector pipeline read --layer %s %s ! reproject --dst-crs EPSG:6624 ! explode-collections ! write --overwrite --lco GEOMETRY_NAME=geom %s",
    "&& gdal vector pipeline --progress read %s ! simplify --tolerance 100 ! buffer --distance 2000 --quadrant-segments 4 ! write --overwrite --lco GEOMETRY_NAME=geom %s",
    "&& ogr2ogr -overwrite %s %s -dialect SQLite -sql %s -nlt MULTIPOLYGON",
    "&& gdal vector pipeline --progress read %s ! simplify --tolerance 100 ! buffer --distance 10000 --quadrant-segments 8 ! buffer --distance -10000 --quadrant-segments 8 ! write --overwrite %s"
  ),
    q(layer), q(file), q(tmp[1]),
    q(tmp[1]), q(tmp[2]),
    q(tmp[3]), q(tmp[2]), q(sql),
    q(tmp[3]), q(out)
  )

  # run the chain, always remove intermediates, keep the chain's exit status
  status <- system(sprintf("(%s); s=$?; rm -f %s; exit $s", cmd, paste(q(tmp), collapse = " ")))
  if (status != 0) stop("GDAL chain failed (exit status ", status, ")")
  invisible(out)
  out
}

#path_raster <- system("echo $SCRATCH", intern = TRUE) # results/rasters

path_raster <- "/home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/scratch" # results/rasters
setwd(path_raster)

qc <- st_read("../../data/QC.gpkg")

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
#cases <- cases[1:2, ]


for(i in 64:nrow(cases)){
  print(cases$file[i])  
  #system(
  #  sprintf("ogr2ogr -f PMTiles %s %s %s -nln model", cases$newfile[i], cases$file[i], shQuote(cases$layer[i]))
  #)

  current <- smooth_range(file = gsub("_proj_", "_", cases$file[i]), layer = display_model, out = "current.gpkg") |>
    st_read()
  projected <- smooth_range(file = cases$file[i], layer = cases$layer[i], out = "projected.gpkg") |>
    st_read()

  if(cases$scenarios[i] == "ssp000"){
    change <- current |> mutate(change = "stable")
  } else {

    minus <- st_difference(current, projected) |> st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON") |> st_union() |> st_geometry() |> st_union() |> st_as_sf() |> mutate(change = "loss") |> st_make_valid()
    plus <- st_difference(projected, current) |> st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON") |> st_union() |> st_geometry() |> st_as_sf() |> mutate(change = "gain") |> st_make_valid()
    equal <- st_intersection(projected, current) |> st_collection_extract("POLYGON") |> st_cast("MULTIPOLYGON") |> st_union() |> st_geometry() |> st_as_sf() |> mutate(change = "stable") |> st_make_valid()

    change <- rbind(minus, plus, equal)
  }

  change <- st_intersection(change, qc) |> st_transform(4326) |> st_make_valid() # make sure with buffer on precise locations that it does not extend pas the QC polygon 


  #png("/home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/plot.png", width = 7, height = 7, res = 300, units = "in")
  #plot(st_geometry(plus), border = NA, col = adjustcolor(c("tomato", "forestgreen", "blue"), 0.5))
  #dev.off()

  st_write(change, file.path(path_raster, gsub(".pmtiles", "_change.gpkg", cases$newfile[i])), append = FALSE)


  system(
    sprintf('ogr2ogr -f PMTiles %s %s %s -nln model -sql "SELECT geom, change FROM \\"%s\\""', gsub(".pmtiles", "_change.pmtiles", cases$newfile[i]), gsub(".pmtiles", "_change.gpkg", cases$newfile[i]), shQuote(cases$layer[i]), gsub(".pmtiles", "_change", cases$newfile[i]))
  )


#system(
#  sprintf(
#    'ogr2ogr -f PMTiles %s %s -nln model -sql "SELECT geom, change FROM \\"%s\\""',
#    gsub(".pmtiles", "_change.pmtiles", cases$newfile[i]),
#    cases$file[i],
#    cases$layer[i]
#  )
#)

}

unlink("current.gpkg")
unlink("projected.gpkg")


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

st_write(aires, "storymap_ranges.gpkg", append = FALSE)

system("ogr2ogr -f PMTiles storymap_ranges.pmtiles storymap_ranges.gpkg storymap_ranges")
 


### Send stuff to S3
#system(sprintf(". ~/.ssh/s5cmd_bq.sh; ~/s5cmd --dry-run --numworkers 8 cp -acl public-read --sp '%s/*_change.pmtiles' s3://bq-sdm/niches_climatiques/storymap/", path_raster))

#system(sprintf(". ~/.ssh/s5cmd_bq.sh; ~/s5cmd --numworkers 8 cp -acl public-read --sp '%s/*storymap_sdm.tif' s3://bq-sdm/niches_climatiques/storymap/", path_raster))

#system(sprintf(". ~/.ssh/s5cmd_bq.sh; ~/s5cmd --numworkers 8 cp -acl public-read --sp '%s/storymap_ranges.pmtiles' s3://bq-sdm/niches_climatiques/storymap/", path_raster)) 



