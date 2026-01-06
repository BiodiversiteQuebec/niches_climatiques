

options(width = 150)
#terraOptions(tempdir = "/home/frousseu/data2/tmp", memfrac = 0.8)

#predictors <- rast("data/predictors.tif")
#names(predictors)[c(20, 21, 24, 25, 26)] <- c("geomfootslope", "geomflat", "silt", "sand", "clay")
#predictors_proj <- rast("data/predictors_proj.tif")
#names(predictors_proj)[c(20, 21, 24, 25, 26)] <- c("geomfootslope", "geomflat", "silt", "sand", "clay")

### vars to pool
forest_cats <- c("mixed", "coniferous", "tropical_evergreen", "tropical_deciduous", "deciduous", "temperate_deciduous", "taiga")
bog_cats <- c("tourbiere_boisee", "tourbiere_indifferenciee", "tourbiere_minerotrophe", "tourbiere_ombrotrophe")
openwater_cats <- c("distance_to_lakes", "distance_to_rivers")

predictors <- rast("data/predictors_1000_NA.tif")
#predictors <- predictors[[!duplicated(names(predictors))]] # not sure why there are some duplicates in there...
#predictors <- aggregate(predictors, 10, na.rm = TRUE) # 2
predictors$forest <- sum(predictors[[intersect(forest_cats, names(predictors))]])

# scenarios
timeperiod <- c("2071-2100", "2041-2070", "2011-2040")
model <- c("ukesm1-0-ll", "mri-esm2-0", "mpi-esm1-2-hr", "ipsl-cm6a-lr", "gfdl-esm4")
ssp <- c("ssp585", "ssp370", "ssp126")

keep <- grep(paste(c(timeperiod, model, ssp), collapse = "|"), names(predictors), value = TRUE, invert = TRUE)

# scenarios to consider
timeperiod <- timeperiod[1:3]
model <- model[5]
ssp <- ssp[2]

scenarios <- expand.grid(timeperiod = timeperiod, model = model, ssp = ssp) |>
      apply(1, function(i){paste(i, collapse = "_")}) |>
      sort()

proj <- predictors
predictors_proj <- lapply(scenarios, function(i){
  climate <- proj[[grep(i, names(proj), value = TRUE, perl = TRUE)]]
  names(climate) <- gsub(paste0("_", i), "", names(climate))
  h <- names(predictors)[!names(predictors) %in% c(names(climate), grep(paste(scenarios, collapse = "|"), names(predictors), value = TRUE))]
  c(predictors[[keep]][[h]], climate)
})
names(predictors_proj) <- scenarios
    
plarge <- predictors[[keep]]
plarge_proj <- predictors_proj

#png("proj.png", width = 10, height = 10, units = "in", res = 300)
#r <- c(predictors$mean_annual_air_temperature, predictors_proj$mean_annual_air_temperature)
#zlim <- global(r, "range", na.rm = TRUE) |> unlist() |> range()
#plot(r, range = zlim)
##plot(diff(r))
#dev.off()

#psmall <-rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif") |>
#  aggregate(5, na.rm = TRUE)
#writeRaster(psmall, "data/predictors_QC_500.tif", filetype = "COG", gdal=c("COMPRESS=DEFLATE"))

psmall <-rast("data/predictors_500_QC.tif")
#psmall <- aggregate(psmall, 10, na.rm = TRUE)
psmall$forest <- sum(psmall[[intersect(forest_cats, names(psmall))]])
psmall$tourbiere <- sum(psmall[[intersect(bog_cats, names(psmall))]])
psmall$distance_to_openwater <- min(psmall[[intersect(openwater_cats, names(psmall))]])

# duplicate scenarios for psmall to facilitate iterations later (could want to produce scenarios also in QC predictors even though we only build climate models at large scale or adjust iterations later to avoid producing duplicates for models in which climate projections do not make sens since based only on habitat)

psmall_proj <- lapply(scenarios, function(j){psmall})
names(psmall_proj) <- scenarios

p <- list(small = psmall, large = plarge)
p_proj <- list(small = psmall_proj, large = plarge_proj)

rm(psmall, psmall_proj, plarge, plarge_proj, predictors, predictors_proj)


desc <- read.csv("data/qcdescription.csv") |> arrange(collection, variable)
on <- names(p$small)[order(match(names(p$small), desc$variable), na.last = NA)]
pp <- p$small[[on]]
xxx <- ext(p$small)$xmin + 0.80 * abs((ext(p$small)$xmax - ext(p$small)$xmin))
yyy <- ext(p$small)$ymin + 0.90 * abs((ext(p$small)$ymax - ext(p$small)$ymin))
plg <- list(x = xxx, y = yyy, size = c(0.4, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out")
png("predictors_small.png", width = 26, height = 36, units = "in", res = 200)
plot(pp, mar = c(0, 0, 2, 0), maxnl = 100, maxcell = 1e7, main = names(pp), plg = plg, axes = FALSE, fun = function(){plot(st_geometry(lakes), col = "white", border = NA, add = TRUE)})
dev.off()


desc <- read.csv("data/nadescription.csv") |> arrange(collection, variable)
on <- names(p$large)[order(match(names(p$large), desc$variable), na.last = NA)]
pp <- p$large[[on]]
xxx <- ext(p$large)$xmin + 0.90 * abs((ext(p$large)$xmax - ext(p$large)$xmin))
yyy <- ext(p$large)$ymin + 0.90 * abs((ext(p$large)$ymax - ext(p$large)$ymin))
plg <- list(x = xxx, y = yyy, size = c(0.4, 1.25), tic.box.col = "#ddd", tic.lwd = 0.5, tic.col = "#777", tic = "out")
png("predictors_large.png", width = 26, height = 26, units = "in", res = 200)
plot(pp, mar = c(0, 0, 2, 0), maxnl = 100, maxcell = 1e7, main = names(pp), plg = plg, axes = FALSE, fun = function(){plot(st_geometry(lakes), col = "white", border = NA, add = TRUE)})
dev.off()



if(FALSE){
    url <- "/vsicurl/https://object-arbutus.cloud.computecanada.ca"
    bucket <- "bq-io/sdm_predictors/na" 
    r <- sprintf('s5cmd ls --exclude "predictors*" s3://%s/*.tif', bucket) |>
        system(intern = TRUE) |>
        strsplit(" ") |>
        sapply(function(i){i[length(i)]}) |>
        (\(.) file.path(url, bucket, .))() |>
        lapply(rast) |>
        rast()

    p <- aggregate(r, 10, na.rm = TRUE)

    writeRaster(p, "data/predictors_NA_2000.tif", filetype = "COG", gdal=c("COMPRESS=DEFLATE"))

    url <- "/vsicurl/https://object-arbutus.cloud.computecanada.ca"
    bucket <- "bq-io/sdm_predictors/na" 
    r <- sprintf('s5cmd ls --exclude "predictors*" s3://%s/*.tif', bucket) |>
    system(intern = TRUE) |>
    strsplit(" ") |>
    sapply(function(i){i[length(i)]}) |>
    (\(.) file.path(url, bucket, .))() |>
    lapply(rast) |>
    rast()

    climate <- grep("temperature|precipitation|isothermality", names(r), value = TRUE)
    climate <- climate[4]

    p <- aggregate(r[[climate]], 20)
    p_proj <- p + 2


    p <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/sand.tif")






    #################################

    predictors <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif")

    predictors <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/sand.tif")

    p1 <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/sand.tif")

    r <- aggregate(predictors, 10, na.rm = TRUE)


    url <- "/vsicurl/https://object-arbutus.cloud.computecanada.ca"
    bucket <- "bq-io/sdm_predictors/na" 
    layers <- sprintf('s5cmd ls --exclude "predictors*" s3://%s/*.tif', bucket) |>
    system(intern = TRUE) |>
    strsplit(" ") |>
    sapply(function(i){i[length(i)]}) |>
    (\(.) file.path(url, bucket, .))()


    r <- rast(lapply(layers, rast))

    rr <- aggregate(r$mean_annual_air_temperature, 20)


    cmd <- sprintf('gdalwarp -overwrite -dstnodata -9999.0 -r average -tr 1000 1000 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 %s %s/%s', layers[1], "/home/frousseu/data/niches_climatiques", basename(layers[1]))
    system(cmd)

    cmd <- sprintf('gdalwarp -overwrite -dstnodata -9999.0 -r average -tr 1000 1000 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 %s %s/%s', "/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif", "/home/frousseu/data/niches_climatiques/data", "predictors_1000_QC.tif")
    system(cmd)

    p <- rast("data/sand_cog.tif")


    cmd <- sprintf('gdal_translate -of COG -r average -co INTERLEAVE=BAND -co COMPRESS=DEFLATE -co NUM_THREADS=ALL_CPUS -co BIGTIFF=YES %s/%s.tif %s/%s.tif', "/home/frousseu/data2/na", "sand", "/home/frousseu/data/niches_climatiques/data", "sand")
    system(cmd)

    cmd <- sprintf('gdalwarp -overwrite -dstnodata -9999.0 -r average -tr 1000 1000 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 %s %s/%s', "/home/frousseu/data/niches_climatiques/data/sand.tif", "/home/frousseu/data/niches_climatiques/data", "sand_cog.tif")
    system(cmd)

    cmd <- sprintf('gdalwarp -overwrite -dstnodata -9999.0 -r average -tr 1000 1000 -t_srs EPSG:6624 -co COMPRESS=DEFLATE -co BIGTIFF=YES -ot Float32 -wm 6000 -wo NUM_THREADS=ALL_CPUS --config GDAL_CACHEMAX 4096 %s %s/%s', "/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/sand.tif", "/home/frousseu/data/niches_climatiques/data", "sand.tif")
    system(cmd)

    p <- rast("/home/frousseu/data/niches_climatiques/data/sand.tif")

    p <- rast("/home/frousseu/data2/na/sand.tif")

    p <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/na/sand.tif")


### produce below st-lawrence
png("st.png", width = 5, height = 6, units = "in", res = 200)
plot(p$small[["distance_to_stlawrence"]], mar = c(0, 0, 2, 0), maxcell = 1e6, plg = plg, axes = FALSE, fun = function(){plot(st_geometry(lakes), col = "white", border = NA, add = TRUE)})
plot(st_geometry(ps), add = TRUE)
plot(st_geometry(south), border = "red", add = TRUE)
dev.off()

r <- p$small[["distance_to_stlawrence"]]
r <- ifel(r > 0, 1, NA)
ps <- as.polygons(r, aggregate = TRUE) |>
  st_as_sf() |>
  ms_explode() 

south <- ps[rev(order(st_area(ps)))[2], ]  





}



#pp <- rast("data/predictors_1000_NA.tif")$mean_annual_air_temperature
#pp[pp >= -32] <- NA
#pp <- aggregate(pp, 50, fun = "max", na.rm = TRUE)
#png("mat.png", width = 9, height = 9, units = "in", res = 300)
#plot(pp)
#plot(st_geometry(na), add = TRUE)
#dev.off()