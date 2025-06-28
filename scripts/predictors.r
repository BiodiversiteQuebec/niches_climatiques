

options(width = 150)
terraOptions(tempdir = "/home/frousseu/data2/tmp", memfrac = 0.8)

predictors <- rast("data/predictors.tif")
predictors_proj <- rast("data/predictors_proj.tif")

#predictors <- rast("data/predictors_NA_1000.tif")
#predictors_proj <- rast("data/predictors_proj.tif")

plarge <- aggregate(predictors, 2, na.rm = TRUE)
plarge_proj <- aggregate(predictors_proj, 2, na.rm = TRUE)

#psmall <-rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/sdm_predictors/qc/predictors_100_QC.tif") |>
#  aggregate(5, na.rm = TRUE)
#writeRaster(psmall, "data/predictors_QC_500.tif", filetype = "COG", gdal=c("COMPRESS=DEFLATE"))

psmall <-rast("data/predictors_QC_500.tif")

p <- list(small = psmall, large = plarge)
p_proj <- list(small = psmall, large = plarge_proj)

rm(psmall, plarge, plarge_proj)

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

    p <- aggregate(r, 20, na.rm = TRUE)

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


}