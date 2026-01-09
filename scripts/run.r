

t_init <- Sys.time()

library(car)
library(dismo)
library(dplyr)
library(duckdb)
library(duckdbfs)

on.exit({
  if(exists("t_end", envir = .GlobalEnv)){
    cat(paste("Species job completed -", sp, "- array id", array_id, "-", difftime(Sys.time(), t_init, units = "mins")), "\n")
  } else {
    cat(paste("Species job aborted -", sp, "- model id", i), "\n")
  }
  flush.console()
}, add = TRUE
)

array_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#array_id <- 4

args <- commandArgs(trailingOnly=TRUE)
#args <- "niches_climatiques.r"

source(file.path("scripts/jobs", args))

source("scripts/prelim.r")
source("scripts/predictors.r")
#source("scripts/parameters.r")
#source("scripts/species.r")

#if(TRUE){

#for(sp in species){#[-c(9,10,11,13)]){
#sp <- species[1]    
sp <- species[array_id]
#sp <- species
print(sp)
source("scripts/data.r")
source("scripts/variables.r")
for(i in seq_along(models)){
  #i <- 4
  print(names(models)[i])
  source("scripts/models.r")
  source("scripts/predictions.r")
  source("scripts/results.r")
}
source("scripts/graphics.r")

t_end <- Sys.time()

difftime(Sys.time(), t_init, units = "mins")

#}

#}


#sdm3 <- preds
#sdm4 <- sdm2 * (sdm1 / global(sdm1, "max", na.rm = TRUE)[1, 1])


#carte <- function(){
#    plot(st_geometry(obs), cex = 0.5, lwd = 0.1, add = TRUE)
#    plot(st_geometry(na), lwd = 0.25, add = TRUE)
#    plot(st_geometry(lakes), lwd = 0.1, col = "white", add = TRUE) 
#}

#plot(crop(c(sdm1, sdm2, sdm3, sdm4), st_buffer(obs, 400000)), fun = carte)

#plot(crop(c(sdm4), st_buffer(obs, 400000)), fun = carte)

#plot(crop(c(sdm4), qc), fun = carte)


#r <- rast("/vsicurl/https://object-arbutus.cloud.computecanada.ca/bq-io/io/CEC_land_cover/NA_NALCMS_landcover_2020_30m.tif")


#r <- rast("data/land_cover_2020v2_30m_tif/NA_NALCMS_landcover_2020v2_30m/data/NA_NALCMS_landcover_2020v2_30m.tif")

#r <- crop(r, st_transform(obs, st_crs(r)))

#r <- crop(r, c(1900000, 2600000, 300000, 800000))
#r <- crop(r, c(2100000, 2300000, 400000, 700000))
#mh <- ifel(r == 19, 1, NA)
#plot(mh)
#plot(st_geometry(st_transform(na, st_crs(r))), add = TRUE)



#coll <- "cec_land_cover"

#ids <- io |>
#  stac_search(collections = coll) |>
#  post_request() |> 
#  items_fetch() |>
#  _$features |>
#  sapply(X = _, function(i){i$id})



 
