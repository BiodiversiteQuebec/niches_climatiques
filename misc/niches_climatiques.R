
library(terra)
library(sf)
library(geodata)
library(rnaturalearth)
library(rmapshaper)
library(dplyr)
library(duckdb)
options(java.parameters = "-Xmx100g")
library(predicts)
library(gbifdb)

source("https://raw.githubusercontent.com/frousseu/FRutils/refs/heads/master/R/colo.scale.R")

options(terra.pal=map.pal("water"))

predictors <- rast("predictors.tif")
predictors_proj <- rast("predictors_proj.tif")


epsg <- 6624 #32618

# Downloads polygons using package geodata
can <- gadm("CAN", level = 1, path = getwd()) |> st_as_sf()
usa <- gadm("USA", level = 1, path = getwd()) |> st_as_sf()
na <- rbind(can,usa)
na <- st_transform(na, epsg)
na <- na[!na$NAME_1 %in% c("Hawaii", "Alaska"), ]

# keep Québec and bordering provinces/states as a buffer
#region <- na[na$NAME_1 %in% c("Québec", "New Brunswick", "Maine", "Vermont", "New Hampshire", "New York", "Ontario", "Nova Scotia", "Prince Edward Island", "Massachusetts", "Connecticut", "Rhode Island"),]
region <- na[!na$NAME_1 %in% c("Yukon", "British Columbia", "Washington", "Oregon", "California", "Arizona", "Nevada", "Idaho", "Utah"), ]

# split NF into different polygons
labrador <- ms_explode(na[na$NAME_1 %in% c("Newfoundland and Labrador"), ]) 
labrador <- labrador[which.max(st_area(labrador)), ] # keep Labarador
region <- rbind(region, labrador)
qc <- na[na$NAME_1 %in% c("Québec"),] |>
        rbind(labrador) |>
        ms_simplify(0.01)

na <- ms_simplify(na, 0.01)

# Add it to the study region
region<-rbind(region,labrador) 

# Simplify polygons to make things faster
region<-ms_simplify(region,0.005)
region<-st_union(region) |> st_as_sf()

# lakes
lakes<-ne_download(scale="medium",type="lakes",destdir=getwd(),category="physical",returnclass="sf") |> st_transform(epsg)
lakes<-st_filter(lakes,region)


species <- c("Pseudacris triseriata", "Hemidactylium scutatum", "Gyrinophilus porphyriticus", "Desmognathus ochrophaeus", "Emydoidea blandingii", "Glyptemys insculpta", "Nerodia sipedon", "Lampropeltis triangulum", "Aquila chrysaetos", "Catharus bicknelli", "Setophaga cerulea", "Coturnicops noveboracensis", "Ixobrychus exilis", "Glaucomys volans")

species_common_predictors <- c("")

#sp_extra_predictors <- vector(mode = "list", length = length(sp))
#names(sp_extra_predictors) <- sp
#dput(sp_extra_predictors)

species_predictors <- list(`Pseudacris triseriata` = c("wetland"), 
     `Hemidactylium scutatum` = c("bog"), 
     `Gyrinophilus porphyriticus` = c("flat", "forest"), 
     `Desmognathus ochrophaeus` = c("flat", "forest"), 
     `Emydoidea blandingii` = c("water"), 
     `Glyptemys insculpta` = c("water"), 
     `Nerodia sipedon` = c("water"), 
     `Lampropeltis triangulum` = c(""), 
     `Aquila chrysaetos` = c("truggedness", "flat"), 
     `Catharus bicknelli` = c("elevation"), 
     `Setophaga cerulea` = c("forest"), 
     `Coturnicops noveboracensis` = c("flat"), 
     `Ixobrychus exilis` = c("flat", "wetland"), 
     `Glaucomys volans` = c("forest")
)

species_target_groups <- list(`Pseudacris triseriata` = c("wetland"), 
                            `Hemidactylium scutatum` = c("bog"), 
                            `Gyrinophilus porphyriticus` = c("flat", "forest"), 
                            `Desmognathus ochrophaeus` = c("flat", "forest"), 
                            `Emydoidea blandingii` = c("water"), 
                            `Glyptemys insculpta` = c("water"), 
                            `Nerodia sipedon` = c("water"), 
                            `Lampropeltis triangulum` = c(""), 
                            `Aquila chrysaetos` = c("truggedness", "flat"), 
                            `Catharus bicknelli` = c("elevation"), 
                            `Setophaga cerulea` = c("forest"), 
                            `Coturnicops noveboracensis` = c("flat"), 
                            `Ixobrychus exilis` = c("flat", "wetland"), 
                            `Glaucomys volans` = c("forest")
)





source("https://object-arbutus.cloud.computecanada.ca/bq-io/atlas/parquet/bq-atlas-parquet.R")
#atlas <- atlas_remote(parquet_date = tail(atlas_dates, 1))
#atlas <- atlas_local(parquet_date = tail(atlas_dates, 1), getwd())
#atlas <- duckdbfs::open_dataset("atlas_2024-11-07.parquet", tblname = "atlas")
atlas <- duckdbfs::open_dataset("atlas_2024-11-07.parquet", tblname = "atlas")
gbif <- duckdbfs::open_dataset("gbif.parquet")


#g1 <- atlas |> 
#  group_by(phylum) |>
#  summarize(n = n()) |>
#  collect()

#g1 <- atlas |> 
#  group_by(group_fr) |>
#  summarize(n = n()) |>
#  collect()

#g2 <- atlas |> 
#  group_by(group_fr, valid_scientific_name) |>
#  summarize(n = n()) |>
#  group_by(group_fr) |>
#  summarize(species = n()) |>
#  collect()

#g <- full_join(g1, g2) |>
#       mutate(moy = n / species)
#g

#png("bar.png", width = 16, height = 12, units = "in", res = 200)
#barplot(g$moy, names.arg = g$group_fr, cex.names = 0.45)
#dev.off()


#g <- atlas |> 
#  group_by(group_en) |>
#  summarize(n = n()) |>
#  collect() |>
#  as.data.frame() |>
#  _$group_en  


#reptiles <- atlas |> 
#  filter(group_en %in% c("Reptiles", "Amphibians")) |>
#  mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
#  to_sf(crs = 4326) |> 
#  collect() |>
#  as.data.frame()


reptiles <- atlas |> 
  filter(group_en %in% c("Reptiles", "Amphibians")) |>
  collect() |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg) |>
  mutate(species = valid_scientific_name) |>
  mutate(source = "atlas") |>
  mutate(date = paste(year_obs, formatC(month_obs, width = 2, flag = 0), formatC(day_obs, width = 2, flag = 0), sep = "-"))
  #mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  #to_sf(crs = 4326)# |> # |>
  #collect() |>

reptiles_gbif <- gbif |> 
  filter(class %in% c("Squamata", "Amphibia", "Testudines")) |>
  filter(!is.na(decimallongitude) &  !is.na(decimallatitude)) |>
  #count(class) |>
  collect() |>
  mutate(source = "gbif") |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)

reptiles_gbif <- reptiles_gbif[!lengths(st_intersects(reptiles_gbif, qc)), ]

  #st_transform(epsg)

#birds <- atlas |> # too heavy for memory
#  filter(group_en %in% c("Birds")) |>
#  collect() |>
#  as.data.frame()

mammals <- atlas |> 
  filter(group_en %in% c("Mammals")) |>
  collect() |>
  as.data.frame()



#params<-list()
#params$species <- "Bonasa umbellus"
sp <- species[6]
genus <- strsplit(sp, " ")[[1]][1] # temp fix to also get subspecies and string manipulations do not seem to work when dplyr remote


obs <- atlas |> 
  mutate(genus = case_match(genus, "Emys" ~ "Emydoidea", .default = genus)) |>
  filter(genus == !!genus) |> 
  #filter(valid_scientific_name == species) |> 
  #mutate(geom = ST_Point(as.numeric(longitude), as.numeric(latitude))) |> 
  #to_sf(crs = 4326) |> 
  mutate(coordinate_uncertainty = as.numeric(coordinate_uncertainty)) |>
  collect() |>
  as.data.frame() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(epsg)

obs <- obs |>
  mutate(species = sapply(strsplit(valid_scientific_name, " "), function(i){paste(i[1:2], collapse = " ")})) |>
  filter(observation_value != "0") |>
  #mutate(species = sub("^(([^ ]+ ){1}[^ ]+).*", "\\1", valid_scientific_name)) |>
  filter(species == !!sp)

obs <- obs |>
  mutate(date = as.character(as.Date(paste(year_obs, month_obs, day_obs, sep="-"), format = "%Y-%m-%d"))) |>
  mutate(source = "atlas")

obs <- st_transform(obs, epsg)

obs_gbif <- gbif |>
  mutate(species = case_match(species, "Emys blandingii" ~ "Emydoidea blandingii", .default = species)) |>
  filter(species == !!sp) |>
  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
  mutate(coordinateuncertaintyinmeters = as.numeric(coordinateuncertaintyinmeters)) |>
  filter(coordinateuncertaintyinmeters <= 50000 | is.na(coordinateuncertaintyinmeters)) |>
  #mutate(geom = ST_Point(as.numeric(decimallongitude), as.numeric(decimallatitude))) |> 
  #to_sf(crs = 4326) |> 
  collect() |>
  mutate(recordedby = sapply(recordedby, paste, collapse = "; ")) |>
  mutate(source = "gbif") |>
  mutate(coordinate_uncertainty = coordinateuncertaintyinmeters) |>
  as.data.frame() |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), crs = 4326) |>
  st_transform(epsg)# |>

#x$recordedby <- sapply(x$recordedby, paste, collapse = "; ")
  
obs_gbif <- obs_gbif[!lengths(st_intersects(obs_gbif, qc)), ]
  

par(mar = c(1, 1, 1, 1))
plot(st_buffer(st_geometry(obs), 1600000), col = NA, border = NA)
plot(st_geometry(na), col = "grey90", border = "grey80", add = TRUE)
plot(st_geometry(qc), col = "grey90", border = NA, add = TRUE)
plot(st_geometry(qc), border = "grey80", lwd = 1, add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, add = TRUE)
#plot(st_geometry(reptiles_gbif), pch = 21, bg = adjustcolor("black", 0.25), col = adjustcolor("black", 0.75), lwd = 0.25, add = TRUE)
#plot(st_geometry(reptiles), pch = 21, bg = adjustcolor("black", 0.25), col = adjustcolor("black", 0.75), lwd = 0.25, add = TRUE)
plot(st_geometry(obs), pch = 21, bg = adjustcolor("forestgreen", 0.25), col = adjustcolor("forestgreen", 0.75), lwd = 0.25, add = TRUE)
plot(st_geometry(obs_gbif), pch = 21, bg = adjustcolor("brown", 0.25), col = adjustcolor("brown", 0.75), lwd = 0.25, add = TRUE)


n <- intersect(names(reptiles), names(reptiles_gbif))
background <- rbind(reptiles[, n], reptiles_gbif[, n])

n <- intersect(names(obs), names(obs_gbif))
obs <- rbind(obs[, n], obs_gbif[, n])

obs <- obs[region, ]
background <- background[region, ]



p <- aggregate(predictors, 10, na.rm = TRUE)
p_proj <- aggregate(predictors_proj, 10, na.rm = TRUE)

#predictors2<-aggregate(predictors,2,na.rm=TRUE)

#climate_vars <- names(p)[1:19]
#climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
climate_vars <- c("mean_annual_air_temperature", "mean_daily_maximum_air_temperature_of_the_warmest_month", "annual_range_of_air_temperature", "annual_precipitation_amount")
#other_vars <- names(predictors)[20:nlyr(predictors)]
other_vars <- c("geomfootslope_per", "geomflat_perc", "vrm", "elevation", "sand_0-5cm", "forest", "urban", "water", "wetland", "crop")


bg <- background[sample(1:nrow(background), 100000), ]
bg <- rbind(obs, bg)

buff <- st_buffer(obs, 250000) |> st_union()
nbuff <- 100000
x <- st_difference(region, buff) |> st_sample(size = nbuff)# |> st_as_sf()
x <- st_as_sf(cbind(st_drop_geometry(bg[rep(1, nbuff), ]), geometry = x), crs = epsg)
bg <- rbind(bg, x)

vars <- c(names(p)[1:19], other_vars)
vars <- c(climate_vars, other_vars)
vars <- c(climate_vars[c(2, 3)], other_vars)
vars <- c(climate_vars[1:2], other_vars[1:2])
vars <- c(climate_vars[1:2])#, other_vars)
vars <- c(climate_vars[c(1, 3)])
vars <- c(other_vars[c(1, 2, 9, 10)])
vars <- climate_vars[c(1, 3, 4)]
vars <- climate_vars[1]
vars <- c(other_vars)#, other_vars)

#batlas <- background |> filter(source == "atlas")
#bgbif <- background |> filter(source == "gbif") |> sample_n(100000)
#bg <- rbind(batlas, bgbif)
#rr <- c(p[[vars]], p[[vars]]^2, p[[vars]]^3, , p[[vars]]^4)
#names(rr) <- paste0("x", seq_len(nlyr(rr)))
#m <- MaxEnt(rr,
m <- MaxEnt(p[[vars]], 
            #vect(d[d$presence==1,]),vect(d[d$presence==0,]),
            p = vect(obs), a = vect(bg),
            removeDuplicates = FALSE,
            silent = FALSE,
            #args=c("replicatetype=bootstrap","replicates=1","threads=4")
            #args=c("linear","quadratic","product","hinge","nothreshold","replicatetype=bootstrap","replicates=1","threads=4")
            args = c("linear", "quadratic", "noproduct","nohinge", "nothreshold", "noautofeature","replicatetype=bootstrap","replicates=1", "threads=4"
            )
)

predictions <- mask(predict(m, p, args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
predictions_proj <- mask(predict(m, p_proj, args = c("outputformat=raw", "replicatetype=bootstrap")), vect(region))
#preds <- aggregate(predictions, 100, fun = "max", na.rm = TRUE)
preds<- predictions
preds_proj <- predictions_proj
dif <- preds_proj - preds
rr <- unlist(global(dif, "range", na.rm = TRUE)[1, ])

preds <- predictions

#options(terra.pal=rev(sequential_hcl(200, palette = "Terrain")))
options(terra.pal=rev(terrain.colors(200)))
options(terra.pal=viridisLite::viridis(200))
#plot(crop(dif, st_buffer(obs, 1500000)), mar = c(0, 0, 0, 0), axes = TRUE, col = colo.scale(seq(rr[1], rr[2], length.out = 200), rev(c("darkred", "tomato", "white", "steelblue1", "blue4")), center = TRUE))
plot(crop(preds, st_buffer(obs, 1500000)), axes = FALSE)
plot(st_geometry(na), lwd = 0.5, border = adjustcolor("black", 0.5), add = TRUE)
#plot(terra::crop(terra::crop(preds, qc, mask = TRUE), st_buffer(obs, dist = 500000)))
#plot(st_geometry(bg), add = TRUE, pch = 16, cex = 0.15)
plot(st_geometry(obs), col = adjustcolor("black", 0.25), pch = 16, cex = 0.25, add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)

g <- global(crop(p[[vars]], obs, mask = TRUE), mean, na.rm = TRUE)
newdata <- t(g) |> as.data.frame()
newdata <- newdata[rep(1, 500), , drop = FALSE]
v <- seq(-5, 25, length.out = 500)
newdata$mean_annual_air_temperature <- v
plot(v, predict(m, newdata), type = "l")
preds <- predictions

#options(terra.pal=rev(sequential_hcl(200, palette = "Terrain")))
options(terra.pal=rev(terrain.colors(200)))
options(terra.pal=viridisLite::viridis(200))
#plot(crop(dif, st_buffer(obs, 1500000)), mar = c(0, 0, 0, 0), axes = TRUE, col = colo.scale(seq(rr[1], rr[2], length.out = 200), rev(c("darkred", "tomato", "white", "steelblue1", "blue4")), center = TRUE))
plot(crop(preds, st_buffer(obs, 1500000)), axes = FALSE)
plot(st_geometry(na), lwd = 0.5, border = adjustcolor("black", 0.5), add = TRUE)
#plot(terra::crop(terra::crop(preds, qc, mask = TRUE), st_buffer(obs, dist = 500000)))
#plot(st_geometry(bg), add = TRUE, pch = 16, cex = 0.15)
plot(st_geometry(obs), col = adjustcolor("black", 0.25), pch = 16, cex = 0.25, add = TRUE)
plot(st_geometry(lakes), col = "white", lwd = 0.1, border = adjustcolor("black", 0.5), add = TRUE)

g <- global(crop(p[[vars]], obs, mask = TRUE), mean, na.rm = TRUE)
newdata <- t(g) |> as.data.frame()
newdata <- newdata[rep(1, 500), , drop = FALSE]
v <- seq(-5, 25, length.out = 500)
newdata$mean_annual_air_temperature <- v
plot(v, predict(m, newdata), type = "l")

#partialResponse(m, nr = 2, nsteps = 25)


#predict(m, p, args = c("outputformat=raw", "replicatetype=bootstrap"))



raw1 <- rasterize(obs, aggregate(p, 10), fun = "count", background = 0)
raw2 <- rasterize(bg, aggregate(p, 10), fun = "count", background = 0)
raw <- raw1 / raw2
plot(raw, mar = c(0, 0, 0, 0), axes = FALSE)
plot(st_geometry(na), lwd = 0.5, border = adjustcolor("black", 0.25), add = TRUE)
plot(st_geometry(lakes), col = "white", border = NA, add = TRUE)


mapview(obs)

eo <- rasterize(obs, p, fun = "count", background = 0) |> values()
eb <- rasterize(bg, p, fun = "count", background = 0) |> values()
ep <- values(p[[vars]])

dat <- data.frame(eo, eb, ep)
names(dat)[1:2] <- c("obs", "eff")

dat <- dat[dat$eff > 0, ]

m <- gam(cbind(obs, eff) ~ s(mean_annual_air_temperature, k = 9, m = 2), data = dat, family = binomial)

m <- scam(cbind(obs, eff) ~ s(mean_annual_air_temperature, k = 9, bs = "cv", m = 2), data = dat, family = binomial)

m <- scam(obs ~ s(mean_annual_air_temperature, k = 9, bs = "cv", m = 2) + offset(log(eff)), data = dat, family = poisson)

plot(newdata$mean_annual_air_temperature, predict(m, cbind(newdata, eff = 1000), type = "response"), type = "l")

prez <- predict(m, as.data.frame(cbind(values(p+0), eff = 1000)), type = "response")
ppp <- p[[1]]
#ppp <- setValues(ppp, unname(prez))
ppp[] <- prez
plot(mask(ppp, st_as_sf(st_geometry(na))))

head(dat)


plot(crop(p[["mean_daily_maximum_air_temperature_of_the_warmest_month"]], st_as_sf(na), mask = TRUE))


v <- values(p) |> as.data.frame()
cor(v[, vars])

car::vif(lm(y ~ ., data = cbind(y = 1, v[, vars])))

p$mean_daily_maximum_air_temperature_of_the_warmest_month


x <- gbif |>
  filter(species == !!sp) |>
  filter(!is.na(decimallatitude) & !is.na(decimallatitude)) |>
  mutate(geom = ST_Point(as.numeric(decimallongitude), as.numeric(decimallatitude))) |> 
  to_sf(crs = 4326) |> 
  collect() |>
  st_transform(epsg)








gbif <- gbif_remote(backend = "duckdb")

keep <- c("species", "phylum", "class", "order", "eventdate", "countrycode", "stateprovince", "occurrencestatus","taxonrank","decimallatitude", "decimallongitude", "coordinateuncertaintyinmeters", "occurrencestatus", "institutioncode", "recordedby")


g <- gbif |>
  head(10) |>
  collect()
  filter(phylum == "Chordata", year > 1990) %>%
  count(class, year) %>%
  collect()

g <- gbif |>
      filter(phylum == "Chordata", year > 1990) |>
      count(class, year) |>
      collect()

g <- gbif |>
  filter(species == !!sp) |>
  collect()

g <- gbif |> filter(species == !!sp) |> collect()

Sys.time()
library(gbifdb)
gbif <- gbif_remote(backend = "duckdb")
gbif %>%
  filter(phylum == "Chordata", year > 1990) %>%
  count(class, year) %>%
  collect()
Sys.time()         
         
Sys.time()
library(gbifdb)
gbif <- gbif_remote(backend = "duckdb")
g <- gbif |>
  filter(countrycode %in% c("CA", "US")) |>
  filter(stateprovince %in% c("Québec")) |>
  head() |>
  collect() |>
  as.data.frame()
Sys.time() 

mc_cp("anon/gbif-open-data-us-east-1/index.html", "gbif.html")
mc_ls("anon/gbif-open-data-us-east-1/occurrence/2024-12-01/occurrence.parquet/")
mc_du("anon/gbif-open-data-us-east-1/occurrence/2024-12-01/occurrence.parquet/")
mc_cp("anon/gbif-open-data-us-east-1/occurrence/2024-12-01/occurrence.parquet/000015", "/home/frousseu/Downloads/gbif.parquet/")


gbif <- duckdbfs::open_dataset("/home/frousseu/Downloads/gbif.parquet")
x <- gbif |>
  filter(countrycode %in% c("US", "CA")) |>
  #filter(!class %in% c("Aves")) |>
  count(countrycode) |>
  collect() |>
  _$n |> sum()

x <- gbif |>
  filter(countrycode %in% c("US", "CA")) |>
  filter(!class %in% c("Aves")) |>
  head() |>
  collect() |>
  as.data.table()


keep <- c("species", "phylum", "class", "order", "eventdate", "countrycode", "stateprovince", "occurrencestatus","taxonrank","decimallatitude", "decimallongitude", "coordinateuncertaintyinmeters", "occurrencestatus", "institutioncode", "recordedby")
library(gbifdb)
library(dplyr)
gbif <- gbif_remote(backend = "duckdb")
Sys.time()
g <- gbif |>
  select(any_of(keep)) |>
  filter(countrycode %in% c("CA", "US")) |>
  filter(!class %in% c("Aves")) |>
  collect() 
Sys.time() 


Sys.time()
g <- gbif |>
  filter(phylum == "Hemidactylium scutatum") |>
  collect()
Sys.time()




         
gbif<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/gbif_heatmaps/gbif_plants_density_06-2022.tif")
gbif<-crop(gbif,vect(st_buffer(st_transform(region,4326),1)))
gbif<-mask(gbif,vect(st_buffer(st_transform(region,4326),1)))
gbif<-subst(gbif,NA,0) # replace NAs with 0


lsp<-vector(mode="list",length=length(sp))
names(lsp)<-sp

# https://io.biodiversite-quebec.ca/stac/collections/soilgrids/items

gbif<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/gbif_heatmaps/gbif_plants_density_06-2022.tif")

gbif<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/soilgrids/silt_0-5cm.tif")

rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/earthenv/landcover/consensus_full_class_7.tif")

rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/CHELSA/climatologies/CHELSA_bio5_1981-2010_V.2.1.tif")

r<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/soilgrids/wv1500_30-60cm_mean_1000.tif")

r<-rast("https://object-arbutus.cloud.computecanada.ca/bq-io/io/soilgrids/silt_0-5cm_mean_1000.tif")
r<-crop(r,vect(st_transform(st_buffer(region,100000),st_crs(r))),mask=TRUE)
plot(r,maxcell=1e7)
plot(st_transform(st_geometry(region),st_crs(r)),add=TRUE)

soilgrids/silt_0_5cm



library(duckdb)
library(dplyr)
library(gbifdb)
con <- dbConnect(duckdb())
dbExecute(con,"INSTALL httpfs;LOAD httpfs;")
pfile=paste("s3:/","gbif-open-data-us-east-1","occurrence",gbif_version(),"occurrence.parquet", "*", sep="/")
Sys.time()
dbGetQuery(con, paste0("COPY (SELECT * FROM read_parquet('",pfile,"') WHERE countrycode IN('CAN','US') AND class !='Aves') TO '/home/frousseu/Downloads/test.parquet' (FORMAT 'parquet');"))
Sys.time()





x <- atlas |> 
  mutate(date = paste(year_obs, formatC(month_obs, width = 2, flag = 0), formatC(day_obs, 





set.seed(2)
n <- 200
x1 <- runif(n)*6-3
f1 <- 3*exp(-x1^2 + (1/3)*x1^3) # unconstrained term
x2 <- runif(n)*4-1;
f2 <- exp(4*x2)/(1+exp(4*x2)) # monotone increasing smooth
f <- f1+f2
y <- rpois(n,exp(f))
dat <- data.frame(x1=x1,x2=x2,y=y)
  ## fit model, get results, and plot...
b <- scam(y~s(x1,bs="cv")+s(x2,bs="mpi"),
      family=poisson(link="log"),data=dat,optimizer=c("efs","bfgs"))
summary(b)
plot(b,pages=1,shade=TRUE)
