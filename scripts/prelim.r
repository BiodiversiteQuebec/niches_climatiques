


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
library(scam)
library(FRutils)

#source("https://raw.githubusercontent.com/frousseu/FRutils/refs/heads/master/R/colo.scale.R")

#options(terra.pal=rev(map.pal("grass")))
options(terra.pal=rev(terrain.colors(200)))
terraOptions(tempdir = "/home/frousseu/data2/tmp", memfrac = 0.8)

epsg <- 6624 #32618

# Downloads polygons using package geodata
can <- gadm("CAN", level = 1, path = "data") |> st_as_sf()
usa <- gadm("USA", level = 1, path = "data") |> st_as_sf()
na <- rbind(can,usa)
na <- st_transform(na, epsg)
na <- na[!na$NAME_1 %in% c("Hawaii", "Alaska"), ]

# keep Québec and bordering provinces/states as a buffer
#region <- na[na$NAME_1 %in% c("Québec", "New Brunswick", "Maine", "Vermont", "New Hampshire", "New York", "Ontario", "Nova Scotia", "Prince Edward Island", "Massachusetts", "Connecticut", "Rhode Island"),]
region <- na[!na$NAME_1 %in% c("Yukon", "British Columbia", "Washington", "Oregon", "California", "Arizona", "Nevada", "Idaho", "Utah"), ]

#region <- na[na$NAME_1 %in% c("Québec"), ]

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
lakes<-ne_download(scale="medium",type="lakes",destdir="data",category="physical",returnclass="sf") |> st_transform(epsg)
lakes<-st_filter(lakes,region)
