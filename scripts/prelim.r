


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
library(sdmtools)

#source("https://raw.githubusercontent.com/frousseu/FRutils/refs/heads/master/R/colo.scale.R")

#options(terra.pal=rev(map.pal("grass")))
options(terra.pal = rev(terrain.colors(200)))
#terraOptions(tempdir = "/home/frousseu/data2/tmp", memfrac = 0.8)

epsg <- 6624 #32618

# Downloads polygons using package geodata
#can <- gadm("CAN", level = 1, path = "data") |> st_as_sf()
#usa <- gadm("USA", level = 1, path = "data") |> st_as_sf()
can <- readRDS("data/gadm/gadm41_CAN_2_pk.rds") |> st_as_sf()
usa <- readRDS("data/gadm/gadm41_USA_2_pk.rds") |> st_as_sf()
na <- rbind(can, usa)
na <- st_transform(na, epsg)
na <- na[!na$NAME_1 %in% c("Hawaii"), ]
na <- na[!na$NAME_2 %in% c("Aleutians West", "Aleutians East"), ]
na <- na |>
  group_by(NAME_1) |>
  summarise(geometry = st_union(geometry)) |>
  as.data.frame() |>
  st_as_sf(sf_column_name = "geometry")

# keep Québec and bordering provinces/states as a buffer
#region <- na[na$NAME_1 %in% c("Québec", "New Brunswick", "Maine", "Vermont", "New Hampshire", "New York", "Ontario", "Nova Scotia", "Prince Edward Island", "Massachusetts", "Connecticut", "Rhode Island"),]
#region <- na[!na$NAME_1 %in% c("Yukon", "British Columbia", "Washington", "Oregon", "California", "Arizona", "Nevada", "Idaho", "Utah"), ]

region <- na

#region <- na[na$NAME_1 %in% c("Québec"), ]

# split NF into different polygons
labrador <- ms_explode(na[na$NAME_1 %in% c("Newfoundland and Labrador"), ]) 
labrador <- labrador[which.max(st_area(labrador)), ] # keep Labarador
#region <- rbind(region, labrador)
qc <- na[na$NAME_1 %in% c("Québec"),] |>
        #rbind(labrador) |>
        ms_simplify(0.01)

na <- ms_simplify(na, 0.01) |> st_make_valid()

# Add it to the study region
#region <- rbind(region, labrador) 

# Simplify polygons to make things faster
region <- ms_simplify(region, 0.01) |> st_make_valid()
region <- st_union(region) |> st_as_sf()

# lakes
#lakes<-ne_download(scale="medium",type="lakes",destdir="data",category="physical",returnclass="sf") |> st_transform(epsg)
lakes <- st_read("data/ne_50m_lakes.shp") |> st_transform(epsg)
lakes <- st_filter(lakes, region)

aires <- st_read("data/vertébrés.gpkg") 

### corrections to the dismo::evaluate function to allow smaller values in the thresholding
evaluate2 <- function (p, a, model, x, tr, ...) 
{
    if (!missing(x)) {
        p <- predict(model, data.frame(extract(x, p)), ...)
        a <- predict(model, data.frame(extract(x, a)), ...)
    }
    else if (is.vector(p) & is.vector(a)) {
    }
    else {
        p <- predict(model, data.frame(p), ...)
        a <- predict(model, data.frame(a), ...)
    }
    p <- stats::na.omit(p)
    a <- stats::na.omit(a)
    np <- length(p)
    na <- length(a)
    if (na == 0 | np == 0) {
        stop("cannot evaluate a model without absence and presence data that are not NA")
    }
    if (missing(tr)) {
        if (length(p) > 1000) {
            tr <- as.vector(quantile(p, 0:1000/1000))
        }
        else {
            tr <- p
        }
        if (length(a) > 1000) {
            tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
        }
        else {
            tr <- c(tr, a)
        }
        tr <- sort(unique(round(tr, 20)))
        tr <- c(tr - 0.0000000000000001, tr[length(tr)] + c(0, 0.0000000000000001))
    }
    else {
        tr <- sort(as.vector(tr))
    }
    N <- na + np
    xc <- new("ModelEvaluation")
    xc@presence = p
    xc@absence = a
    R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
    xc@auc <- R/(as.numeric(na) * as.numeric(np))
    cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), 
        silent = TRUE)
    if (!inherits(cr, "try-error")) {
        xc@cor <- cr$estimate
        xc@pcor <- cr$p.value
    }
    res <- matrix(ncol = 4, nrow = length(tr))
    colnames(res) <- c("tp", "fp", "fn", "tn")
    xc@t <- tr
    for (i in 1:length(tr)) {
        res[i, 1] <- length(p[p >= tr[i]])
        res[i, 2] <- length(a[a >= tr[i]])
        res[i, 3] <- length(p[p < tr[i]])
        res[i, 4] <- length(a[a < tr[i]])
    }
    xc@confusion = res
    a = res[, 1]
    b = res[, 2]
    c = res[, 3]
    d = res[, 4]
    xc@np <- as.integer(np)
    xc@na <- as.integer(na)
    xc@prevalence = (a + c)/N
    xc@ODP = (b + d)/N
    xc@CCR = (a + d)/N
    xc@TPR = a/(a + c)
    xc@TNR = d/(b + d)
    xc@FPR = b/(b + d)
    xc@FNR = c/(a + c)
    xc@PPP = a/(a + b)
    xc@NPP = d/(c + d)
    xc@MCR = (b + c)/N
    xc@OR = (a * d)/(c * b)
    prA = (a + d)/N
    prY = (a + b)/N * (a + c)/N
    prN = (c + d)/N * (b + d)/N
    prE = prY + prN
    xc@kappa = (prA - prE)/(1 - prE)
    return(xc)
}


pa_evaluate2 <- function (p, a, model = NULL, x = NULL, tr, ...) 
{
    if (!is.null(model)) {
        if (is.null(x)) {
            p <- predict(model, p, ...)
            a <- predict(model, a, ...)
        }
        else {
            p <- terra::extract(x, p)
            p <- predict(model, p, ...)
            a <- terra::extract(x, a)
            a <- predict(model, a, ...)
        }
    }
    p <- as.numeric(stats::na.omit(p))
    a <- as.numeric(stats::na.omit(a))
    np <- length(p)
    na <- length(a)
    if (na == 0 | np == 0) {
        stop("cannot evaluate a model without absence and presence data that are not NA")
    }
    if (missing(tr)) {
        if (length(p) > 1000) {
            tr <- as.vector(stats::quantile(p, 0:1000/1000))
        }
        else {
            tr <- p
        }
        if (length(a) > 1000) {
            tr <- c(tr, as.vector(stats::quantile(a, 0:1000/1000)))
        }
        else {
            tr <- c(tr, a)
        }
        tr <- sort(unique(round(tr, 16)))
        tr <- c(tr - 1e-17, tr[length(tr)] + c(0, 1e-17))
    }
    else {
        tr <- sort(as.vector(tr))
    }
    N <- na + np
    xc <- methods::new("paModelEvaluation")
    xc@presence = p
    xc@absence = a
    R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
    auc <- R/(as.numeric(na) * as.numeric(np))
    cr <- try(stats::cor.test(c(p, a), c(rep(1, length(p)), rep(0, 
        length(a)))), silent = TRUE)
    corc <- pcor <- NA
    if (!inherits(cr, "try-error")) {
        corc <- cr$estimate
        pcor <- cr$p.value
    }
    res <- matrix(ncol = 4, nrow = length(tr))
    colnames(res) <- c("tp", "fp", "fn", "tn")
    for (i in 1:length(tr)) {
        res[i, 1] <- length(p[p >= tr[i]])
        res[i, 2] <- length(a[a >= tr[i]])
        res[i, 3] <- length(p[p < tr[i]])
        res[i, 4] <- length(a[a < tr[i]])
    }
    xc@confusion = res
    a = res[, 1]
    b = res[, 2]
    c = res[, 3]
    d = res[, 4]
    np <- as.integer(np)
    na <- as.integer(na)
    prevalence = (a[1] + c[1])/N
    ODP = (b[1] + d[1])/N
    xc@stats <- data.frame(np, na, prevalence, auc, cor = corc, 
        pcor, ODP)
    rownames(xc@stats) <- NULL
    CCR = (a + d)/N
    TPR = a/(a + c)
    TNR = d/(b + d)
    FPR = b/(b + d)
    FNR = c/(a + c)
    PPP = a/(a + b)
    NPP = d/(c + d)
    MCR = (b + c)/N
    OR = (a * d)/(c * b)
    prA = (a + d)/N
    prY = (a + b)/N * (a + c)/N
    prN = (c + d)/N * (b + d)/N
    prE = prY + prN
    kappa = (prA - prE)/(1 - prE)
    xc@tr_stats <- data.frame(treshold = tr, kappa, CCR, TPR, 
        TNR, FPR, FNR, PPP, NPP, MCR, OR)
    max_kappa <- tr[which.max(kappa)]
    max_spec_sens <- tr[which.max(TPR + TNR)]
    no_omission <- tr[max(which(res[, "fn"] == 0))]
    equal_prevalence = tr[which.min(abs(tr - prevalence))]
    equal_sens_spec <- tr[which.min(abs(TPR - TNR))]
    xc@thresholds <- data.frame(max_kappa, max_spec_sens, no_omission, 
        equal_prevalence, equal_sens_spec)
    return(xc)
}
