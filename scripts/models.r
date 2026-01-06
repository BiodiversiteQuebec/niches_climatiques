### Maxent parameters!
# https://github.com/mrmaxent/Maxent/blob/master/density/parameters.csv


echelle <- c("large", "small")[as.integer(grepl("small", names(models)[i])) + 1]

if(is.character(models[[i]])){

    vars <- models[[i]]
    vars <- intersect(vars, species_vars[[sp]])
    vars <- intersect(names(p[[echelle]]), vars)
    if(grepl("climat|gam", names(models)[i])){
        vars <- c(vars, climate_vars)
    }
    
    print(paste("Variables used:", paste(vars, collapse = " ")))

    if(!grepl("gam", names(models)[i])){

        if(names(models)[i] == "climat"){
            arg <- c("linear", "quadratic", "noproduct", "nohinge", "nothreshold", "noautofeature", "replicatetype=bootstrap", "replicates=1", "threads=4", "betaMultiplier=0")
        } else {
            arg <- c("linear", "quadratic", "noproduct", "nohinge", "nothreshold", "noautofeature", "replicatetype=bootstrap", "replicates=1", "threads=4")
        }


        m <- MaxEnt(p[[echelle]][[vars]], 
                    p = vect(obs[[echelle]]), a = vect(bg[[echelle]]),#[sample(1:nrow(bg[[echelle]]), nrow(obs[[echelle]])),]),
                    removeDuplicates = TRUE,
                    silent = FALSE,
                    args = arg
        )

    } else {
        
        gamfamily <- c("poisson", "binomial")[2]
        
        if(gamfamily == "poisson"){
            ppp <- aggregate(p[[echelle]][[vars]], 2)
            eo <- rasterize(obs[[echelle]], ppp, fun = "count", background = 0) |> values()
            eb <- rasterize(bg[[echelle]], ppp, fun = "count", background = 0) |> values()
            ep <- values(ppp[[vars]])

            dat <- data.frame(eo, eb, ep)
            names(dat)[1:2] <- c("obs", "eff")

            dat <- dat[dat$eff > 0, ]

            f <- paste("obs ~", paste("s(", vars, ", k = 22, bs = \"cv\", m = 2) + offset(log(eff))")) |>
            as.formula()

            #optimizer <- c("bfgs", "newton")#c("efs", "bfgs")
            #optimizer <- c("efs", "bfgs")
            #m <- scam(f, optimizer = optimizer, data = dat, family = poisson)
            m <- scam(f, data = dat, family = poisson)
        } else {
            ppp <- aggregate(p[[echelle]][[vars]], 2)
            eo <- rasterize(obs[[echelle]], ppp, fun = "count", background = 0)
            eb <- rasterize(bg[[echelle]], ppp, fun = "count", background = 0)
            eo <- ifel(eo > 0, 1, 0) |> values()
            eb <- ifel(eb > 0, 1, 0) |> values()
            ep <- values(ppp[[vars]])

            dat <- data.frame(eo, eb, ep)
            names(dat)[1:2] <- c("obs", "eb")

            dat <- dat[dat$eb > 0, ]

            f <- paste("obs ~", paste("s(", vars, ", k = 20, bs = \"cv\", m = 2)")) |>
            as.formula()

            #optimizer <- c("bfgs", "newton")#c("efs", "bfgs")
            optimizer <- c("efs", "bfgs")
            m <- scam(f, optimizer = optimizer, data = dat, family = binomial)
            #m <- scam(f, data = dat, family = binomial)

        }
        
    }
    





}


#r <- rast("results/rasters/Gyrinophilus_porphyriticus_sdm.tif")




if(FALSE){

    g <- global(crop(p[[echelle]][[vars]], obs$large, mask = TRUE), mean, na.rm = TRUE)
    newdata <- t(g) |> as.data.frame()
    newdata <- newdata[rep(1, 500), , drop = FALSE]
    v <- seq(-25, 25, length.out = 500)
    newdata$mean_annual_air_temperature <- v
    pm <- predict(m, newdata)
    plot(v, pm, type = "l")


    ppp <- aggregate(p$large, 20)
    eo <- rasterize(obs$large, ppp, fun = "count", background = 0) |> values()
    eb <- rasterize(bg$large, ppp, fun = "count", background = 0) |> values()
    ep <- values(ppp[[vars]])

    dat <- data.frame(eo, eb, ep)
    names(dat)[1:2] <- c("obs", "eff")

    dat <- dat[dat$eff > 0, ]

    #ms <- gam(cbind(obs, eff) ~ s(mean_annual_air_temperature, k = 19, m = 1), data = dat, family = binomial)

    #ms <- scam(cbind(obs, eff) ~ s(mean_annual_air_temperature, k = 45, bs = "cv", m = 2), data = dat, family = binomial)

    optimizer <- c("bfgs", "newton")#c("efs", "bfgs")
    ms <- scam(obs ~ s(mean_annual_air_temperature, k = 12, bs = "cv", m = 2) + offset(log(eff)), optimizer = optimizer, data = dat, family = poisson)

    #plot(newdata$mean_annual_air_temperature, predict(ms, cbind(newdata, eff = 1000), type = "response"), type = "l")

    pms <- predict(ms, cbind(newdata, eff = 1000), type = "response")
    pms <- scales::rescale(pms, to = c(0, max(pm, na.rm = TRUE)))
    lines(newdata$mean_annual_air_temperature, pms, col = "blue")

    prez <- predict(ms, as.data.frame(cbind(values(p[[echelle]]+0), eff = 1000)), type = "response")
    pppp <- p[[echelle]][[1]]
    #ppp <- setValues(ppp, unname(prez))
    pppp[] <- prez
    plot(mask(pppp, st_as_sf(st_geometry(na))))
    plot(st_geometry(obs$large), cex = 0.25, add = TRUE)

}


#png("raw.png", width = 9, height = 9, units = "in", res = 300)
#plot(trim(aggregate(eo, 20, fun = "sum")/aggregate(eb, 20, fun = "sum")))
#plot(st_geometry(na), add = TRUE)
#dev.off()