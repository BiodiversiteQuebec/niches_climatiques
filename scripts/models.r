### Maxent parameters!
# https://github.com/mrmaxent/Maxent/blob/master/density/parameters.csv


echelle <- c("large", "small")[as.integer(grepl("small", names(models)[i])) + 1]

if(is.character(models[[i]])){

    vars <- models[[i]]

    if(models[[i]] != "gam"){

        m <- MaxEnt(p[[echelle]][[vars]], 
                    p = vect(obs[[echelle]]), a = vect(bg[[echelle]]),
                    removeDuplicates = FALSE,
                    silent = FALSE,
                    args = c("linear", "quadratic", "noproduct", "nohinge", "nothreshold", "noautofeature", "replicatetype=bootstrap", "replicates=1", "threads=4")
        )

    } else {

        ppp <- aggregate(p[[echelle]][[vars]], 20)
        eo <- rasterize(obs[[echelle]], ppp, fun = "count", background = 0) |> values()
        eb <- rasterize(bg[[echelle]], ppp, fun = "count", background = 0) |> values()
        ep <- values(ppp[[vars]])

        dat <- data.frame(eo, eb, ep)
        names(dat)[1:2] <- c("obs", "eff")

        dat <- dat[dat$eff > 0, ]

        f <- paste("obs ~", paste("s(", vars, ", k = 9, bs = \"cv\", m = 2) + offset(log(eff))")) |>
          as.formula()

        optimizer <- c("bfgs", "newton")#c("efs", "bfgs")
        m <- scam(f, optimizer = optimizer, data = dat, family = poisson)
        




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
