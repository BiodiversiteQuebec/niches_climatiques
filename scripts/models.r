### Maxent parameters!
# https://github.com/mrmaxent/Maxent/blob/master/density/parameters.csv

if(is.character(models[[i]])){

    vars <- models[[i]]

    m <- MaxEnt(p[[vars]], 
                p = vect(obs), a = vect(bg),
                removeDuplicates = FALSE,
                silent = FALSE,
                args = c("linear", "quadratic", "noproduct", "nohinge", "nothreshold", "noautofeature", "replicatetype=bootstrap", "replicates=1", "threads=4")
    )

}


g <- global(crop(p[[vars]], obs, mask = TRUE), mean, na.rm = TRUE)
newdata <- t(g) |> as.data.frame()
newdata <- newdata[rep(1, 500), , drop = FALSE]
v <- seq(-25, 25, length.out = 500)
newdata$mean_annual_air_temperature <- v
pm <- predict(m, newdata)
plot(v, pm, type = "l")


ppp <- aggregate(p, 2)
eo <- rasterize(obs, ppp, fun = "count", background = 0) |> values()
eb <- rasterize(bg, ppp, fun = "count", background = 0) |> values()
ep <- values(ppp[[vars]])

dat <- data.frame(eo, eb, ep)
names(dat)[1:2] <- c("obs", "eff")

dat <- dat[dat$eff > 0, ]

#ms <- gam(cbind(obs, eff) ~ s(mean_annual_air_temperature, k = 19, m = 1), data = dat, family = binomial)

#ms <- scam(cbind(obs, eff) ~ s(mean_annual_air_temperature, k = 45, bs = "cv", m = 2), data = dat, family = binomial)

optimizer <- c("bfgs", "newton")#c("efs", "bfgs")
ms <- scam(obs ~ s(mean_annual_air_temperature, k = 25, bs = "cv", m = 2) + offset(log(eff)), optimizer = optimizer, data = dat, family = poisson)

#plot(newdata$mean_annual_air_temperature, predict(ms, cbind(newdata, eff = 1000), type = "response"), type = "l")

pms <- predict(ms, cbind(newdata, eff = 1000), type = "response")
pms <- scales::rescale(pms, to = c(0, max(pm, na.rm = TRUE)))
lines(newdata$mean_annual_air_temperature, pms, col = "blue")

prez <- predict(ms, as.data.frame(cbind(values(p+0), eff = 1000)), type = "response")
ppp <- p[[1]]
#ppp <- setValues(ppp, unname(prez))
ppp[] <- prez
plot(mask(ppp, st_as_sf(st_geometry(na))))
plot(st_geometry(obs), cex = 0.25, add = TRUE)


