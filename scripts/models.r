### Maxent parameters!
# https://github.com/mrmaxent/Maxent/blob/master/density/parameters.csv

if(is.character(models[[i]])){

    vars <- models[[i]]

    m <- MaxEnt(p[[vars]], 
                p = vect(obs), a = vect(bg),
                removeDuplicates = FALSE,
                silent = FALSE,
                args = c("linear", "quadratic", "noproduct", "nohinge", "nothreshold", "noautofeature", "replicatetype=bootstrap", "replicates=1", "threads=4"
                )
    )

}