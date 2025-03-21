

m <- MaxEnt(p[[vars]], 
            #vect(d[d$presence==1,]),vect(d[d$presence==0,]),
            p = vect(obs), a = vect(bg),
            removeDuplicates = FALSE,
            silent = FALSE,
            #args=c("replicatetype=bootstrap","replicates=1","threads=4")
            #args=c("linear","quadratic","product","hinge","nothreshold","replicatetype=bootstrap","replicates=1","threads=4")
            args = c("linear", "quadratic", "noproduct","nohinge", "threshold", "noautofeature","replicatetype=bootstrap","replicates=1", "threads=4"
            )
)