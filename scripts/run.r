

source("scripts/prelim.r")
source("scripts/predictors.r")
source("scripts/parameters.r")


for(sp in species[3]){#[-c(9,10,11,13)]){
    print(sp)
    source("scripts/data.r")
    source("scripts/background.r")
    source("scripts/variables.r")
    source("scripts/models.r")
    source("scripts/predictions.r")
    source("scripts/results.r")
}








