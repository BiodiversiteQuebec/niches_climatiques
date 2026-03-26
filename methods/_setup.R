
library(tidyr)
library(dplyr)
library(knitr)
library(kableExtra)
library(scam)
library(gt)
library(rstac)
library(flextable)


set_flextable_defaults(
  font.size = 8, 
  border.color = "grey70"
)

pdfw <- 8.5 - (2 * 1.25)

spnames <- list(list("Pseudacris triseriata", "Rainette faux-grillon de l'Ouest", "Rainettes"), list("Hemidactylium scutatum", 
    "Salamandre à quatre orteils", "Salamandres"), list("Gyrinophilus porphyriticus", "Salamandre pourpre", "Salamandres"), list("Desmognathus ochrophaeus", 
    "Salamandre sombre des montagnes", "Salamandres"), list("Emydoidea blandingii", "Tortue mouchetée", "Tortues"), list("Glyptemys insculpta", 
    "Tortue des bois", "Tortues"), list("Nerodia sipedon", "Couleuvre d'eau", "Couleuvres"), list("Lampropeltis triangulum", 
    "Couleuvre tachetée", "Couleuvres"), list("Aquila chrysaetos", "Aigle royal", "Oiseaux"), list("Catharus bicknelli", 
    "Grive de Bicknell", "Oiseaux"), list("Setophaga cerulea", "Paruline azurée", "Oiseaux"), list("Coturnicops noveboracensis", 
    "Râle jaune", "Oiseaux"), list("Ixobrychus exilis", "Petit Blongios", "Oiseaux"), list("Glaucomys volans", 
    "Petit Polatouche", "Mammifères"))

splatin <- sapply(spnames, "[[", 1)
spfr <- sapply(spnames, "[[", 2)
spgroup <- sapply(spnames, "[[", 3) |>
  factor(levels = c("Rainettes", "Salamandres", "Couleuvres", "Tortues", "Oiseaux", "Mammifères"))
