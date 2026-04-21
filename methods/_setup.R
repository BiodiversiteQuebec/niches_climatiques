
library(terra) 
library(sf)
library(geodata)
library(rmapshaper)
library(dplyr)
library(rnaturalearth)
library(sdmtools)
library(tidyr)
library(knitr)
library(kableExtra)
library(scam)
library(gt)
library(rstac)
library(flextable)
library(officer)


set_flextable_defaults(
  font.size = 8, 
  border.color = "black"
)

### Table "theme" for latex
theme_pdf <- function(ft) {
  header_border <- fp_border(width = 0.75, color = "black")#, style = "dotted")
  body_border <- fp_border(width = 0.25, color = "black")#, style = "dotted")
  ft |>
    fontsize(size = 8, part = "all") |>
    border_remove() |>
    hline_top(part = "header", border = header_border) |>
    hline_bottom(part = "header", border = header_border) |>
    hline(part = "body", border = body_border) |>
    hline_bottom(part = "body", border = header_border) |>  
    fix_border_issues(part = "all") |>
    height(height = 0.05, part = "body") |>
    hrule(rule = "exact", part = "body")
}

pdfw <- 8.5 - (2 * 1.25)

epsg <- 6624 #32618

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
