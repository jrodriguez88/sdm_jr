# Run Main Modelacion ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025

# Load dependencies

install.packages(c('tidyverse', 'terra', 'dismo', 'rJava', 'geodata', "tidymodels", "car", "naniar","randomForest"))
library(tidyverse)
library(terra)
library(raster)
library(dismo)
library(rJava)
library(geodata)
library(tidymodels)
library(randomForest)
library(car)
library(sf)
library(naniar)
#library(biooracler)


descargar_predictores <- FALSE
if(isTRUE(descargar_predictores)){
  dir.create("data/predictors/")
  source("01_Download_SDM_predictors.R")
} else {
  all_predictors <- rast("data/predictors/all_predictors.tiff")}


## Limites Territoriales----
w <- world(path=tempdir())
w2 <- gadm(country = "COL", path=tempdir())
limites_sel <- w[w$GID_0 == "COL"]
ext_sel <- ext(limites_sel)


## Especies ----
# Caqueta
cacao <- c("theobroma", "cacao")
canangucha <- c("mauritia", "flexuosa")
copoazu <- c("theobroma", "grandiflorum")

# Choco
coco <- c("cocos", "nucifera")
vainilla <- c("vanilla", "planifolia")

especie <- cacao


set.seed(2024)

# Descarga y limpieza
source("02_Ocurrence_data.R")

par(mfrow = c(1,1))
plot(all_predictors[["elevation"]], 
     main = paste0("Ocurrencia Especies GBIF - ", 
                   tag_esp), col = map.pal("elevation"))
lines(limites_sel)
points(datos_gbif_crop, col='red', cex= 1)
legend("bottom", #x = -73.5, y = 3, # Coordinates
       legend = tag_esp,
       col = c('red'))



## VIF and background

source("03_VIF_and_background.R")
pred_select





