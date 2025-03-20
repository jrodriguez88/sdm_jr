# Run Main Modelacion ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025

# Load dependencies

#install.packages(c('tidyverse', 'terra', 'dismo', 'rJava', 'geodata', "tidymodels", "car", "naniar","randomForest", "corrplot"))
library(tidyverse)
library(terra)
library(raster)
library(dismo)
library(rJava)
library(geodata)
library(tidymodels)
library(randomForest)
library(gbm)
library(mgcv)
library(caret)
library(pROC)
library(car)
library(sf)
library(naniar)
library(corrplot)
library(spThin)
library(CoordinateCleaner)
library(viridis)
#library(biooracler)
# library(exactextractr)


## Limites Territoriales----
w <- geodata::world(path=tempdir())
w2 <- gadm(country = "COL", path=tempdir())
limites_sel <- w[w$GID_0 == "COL"]
ext_sel <- ext(limites_sel)

study_area <- w %>% crop(ext_sel)
plot(study_area)

descargar_predictores <- FALSE
if(isTRUE(descargar_predictores)){
  dir.create("data/predictors/")
  source("01_Download_SDM_predictors.R")
} else {
  bioclim_predictors <- rast("data/predictors/bioclim_predictors.tiff")
  soil_predictors <- rast("data/predictors/soil_predictors.tiff")
  terrain_predictors <- rast("data/predictors/terrain_predictors.tiff")
  
  all_predictors <- c(bioclim_predictors, soil_predictors, terrain_predictors)
  predictor_names <- names(all_predictors) %>% str_remove_all("wc2.1_30s_")
  all_predictors <- setNames(all_predictors, predictor_names)}


## Especies ----
# Caqueta
cacao <- c("theobroma", "cacao")
canangucha <- c("mauritia", "flexuosa")
copoazu <- c("theobroma", "grandiflorum")

# Choco
coco <- c("cocos", "nucifera")
vainilla <- c("vanilla", "") #"planifolia")

especie <- vainilla


set.seed(2024)
output_dir <- "outputs/"  ; dir.create(output_dir)
tag_spe <- paste0(str_to_title(especie[1]), "_", str_to_title(especie[2]))
output_specie <- paste0(output_dir, tag_spe, "/") ; dir.create(output_specie)

# Descarga y limpieza
source("02_Ocurrence_data.R")
datos_gbif_crop
data_cleaned
## VIF and background

vif_threshold = 15
# Spearman correlation 
#corr_threshold = 0.90
#buffer
threshold_distance <- 20000
pAusencias_times <- 3
source("03_VIF_and_background.R")
pred_select
plot(p)


# Modeling SDM
source("04_Modeling_SDM.R")




