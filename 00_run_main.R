# Run Main Modelacion ABRIGUE
## Autor: Rodriguez-Espinoza J.
## github.com/jrodriguez88
## Febrero 2025

# Load dependencies

#install.packages(c('tidyverse', 'terra', 'dismo', 'rJava', 'geodata', "tidymodels", "car", "naniar","randomForest", "corrplot"))
options(java.parameters = "-Xmx16g")

library(tidyverse)
library(terra)
library(raster)
library(dismo)
library(usdm)
library(rJava)
library(geodata)
# library(tidymodels)
library(randomForest)
library(gbm)
library(mgcv)
library(caret)
library(pROC)
library(car)
library(sf)
# library(naniar)
library(corrplot)
library(spThin)
library(CoordinateCleaner)
library(viridis)
#library(biooracler)
# library(exactextractr)
library(parallel)
library(gridBase)
library(grid)


## Limites Territoriales----
w <- geodata::world(path=tempdir())
w2 <- gadm(country = "COL", path=tempdir())
limites_sel <- w[w$GID_0 == "COL"]
ext_sel <- ext(limites_sel)

study_area <- w %>% crop(ext_sel)
plot(study_area)
predictors_path <- paste0(getwd(), "/data/predictors/")

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


## Modeling
set.seed(2024)

especies_list <- list(cacao, canangucha, copoazu, coco, vainilla)
#especies_list <- list(vainilla)

# Global args
corr_threshold = 0.9
vif_threshold = 15
buffer_presence <- 20000
pseudo_ausencia_mult <- 3
path <- getwd()

tictoc::tic()
for (especie in seq_along(especies_list)){
  
  especie <- especies_list[[especie]]
  
  output_dir <- paste0(path, "/outputs3/")  ; dir.create(output_dir)
  tag_spe <- paste0(str_to_title(especie[1]), "_", str_to_title(especie[2]))
  output_specie <- paste0(output_dir, tag_spe, "/") ; dir.create(output_specie)
  
  
  source("02_Ocurrence_data.R") # Descarga y limpieza
  source("03_VIF_and_background.R")  # VF and Background
  source("04_SDM_training_eval.R")
  source("05_SDM_predict_baseline.R")
  source("05_SDM_predict_ssp245.R")
  source("05_SDM_predict_ssp585.R")
  source("06_SDM_ensemble.R")
  source("06_Delta_maps.R")
  # source("04_Modeling_SDM.R") # SDM models an evals
  
}
tictoc::toc()

  





