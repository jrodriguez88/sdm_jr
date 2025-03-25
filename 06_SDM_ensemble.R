# Ensemble build


# umbrales óptimos calculados para cada modelo
umbral_gam    <- eval_results$threshold[1] # Este valor es ilustrativo; reemplázalo con el obtenido
umbral_gbm    <- eval_results$threshold[2] 
umbral_maxent <- eval_results$threshold[3] 
umbral_rf     <- eval_results$threshold[4]  

# Convertir a binario: 1 si la predicción es mayor o igual al umbral, 0 en caso contrario.
binary_gam    <- gam_pred >= umbral_gam
binary_gbm    <- gbm_pred >= umbral_gbm
binary_maxent <- maxent_pred >= umbral_maxent
binary_rf     <- rf_pred >= umbral_rf

# SSP245
binary_gam_ssp245    <- gam_pred_ssp245 >= umbral_gam
binary_gbm_ssp245    <- gbm_pred_ssp245 >= umbral_gbm
binary_maxent_ssp245 <- maxent_pred_ssp245 >= umbral_maxent
binary_rf_ssp245     <- rf_pred_ssp245 >= umbral_rf

# SSP 585
binary_gam_ssp585    <- gam_pred_ssp585 >= umbral_gam
binary_gbm_ssp585    <- gbm_pred_ssp585 >= umbral_gbm
binary_maxent_ssp585 <- maxent_pred_ssp585 >= umbral_maxent
binary_rf_ssp585     <- rf_pred_ssp585 >= umbral_rf


# Crear una lista o un stack con los raster binarios (para el periodo base, por ejemplo)
binary_list <- rast(list(binary_gam, binary_gbm, binary_maxent, binary_rf))

# Usar los pesos basados en AUC (recuerda que estos pesos se calculan como (AUC - 0.5)^2)
w <- (eval_results$AUC - 0.5)^2

# Realizar el promedio ponderado (Committee Averaging)
ensemble_continuous <- weighted.mean(binary_list, w)
# Reclasificar el ensemble a binario: 
# Se define 1 para celdas con acuerdo ≥ 75% y 0 para el resto.
ensemble_binary <- ensemble_continuous >= 0.75

# # Calculate evaluation metrics
# pred_ensemble <- terra::extract(ensemble_continuous,
#                                 data.frame(x= test_data$x, y = test_data$y), xy = T) %>%
#   dplyr::select(x, y, sim = 2) %>% mutate(obs = test_data$presence)
# 
# roc_obj <- roc(pred_ensemble$obs, round(pred_ensemble$sim))
# auc_val <- auc(roc_obj)
# auc_val
#
#pred_class <- factor(pred_class, levels = c(1,0))
#test_df$presence <- factor(test_df$presence, levels = c(1,0))
# conf_matrix_ens <- confusionMatrix(factor(round(pred_ensemble$sim)), factor(pred_ensemble$obs), positive = "1")
# plot(roc_obj, main = paste0("Ensemble model", " - AUC:", round(auc_val, 3)))

# Repite el mismo procedimiento para las proyecciones futuras (ssp245), usando los raster binarios correspondientes
binary_list_ssp245 <- rast(list(binary_gam_ssp245, binary_gbm_ssp245, binary_maxent_ssp245, binary_rf_ssp245))
ensemble_continuous_ssp245 <- weighted.mean(binary_list_ssp245, w)
ensemble_binary_ssp245 <- ensemble_continuous_ssp245 >= 0.75

# Repite el mismo procedimiento para las proyecciones futuras (ssp585), usando los raster binarios correspondientes
binary_list_ssp585 <- rast(list(binary_gam_ssp585, binary_gbm_ssp585, binary_maxent_ssp585, binary_rf_ssp585))
ensemble_continuous_ssp585 <- weighted.mean(binary_list_ssp585, w)
ensemble_binary_ssp585 <- ensemble_continuous_ssp585 >= 0.75


## Guardar Raster

writeRaster(c(ensemble_continuous, ensemble_continuous_ssp245, ensemble_continuous_ssp585) %>% 
              setNames(c("base", "ssp245", "ssp585")), 
            filename = paste0(output_specie, tag_spe, "_ensembles_continuos_2040_2060.tif"), overwrite = TRUE)

writeRaster(c(ensemble_binary, ensemble_binary_ssp245, ensemble_binary_ssp585) %>% 
              setNames(c("base", "ssp245", "ssp585")), 
            filename = paste0(output_specie, tag_spe, "_ensembles_binarios_2040_2060.tif"), overwrite = TRUE)




## Mapas binarios
jpeg(paste0(output_specie, tag_spe, "_SDM_Binary_Map.jpg"),
     width = 8.3, height = 11.7, units="in", res=300)

par(mfrow = c(2, 2))
plot(binary_gam, main = paste0("GAM Binario \nUmbral: ", umbral_gam),
     col = c("white", "darkgreen"))
plot(study_area, add=TRUE, fill=NA)
plot(binary_gbm, main = paste0("BRT Binario \nUmbral: ", umbral_gbm),
     col = c("white", "darkgreen"))
plot(study_area, add=TRUE, fill=NA)
plot(binary_maxent, main = paste0("MaxEnt Binario \nUmbral: ", umbral_maxent),
     col = c("white", "darkgreen"))
plot(study_area, add=TRUE, fill=NA)
plot(binary_rf, main = paste0("Random Forest Binario \nUmbral: ", umbral_rf),
     col = c("white", "darkgreen"))
plot(study_area, add=TRUE, fill=NA)
dev.off()



# Plot the binary raster using white for 0 and darkgreen for 1
jpeg(paste0(output_specie, tag_spe, "_Ensemble_Map.jpg"),
     width = 11.7, height = 8.3, units="in", res=300)
par(mfrow = c(1,2))
plot(ensemble_continuous, main = str_to_title(paste0(tag_spe, " - ", "Ensemble Model Distribution")))
points(points_cleaned, col = "red")
plot(ensemble_binary, col = c("white", "darkgreen"), 
     main = paste0("Binary Ensemble Prediction: 3 votes (75%)"))
points(points_cleaned, col = "red")
plot(study_area ,add=TRUE, fill=NA)
dev.off()
