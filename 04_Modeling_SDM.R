# 3. Split data into train/test -------------------------------------------
train_index <- createDataPartition(full_data$presence, p = 0.7, list = FALSE)
train_data <- full_data[train_index, ]
test_data <- full_data[-train_index, ]

# 4. Define predictor names ----------------------------------------------
predictor_names <- names(predictors)


# 5. Model training ------------------------------------------------------
# GAM
gam_formula <- as.formula(
  paste("presence ~", paste0("s(", predictor_names, ", k = 4)", collapse = " + "))
)
gam_model <- gam(gam_formula, data = train_data, family = binomial)

# BRT (GBM)
gbm_model <- gbm(
  formula = presence ~ .,
  data = train_data[, c("presence", predictor_names)],
  distribution = "bernoulli",
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5,
  verbose = FALSE
)

# Optimize number of trees
best_trees <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)

# MaxEnt
maxent_model <- maxent(
  x = train_data[, predictor_names], 
  p = train_data$presence
)

# response(maxent_model)

# Random Forest
rf_model <- randomForest(
  x = train_data[, predictor_names],
  y = as.factor(train_data$presence),
  ntree = 500,
  importance = TRUE
)

png(paste0(output_specie, tag_spe, "_Importancia_predictores_plot.png"),
    width = 11.7, height = 8, units="in", res=300)
par(mfrow=c(1,2))
plot(maxent_model, main = "MaxEnt - Contribucion de Variables")
barplot(importance(rf_model) %>% .[order(.[, "MeanDecreaseGini"]), "MeanDecreaseGini"],
        horiz = TRUE,
        col = "steelblue",
        main = "Importancia de Variables (Random Forest)",
        xlab = "Mean Decrease Gini",
        las = 1)
dev.off()

# 6. Model evaluation ----------------------------------------------------
evaluate_model <- function(model, test_df, model_type) {
  pred <- switch(model_type,
                 "GAM" = predict(model, test_df, type = "response"),
                 "BRT" = predict(model, test_df, n.trees = best_trees, type = "response"),
                 "MaxEnt" = predict(model, test_df[, predictor_names]),
                 "RandomForest" = predict(model, test_df[, predictor_names], type = "prob")[, 2])
  
  # Calculate evaluation metrics
  roc_obj <- roc(test_df$presence, pred)
  auc_val <- auc(roc_obj)
  
  pred_class <- ifelse(pred >= 0.5, 1, 0)
  #pred_class <- factor(pred_class, levels = c(1,0))
  #test_df$presence <- factor(test_df$presence, levels = c(1,0))
  conf_matrix <- confusionMatrix(factor(pred_class), factor(test_df$presence))
  best_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = FALSE) %>% 
    sample_n(1)
  plot(roc_obj, main = paste0(model_type, " - AUC:", round(auc_val, 3)))
  
  data.frame(
    Model = model_type,
    AUC = round(auc_val, 3),
    Sensitivity = round(conf_matrix$byClass["Sensitivity"] , 3),
    Specificity = round(conf_matrix$byClass["Specificity"] , 3),
    TSS = round(conf_matrix$byClass["Sensitivity"] + conf_matrix$byClass["Specificity"] - 1, 3),
    F1 = round(conf_matrix$byClass["F1"] , 3),
    Accuracy = round(conf_matrix$overall["Accuracy"], 3),
    Kappa = round(conf_matrix$overall["Kappa"], 3),
    Best_threshold = round(best_threshold,3), 
    row.names = NULL) 
}

# Evaluate all models

png(paste0(output_specie, tag_spe, "_Model_AUC_plot.png"),
    width = 9, height = 9, units="in", res=300)
par(mfrow = c(2, 2))
eval_results <- rbind(
  evaluate_model(gam_model, test_data[, c("presence", predictor_names)], "GAM"),
  evaluate_model(gbm_model, test_data, "BRT"),
  evaluate_model(maxent_model, test_data, "MaxEnt"),
  evaluate_model(rf_model, test_data, "RandomForest")
)
dev.off()


# Print evaluation table
# dir.create("outputs")
print(eval_results)
write_csv(eval_results, file = paste0(output_specie, tag_spe, "_", "eval_metrics.csv"))

save(gbm_model, maxent_model, gam_model, rf_model, 
     file = paste0(output_specie, tag_spe, "_", "sdm_trained_models.RData"))

# 7. Create predictions ---------------------------------------------------
# GAM prediction
gam_pred <- predict(predictors, gam_model, type = "response")

# BRT prediction
gbm_pred <- predict(predictors, gbm_model, n.trees = best_trees, type = "response",na.rm=TRUE)

# MaxEnt prediction
maxent_pred <- predict(predictors, maxent_model,na.rm=TRUE)

# Random Forest prediction
rf_pred <- predict(predictors, rf_model, type = "prob", index = 2, na.rm=TRUE)

# 8. Plot predictions ----------------------------------------------------
jpeg(paste0(output_specie, tag_spe, "_SDM_prediction_Map.jpg"),
     width = 8.3, height = 11.7, units="in", res=300)
par(mfrow = c(2, 2))
plot(gam_pred, main = "GAM Prediction")
plot(gbm_pred, main = "BRT Prediction")
plot(maxent_pred, main = "MaxEnt Prediction")
plot(rf_pred, main = "Random Forest Prediction")
dev.off()

# 9. Ensemble model ------------------------------------------------------
# Rescale predictions to 0-1 range if necessary

# models <- rast(list(gam_pred, gbm_pred, maxent_pred, rf_pred))
# ensemble_media <- app(ensemble_t, mean)

# eval_results
# names(models) <- c("GAM_Logistic", "BRT_Regression", "Maxent", "RandomForest")
# plot(models, main='average score')

# Let’s combine three models weighted by their AUC scores. Here, to create the weights,
# we substract 0.5 (the random expectation) and square the result to give further weight to higher AUC values.
w <- (eval_results$AUC-0.5)^2
ensemble <- weighted.mean(rast(list(gam_pred, gbm_pred, maxent_pred, rf_pred)), w)
#plot(ensemble, main=paste0('Weighted mean of four models - ', tag_spe))

# ensemble <- (gam_pred + gbm_pred + maxent_pred + rf_pred) / 4
writeRaster(gam_pred, paste0(output_specie, tag_spe, "_", "GAM.tif"), overwrite=TRUE)
writeRaster(gbm_pred,paste0(output_specie, tag_spe, "_", "Boosted_Regression_Trees.tif"), overwrite=TRUE)
writeRaster(maxent_pred,paste0(output_specie, tag_spe, "_", "Maxent.tif"), overwrite=TRUE)
writeRaster(rf_pred,paste0(output_specie, tag_spe, "_", "RandomForest.tif"), overwrite=TRUE)

# Plot ensemble
# ensemble_corte <- mask(ensemble,study_area)
# binario_ensamble <- ensemble_corte > 0.5
# 
# # Plot the binary raster using white for 0 and darkgreen for 1
# par(mfrow = c(1,2))
# plot(ensemble, main = "Ensemble Model (Mean)")
# plot(binario_ensamble, col = c("white", "darkgreen"), main = "Binary Ensemble Prediction")
# plot(study_area ,add=TRUE, fill=NA)

# Save ensemble raster if needed


# Corte por departamento

# gam_pred<-mask(gam_pred,tanzania)
# gbm_pred<-mask(gbm_pred,tanzania)
# maxent_pred<-mask(maxent_pred,tanzania)
# rf_pred<-mask(rf_pred,tanzania)

# --- Convert Rasters to Data Frames ---
# Convert each raster to a data frame with x, y coordinates and prediction values
gam_df    <- as.data.frame(gam_pred, xy = TRUE)
gbm_df    <- as.data.frame(gbm_pred, xy = TRUE)
maxent_df <- as.data.frame(maxent_pred, xy = TRUE)
rf_df     <- as.data.frame(rf_pred, xy = TRUE)

# Rename the third column to "prediction" (assuming each has one layer)
names(gam_df)[3] <- "prediction"
names(gbm_df)[3] <- "prediction"
names(maxent_df)[3] <- "prediction"
names(rf_df)[3] <- "prediction"

# Add a column to indicate the model type
gam_df$Model    <- "GAM"
gbm_df$Model    <- "BRT"
maxent_df$Model <- "MaxEnt"
rf_df$Model     <- "RandomForest"

# --- Merge All Data Frames ---
df_all <- rbind(gam_df, gbm_df, maxent_df, rf_df)

# --- Define Global Limits for the Color Scale ---
global_min <- min(df_all$prediction, na.rm = TRUE)
global_max <- max(df_all$prediction, na.rm = TRUE)

# --- Plot with ggplot2 ---
p <- ggplot(df_all, aes(x = x, y = y, fill = prediction)) +
  geom_raster() +
  # Overlay the tanzania sf object without inheriting global aesthetics
  geom_sf(data = st_as_sf(study_area), 
          inherit.aes = FALSE, 
          mapping = aes(geometry = geometry),
          fill = NA, color = "black", size = 0.5) +
  facet_wrap(~ Model, ncol = 2) +
  scale_fill_viridis_c(option = "viridis", labels = function(x) round(x, 2),
                       begin = global_min, end = global_max,
                       name = "Idoneidad") +
  coord_sf() +
  theme_minimal(base_size = 14) +
  labs(title = str_to_title(paste0(tag_spe, " - ", "Mapas de Idoneidad"))) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(), 
        legend.position = "bottom")


ggsave(filename = paste0(output_specie, tag_spe, "_Suitability_Map.jpg"), plot = p, width = 8.3, height = 11.7 )

#binario

# --- Optimal Threshold Determination ---
# Use the test set predictions from one model (e.g., MaxEnt) to compute the optimal threshold using the Youden index.
# You could compute this separately per model; here we illustrate a single threshold for simplicity.
#presence_values <- terra::extract(predictors, species_points, ID = FALSE)
# roc_obj <- roc(test_data$presence, predict(maxent_model, test_data[, predictor_names]))
# best_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = FALSE)
# threshold_value <- best_threshold$threshold[1]
# plot(roc_obj)
# Apply the threshold to create a binary variable for each prediction in df_all
best_threshold <- mean(eval_results$threshold)
df_all$binary <- ifelse(df_all$prediction >= best_threshold, 1, 0)

# -------------------------------
# Plotting the Binary Predictions Map
# -------------------------------
p2 <- ggplot(df_all, aes(x = x, y = y, fill = factor(binary))) +
  geom_raster() +
  geom_sf(data = st_as_sf(study_area), 
          inherit.aes = FALSE, 
          mapping = aes(geometry = geometry),
          fill = NA, color = "black", size = 0.5) +
  facet_wrap(~ Model, ncol = 2) +
  scale_fill_manual(values = c("0" = "white", "1" = "darkgreen"),
                    name = paste("Threshold:", round(best_threshold, 2)),
                    labels = c("Absence", "Presence")) +
  coord_sf() +
  theme_minimal(base_size = 14) +
  labs(title = str_to_title(paste0(tag_spe, " - ", "Mapas Binarios"))) +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(), 
        legend.position = "bottom")

ggsave(filename = paste0(output_specie, tag_spe, "_Binary_Map.jpg"), plot = p2, width = 8, height = 11)


# plot_final

binario_ensamble_f <- ensemble > as.numeric(best_threshold)

# Plot the binary raster using white for 0 and darkgreen for 1
jpeg(paste0(output_specie, tag_spe, "_Ensemble_Map.jpg"),
     width = 11.7, height = 8.3, units="in", res=300)
par(mfrow = c(1,2))
plot(ensemble, main = str_to_title(paste0(tag_spe, " - ", "Ensemble Model Distribution")))
points(points_cleaned, col = "red")
plot(binario_ensamble_f, col = c("white", "darkgreen"), main = "Binary Ensemble Prediction")
points(points_cleaned, col = "red")
plot(study_area ,add=TRUE, fill=NA)
dev.off()


