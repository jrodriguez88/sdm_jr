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
  ntree = 1000,
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
  
  # pred_class <- ifelse(pred >= 0.5, 1, 0)
  roc_obj <- roc(test_df$presence, as.numeric(pred))
  auc_val <- auc(roc_obj)
  best_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = FALSE)[[1]]
  
  
  pred_class <- ifelse(pred >= best_threshold, 1, 0)
  #pred_class <- factor(pred_class, levels = c(1,0))
  #test_df$presence <- factor(test_df$presence, levels = c(1,0))
  conf_matrix <- confusionMatrix(factor(pred_class), factor(test_df$presence))
  
  
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
    threshold = round(best_threshold,3), 
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