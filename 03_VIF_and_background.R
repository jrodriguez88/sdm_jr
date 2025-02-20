# Species Distribution Modelling
## Preprocessing ocurrence data VIF and Background
## github.com/jrodriguez88
## Febrero 2025

# library(tidymodels)
# library(car)

## Prepare data
predictors <- raster::stack(all_predictors)
species_dta <- data_cleaned
species_dta <- species_dta %>% 
  terra::as.data.frame() %>% 
  dplyr::select(lon, lat) 
presvals <- extract(predictors, species_dta)
#presvals <- presvals[,-1] 



n_points <- nrow(data_cleaned)

backgr <- randomPoints(predictors, n_points)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdm_data <- data.frame(cbind(pb, rbind(presvals, absvals)))



vif_sdm <- function(sdm_data, vif_limit = 10, correlation_limit = 0.85){
  
  prep_recipe_all <- 
    recipe( ~ ., data = sdm_data)  %>%
    update_role(pb, new_role = "outcome")  %>% 
    step_filter_missing(all_predictors(), threshold = 0.3) %>% 
    step_nzv(all_numeric()) %>%
    
    step_corr(all_numeric_predictors(), threshold = correlation_limit, method = "spearman") %>% 
    
    # Impute missing values using the knn imputation algorithm
    step_impute_knn(all_predictors()) %>% 
    
    # Create dummy variables for all nominal predictors
    #  step_dummy(all_nominal_predictors()) %>%
    
    step_nzv(all_numeric(), ) %>%
    prep()
  
  
  #
  
  # Apply the recipe to the data
  filtered_test_data <- 
    prep_recipe_all %>% 
    bake(new_data = NULL)
  
  # Visualize missing data
  filtered_test_data %>% vis_miss()
  
  
  #cor_matrix <- cor(sdmdata, use = "complete.obs", method = "spearman")
  cor_matrix <- cor(filtered_test_data, use = "complete.obs", method = "spearman")
  
  corrplot::corrplot.mixed(cor_matrix, main = " Spearman Correlation")
  
  data <- filtered_test_data #%>% mutate(pb = factor(pb))
  
  ## VIF filter
  
  
  m1 <- lm(pb ~ ., data = filtered_test_data)
  
  summary(m1)
  
  vif(m1)
  
  
  pred_select <- which(vif(m1) < vif_limit) %>% names
  
}

pred_select <- vif_sdm(sdm_data)


