# Species Distribution Modelling
## Modelling - TEST 5 models for presence and absence
## github.com/jrodriguez88
## Febrero 2025

## Split data for modeling ----

predictors <- all_predictors[[pred_select]] %>% raster::stack()

# species_dta <- data_cleaned
# species_dta <- species_dta %>% 
#   terra::as.data.frame() %>% 
#   dplyr::select(lon, lat) 


set.seed(1)
group <- kfold(species_dta, 5)
pres_train <- species_dta[group != 1, ]
pres_test <- species_dta[group == 1, ]

set.seed(10)
#backg <- randomPoints(raster::stack(predictors), n=1000, extf = 1.25)
colnames(backgr) = c('lon', 'lat')
group <- kfold(backgr, 5)
backg_train <- backgr[group != 1, ]
backg_test <- backgr[group == 1, ]

# Map select data
r <- predictors[[1]]
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')



train <- rbind(pres_train, backg_train)
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
envtrain <- extract(predictors, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )

testpres <- data.frame( extract(predictors, pres_test) )
testbackg <- data.frame( extract(predictors, backg_test) )


# Calcula TPR + TNR for evaluation
calcula_threshold <- function(e) {
  tpr_tnr_sum <- e@TPR + e@TNR
  # Encuentra el índice del valor máximo
  max_index <- which.max(tpr_tnr_sum)
  
  # Extrae el valor de e@t en el índice del valor máximo
  tr <- e@t[max_index]
  
  tr
  
  
}


## BioClim Model ----


# Fit model
bc <- bioclim(predictors, pres_train)
par(mfrow=c(1,1))
plot(bc, a=1, b=2, p=0.85)

# Evaluate model
be <- evaluate(pres_test, backg_test, bc, predictors)
be

be_tr <- calcula_threshold(be)


# Predict values
pb <- predict(predictors, bc, progress='')
pb

par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(limites_sel, add=TRUE, border='dark grey')
plot(pb > be_tr, main='presence/absence')
plot(limites_sel, add=TRUE, border='dark grey')
points(pres_train, pch='+')


## Generalized Linear Models ----

# Logistic Regression
gm1 <- glm(pa ~ ., family = binomial(link = "logit"), data=envtrain)
summary(gm1)

# Evaluate model
gm1_eval <- evaluate(testpres, testbackg, gm1)
gm1_eval

gm1_tr <- calcula_threshold(gm1_eval)


pg1 <- predict(predictors, gm1)
par(mfrow=c(1,2))
plot(pg1, main='Logistic Reg, raw values')
plot(limites_sel, add=TRUE, border='dark grey')
plot(pg1 > gm1_tr, main='presence/absence')
plot(limites_sel, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)




# Gaussian 
gm2 <- glm(pa ~ .,
           family = gaussian(link = "identity"), data=envtrain)

summary(gm2)

gm2_eval <- evaluate(testpres, testbackg, gm2)
gm2_eval


gm2_tr <- calcula_threshold(gm2_eval)


pg2 <- predict(predictors, gm2)
par(mfrow=c(1,2))
plot(pg2, main='GLM/gaussian, raw values')
plot(limites_sel, add=TRUE, border='dark grey')
plot(pg2 > gm2_tr, main='presence/absence')
plot(limites_sel, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)


## Maxent ----
maxent()
## Loading required namespace: rJava
## This is MaxEnt version 3.4.3
xm <- maxent(predictors, pres_train)
plot(xm)
response(xm)

me <- evaluate(pres_test, backg_test, xm, predictors)
me

tr <- calcula_threshold(me)


px <- predict(predictors, xm, progress='')
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values')
plot(limites_sel, add=TRUE, border='dark grey')
plot(px > tr, main='presence/absence')
plot(limites_sel, add=TRUE, border='dark grey')
points(pres_train, pch='+')


## RandomForest ----


## randomForest 4.6-14
## Training Model
model <- as.formula("pa ~ .")
rf1 <- randomForest(pa ~ ., data=na.omit(envtrain))
## Warning in randomForest.default(m, y, ...): The response has five or fewer
## unique values. Are you sure you want to do regression?

erf <- evaluate(testpres, testbackg, rf1)
erf

rf_tr <- calcula_threshold(erf)
## class          : ModelEvaluation
## n presences    : 23
## n absences     : 200
## AUC            : 0.8580435
## cor            : 0.5010053
## max TPR+TNR at : 0.1060667
pr <- predict(predictors, rf1)
par(mfrow=c(1,2))
plot(pr, main='Random Forest, regression')
plot(limites_sel, add=TRUE, border='dark grey')

plot(pr > rf_tr, main='presence/absence')
plot(limites_sel, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)



## Stack Models ---

models <- stack(pb ,pg1, pg2, px, pr)
names(models) <- c("Bioclim", "glm_Logistic", "glm_Gaussian", "Maxent", "RandomForest")
plot(models)

m <- mean(models)
plot(m, main='average score')


auc <- sapply(list(be, gm1_eval, gm2_eval, me, erf), function(x) x@auc)
w <- (auc-0.5)^2
m2 <- weighted.mean( models[[c("Bioclim", "glm_Logistic", "glm_Gaussian", "Maxent", "RandomForest")]], w)
plot(m2, main='weighted mean of five models - Cacao')



par(mfrow=c(1,2))
auc <- sapply(list(me, erf), function(x) x@auc)
w <- (auc-0.5)^2
m2 <- weighted.mean( models[[c("Maxent", "RandomForest")]], w)
plot(m2, main='weighted mean of Two models - Cacao')
plot(m2 > 0.51, main='presence/absence')
plot(limites_sel, add=TRUE, border='dark grey')
points(pres_test, pch='+')



