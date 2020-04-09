# Linear Regression

## libraries
library(scorecard) # split_df
require(FSA)
require(car)
require(lmtest)
require(caret)

## seed
seed=123
set.seed(seed)

## split ratio
split.ratio = c(0.7, 0.3)

#############################################
## functions

##### residuals analysis
plot.residuals = function(mod){
  par(mfrow=c(1,3))
  hist(rstandard(mod))
  boxplot(rstandard(mod))
  qqnorm(rstandard(mod))
  qqline(rstandard(mod))
  
  # par(mfrow=c(1, 2))
  # qqPlot(residuals(mod))
  # qqPlot(mod)
}
  

##### residuls vs predictors
plot.residuals.vs.predictors = function(mod, data.set){
  par(mfrow=c(2, 2))
  plot(data.set$height, rstandard(mod))
  plot(data.set$weight, rstandard(mod))
  plot(data.set$aphi, rstandard(mod))
  plot(data.set$aplo, rstandard(mod))
}


##### influent points
plot.influent.points = function(mod, data.set){
  par(mfrow=c(1, 1))
  clv = hatvalues(mod)
  plot(cooks.distance(mod), clv, xlab = "Cook's Distance", ylab = "Centered Leverage Value")
  cut_point = 2*(length(mod$coefficients))/dim(data.set)[1]
  abline(cut_point,0, lwd=1, lty="dashed", col="red")
  high.leverages = length(clv[clv > cut_point])
  high.leverages
}


##### factorization
factorizefeatures = function(dataset){
  dataset$gender = as.factor(dataset$gender)
  dataset$choles  = as.factor(dataset$choles)
  dataset$glucose = as.factor(dataset$glucose)
  dataset$smoke = as.factor(dataset$smoke)
  dataset$alcohol = as.factor(dataset$alcohol)
  dataset$active  = as.factor(dataset$active)
  #dataset$cardio = as.factor(dataset$cardio)
  
  return(dataset)
}

##### accuracy
accFromCm = function(pred, true) { confusionMatrix(pred, true)$overall[1] }



#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## factorization
data.set = factorizefeatures(data.set)

## split data
tts = split_df(data.set, ratio=split.ratio, seed=seed)

### null model
lr.mod.0= lm(cardio ~ 1, data=tts$train)
summary(lr.mod.0)
anova(lr.mod.0)

### complete model
lr.mod.1= lm(cardio ~ ., data=tts$train)
summary(lr.mod.1)
# All predictors are statisticaly significant. Glucose2 is the least significant of all.
# The adjusted R.squared is too small, 0.1188, too far away from 1. This means the model needs work, probably less variables.

##### residuals analysis
plot.residuals(lr.mod.1)
# not normal at all

##### residuals against predictors
plot.residuals.vs.predictors(lr.mod.1, tts$train)
# residuals seem to be linearly correlated with weight

##### influent points
plot.influent.points(lr.mod.1, tts$train)
# there are high leverages but since all cook's distance values are below 1,
# there are no influent points messing with the fit of the model.


##### feature selection with step function
lr.mod.2 = step(lr.mod.1, direction = "both")
summary(lr.mod.2)
# didnt remove any feature, mod2 = complete model



#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

## factorization
data.set2 = factorizefeatures(data.set2)

## split data
tts2 = split_df(data.set2, ratio=split.ratio, seed=seed)


### complete model
lr.mod.3= lm(cardio ~ ., data= tts2$train)
summary(lr.mod.3)
# Like the complete model without transformations on the data, mod.1, 
# all predictors are statisticaly significant. 
# The adjusted R.squared is too small, 0.1223, too far away from 1. 
# This means the model doesn't fit well the data.


##### residuals analysis
plot.residuals(lr.mod.3)
# not normal at all

##### residuals against predictors
plot.residuals.vs.predictors(lr.mod.3, tts2$train)
# residuals seem to be linearly correlated with weight

##### influent points
plot.influent.points(lr.mod.3, tts2$train)
# there are high leverages but since all cook's distance values are below 1,
# there are no influent points messing with the fit of the model.

### Feature selection
##### pre feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol
lr.mod.4= lm(cardio ~ age  +
                      height +
                      weight +
                      aphi +
                      aplo +
                      choles +
                      glucose +
                      active, data= tts2$train)
summary(lr.mod.4)
# the R-squared adjusted, 0.1218, is just a little bit smaller than the complete model.
anova(lr.mod.4, lr.mod.3)
# H0: betas=0; H1: betas!=0, pvalue<0.05, reject H0. The best model is the complete one.

##### feature selection with step function
lr.mod.5 = step(lr.mod.3, direction = "both")
summary(lr.mod.5)
# didnt remove any feature, mod5 = complete model
# anova(lr.mod.5, lr.mod.3)
# lrtest(lr.mod.5, lr.mod.3)


# Based on the adjusted R-squared, the complete model with transformations
# on the data is the one with R-sq closest to 1, so it is the best one.

lr.mod = lr.mod.3

# train test errors
lr.pred.train = predict(lr.mod, tts2$train[,-12])
lr.pred.train = ifelse(lr.pred.train > 0.50, 1, 0)
accFromCm(as.factor(lr.pred.train), as.factor(tts2$train$cardio))
# Accuracy 
# 0.6496115 


lr.pred.test = predict(lr.mod, tts2$test[,-12])
lr.pred.test = ifelse(lr.pred.test > 0.50, 1, 0)
accFromCm(as.factor(lr.pred.test), as.factor(tts2$test$cardio))
# Accuracy 
# 0.6446033 



