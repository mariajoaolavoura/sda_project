# Linear Regression

## libraries
library(scorecard) # split_df
require(FSA)
require(car)
require(lmtest)
require(caret)

## seed
set.seed(123)

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


#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

data.set$gender = as.factor(data.set$gender)
data.set$choles  = as.factor(data.set$choles)
data.set$glucose = as.factor(data.set$glucose)
data.set$smoke = as.factor(data.set$smoke)
data.set$alcohol = as.factor(data.set$alcohol)
data.set$active  = as.factor(data.set$active)

## split data
tts = split_df(data.set, ratio = 0.70, seed = 123)
train = tts$train
test = tts$test

### null model
lr.mod.0= lm(cardio ~ 1)
summary(lr.mod.0)
anova(lr.mod.0)

### complete model
lr.mod.1= lm(cardio ~ ., data= train)
summary(lr.mod.1)
# All predictors are statisticaly significant. Glucose2 is the least significant of all.
# The adjusted R.squared is too small, 0.122, too far away from 1. This means the model needs work, probably less variables.

##### residuals analysis
plot.residuals(lr.mod.1)
# not normal at all

##### residuals against predictors
plot.residuals.vs.predictors(lr.mod.1, train)
# residuals seem to be linearly correlated with weight

##### influent points
plot.influent.points(lr.mod.1, train)
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

data.set2$choles  = as.factor(data.set2$choles)
data.set2$glucose = as.factor(data.set2$glucose)
data.set2$active  = as.factor(data.set2$active)

str(data.set2)

## split data
tts2 = split_df(data.set2, ratio = 0.70, seed = 123)
train2 = tts2$train
test2 = tts2$test

### complete model
lr.mod.3= lm(cardio ~ ., data= train2)
summary(lr.mod.3)
# Like the complete model without transformations on the data, mod.1, 
# all predictors are statisticaly significant. 
# The adjusted R.squared is too small, 0.12, too far away from 1. 
# This means the model doesn't fit well the data.


##### residuals analysis
plot.residuals(lr.mod.3)
# not normal at all

##### residuals against predictors
plot.residuals.vs.predictors(lr.mod.3, train2)
# residuals seem to be linearly correlated with weight

##### influent points
plot.influent.points(lr.mod.3, train2)
# there are high leverages but since all cook's distance values are below 1,
# there are no influent points messing with the fit of the model.

### Feature selection
##### pre feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol
# data.set2$gender  = NULL
# data.set2$smoke   = NULL
# data.set2$alcohol = NULL
lr.mod.4= lm(cardio ~ age  +
                      height +
                      weight +
                      aphi +
                      aplo +
                      choles +
                      glucose +
                      active, data= train2)
summary(lr.mod.4)
# the R-squared adjusted is just a little bit smaller than the complete model.
anova(lr.mod.4, lr.mod.3)
# H0: betas=0; H1: betas!=0, pvalue<0.05, reject H0. The best model is the complete one.

##### feature selection with step function
lr.mod.6 = step(lr.mod.3, direction = "both")
summary(lr.mod.6)
# didnt remove any feature, mod5 = complete model
# anova(lr.mod.5, lr.mod.3)
# lrtest(lr.mod.5, lr.mod.3)


# Based on the adjusted R-squared, the complete model without transformations
# on the data is the one with R-sq closest to 1, so it is the better compared to 
# the complete model with data transformation.

lr.mod = lr.mod.1

# error
# accFromCm = function(pred, true) { confusionMatrix(pred, true)$overall[1] }
# 
# lr.pred.train = predict(lr.mod, train[,-12])
# lr.pred.train = ifelse(lr.pred.train > 0.50, 1, 0)
# accFromCm(lr.pred.train, train$cardio)
# 
# lr.pred.test = predict(lr.mod, test[,-12])
# lr.pred.test = ifelse(lr.pred.test > 0.50, 1, 0)
# accFromCm(lr.pred.test, test$cardio)




