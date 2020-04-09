# Quadratic Discriminant Analysis

## libraries
library(scorecard) # split_df
require(FSA)
require(MASS)
require(caret)

## seed
seed=123
set.seed(seed)

## split ratio
split.ratio = c(0.7, 0.3)


## functions
accFromCm = function(pred, true) { confusionMatrix(pred, true)$overall[1] }

factorizefeatures = function(dataset){
  dataset$gender = as.factor(dataset$gender)
  dataset$choles  = as.factor(dataset$choles)
  dataset$glucose = as.factor(dataset$glucose)
  dataset$smoke = as.factor(dataset$smoke)
  dataset$alcohol = as.factor(dataset$alcohol)
  dataset$active  = as.factor(dataset$active)
  dataset$cardio = as.factor(dataset$cardio)
  
  return(dataset)
}

get.qda.train.test.error = function(model, x.train, x.test, y.train, y.test, title){
  pred.train = predict(model, x.train)$class
  pred.test = predict(model, x.test)$class
  
  return( list(title, accFromCm(pred.train, y.train), accFromCm(pred.test, y.test)))
}


#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## factorization
data.set = factorizefeatures(data.set)

## split data
tts = split_df(data.set, ratio=split.ratio, seed=seed)

## complete model
qda.mod.1 = qda(cardio ~ ., data=tts$train)


### feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
qda.mod.2= qda(cardio ~ age  +
                  height +
                  weight +
                  aphi +
                  aplo +
                  choles +
                  glucose +
                  active, 
                  data=tts$train)


### train test error
qda.tt.res = data.frame(0,0,0)
names(qda.tt.res) = c("method", "train.error", "test.error")

qda.tt.res[1,] = get.qda.train.test.error( qda.mod.1,
                                           tts$train[,-12], tts$test[,-12],
                                           tts$train$cardio, tts$test$cardio,
                                           'with outliers - complete model')

qda.tt.res[nrow(qda.tt.res)+1,] = get.qda.train.test.error(qda.mod.2,
                                                           tts$train[,-12], tts$test[,-12],
                                                           tts$train$cardio, tts$test$cardio,
                                                           'with outliers - EDA feature selection')


#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

data.set2 = factorizefeatures(data.set2)

## split data
tts2 = split_df(data.set2, ratio=split.ratio, seed=seed)


## complete model
qda.mod.3 = qda(cardio ~ ., data=tts2$train)


### feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
qda.mod.4= qda(cardio ~ age  +
                 height +
                 weight +
                 aphi +
                 aplo +
                 choles +
                 glucose +
                 active, 
               data=tts2$train)


### train test error
qda.tt.res[nrow(qda.tt.res)+1,] = get.qda.train.test.error(qda.mod.3,
                                                           tts2$train[,-12], tts2$test[,-12],
                                                           tts2$train$cardio, tts2$test$cardio,
                                                           'without outliers - complete model')

qda.tt.res[nrow(qda.tt.res)+1,] = get.qda.train.test.error(qda.mod.4,
                                                           tts2$train[,-12], tts2$test[,-12],
                                                           tts2$train$cardio, tts2$test$cardio,
                                                           'without outliers - EDA feature selection')




qda.tt.res
# method train.error test.error
# 1           with outliers - complete model   0.5891654  0.5920138
# 2    with outliers - EDA feature selection   0.5879851  0.5910071
# 3        without outliers - complete model   0.5956583  0.5911682
# 4 without outliers - EDA feature selection   0.5938749  0.5896705
