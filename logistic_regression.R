# Logistic Regression

## libraries
library(scorecard) # split_df
require(FSA)
require(MASS)
require(caret)

## seed
set.seed(123)

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


#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

data.set = factorizefeatures(data.set)

## split data
tts = split_df(data.set, ratio = 0.70, seed = 123)
train = tts$train
test = tts$test

## complete model
logr.mod.1 = glm(cardio ~., data=train, family = "binomial")


### feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
# Warning messages: 1: glm.fit: algorithm did not converge 
logr.mod.2= glm(cardio ~ age  +
                  height +
                  weight +
                  aphi +
                  aplo +
                  choles +
                  glucose +
                  active, 
                  data= train, family = "binomial")


### train test error
logr.tt.res = data.frame(0,0,0)
names(logr.tt.res) = c("method", "train.error", "test.error")

logr.pred.train.1 = predict(logr.mod.1, train[,-12])
logr.pred.train.1 = factor(ifelse(logr.pred.train.1 > 0.50, 1, 0))
logr.pred.test.1 = predict(logr.mod.1, test[,-12])
logr.pred.test.1 = factor(ifelse(logr.pred.test.1 > 0.50, 1, 0))
logr.tt.res[1,] = list('with outliers - complete model', 
                   accFromCm(logr.pred.train.1, train$cardio), 
                   accFromCm(logr.pred.test.1, test$cardio))


logr.pred.train.2 = predict(logr.mod.2, train[,-12])
logr.pred.train.2 = factor(ifelse(logr.pred.train.2 > 0.50, 1, 0))
logr.pred.test.2 = predict(logr.mod.2, test[,-12])
logr.pred.test.2 = factor(ifelse(logr.pred.test.2 > 0.50, 1, 0))
logr.tt.res[nrow(logr.tt.res)+1,] = list('with outliers - with feature selection', 
                               accFromCm(logr.pred.train.2, train$cardio), 
                               accFromCm(logr.pred.test.2, test$cardio))




#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

data.set2 = factorizefeatures(data.set2)

## split data
tts2 = split_df(data.set2, ratio = 0.70, seed = 123)
train2 = tts2$train
test2 = tts2$test

### complete model
logr.mod.3 = glm(cardio ~., data=train2, family = "binomial")

### feature selection - based on EDA of cardio.r
logr.mod.4= lm(cardio ~ age  +
               height +
               weight +
               aphi +
               aplo +
               choles +
               glucose +
               active,
               data= train2, family = "binomial")


### train test error
logr.pred.train.3 = predict(logr.mod.3, train[,-12])
logr.pred.train.3 = factor(ifelse(logr.pred.train.3 > 0.50, 1, 0))
logr.pred.test.3 = predict(logr.mod.3, test[,-12])
logr.pred.test.3 = factor(ifelse(logr.pred.test.3 > 0.50, 1, 0))
logr.tt.res[nrow(logr.tt.res)+1,] = list('no outliers - complete model', 
                               accFromCm(logr.pred.train.3, train$cardio), 
                               accFromCm(logr.pred.test.3, test$cardio))

logr.pred.train.4 = predict(logr.mod.4, train[,-12])
logr.pred.train.4 = factor(ifelse(logr.pred.train.4 > 0.50, 1, 0))
logr.pred.test.4 = predict(logr.mod.4, test[,-12])
logr.pred.test.4 = factor(ifelse(logr.pred.test.4 > 0.50, 1, 0))
logr.tt.res[nrow(logr.tt.res)+1,] = list('no outliers - with feature selection', 
                               accFromCm(logr.pred.train.4, train$cardio), 
                               accFromCm(logr.pred.test.4, test$cardio))



logr.tt.res
#  no outliers - complete model, mod.3, performed the best

# ROC curve?