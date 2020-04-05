# Quadratic Discriminant Analysis

## libraries
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
train.idx = sample(1:length(data.set$cardio), ceiling(length(data.set$cardio) * 0.7))
train = data.set[train.idx, ]
test = data.set[-train.idx, ]

## complete model
qda.mod.1 = qda(cardio ~ ., data=train)


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
                  data=train)


### train test error
qda.tt.res = data.frame(0,0,0)
names(qda.tt.res) = c("method", "train.error", "test.error")

qda.pred.train.1 = predict(qda.mod.1, train[,-12])$class
qda.pred.test.1 = predict(qda.mod.1, test[,-12])$class
qda.tt.res[1,] = list('with outliers - complete model', 
                  accFromCm(qda.pred.train.1, train$cardio), 
                  accFromCm(qda.pred.test.1, test$cardio))


qda.pred.train.2 = predict(qda.mod.2, train[,-12])$class
qda.pred.test.2 = predict(qda.mod.2, test[,-12])$class
qda.tt.res[nrow(qda.tt.res)+1,] = list('with outliers - with feature selection', 
                               accFromCm(qda.pred.train.2, train$cardio), 
                               accFromCm(qda.pred.test.2, test$cardio))




#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

data.set2 = factorizefeatures(data.set2)

## split data
train.idx2 = sample(1:length(data.set2$cardio), ceiling(length(data.set2$cardio) * 0.7))
train2 = data.set2[train.idx2, ]
test2 = data.set2[-train.idx2, ]

## complete model
qda.mod.3 = qda(cardio ~ ., data=train2)


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
               data=train2)


### train test error
qda.pred.train.3 = predict(qda.mod.3, train[,-12])$class
qda.pred.test.3 = predict(qda.mod.3, test[,-12])$class
qda.tt.res[nrow(qda.tt.res)+1,] = list('with outliers - complete model', 
                      accFromCm(qda.pred.train.3, train$cardio), 
                      accFromCm(qda.pred.test.3, test$cardio))


qda.pred.train.4 = predict(qda.mod.4, train[,-12])$class
qda.pred.test.4 = predict(qda.mod.4, test[,-12])$class
qda.tt.res[nrow(qda.tt.res)+1,] = list('with outliers - with feature selection', 
                                       accFromCm(qda.pred.train.4, train$cardio), 
                                       accFromCm(qda.pred.test.4, test$cardio))


qda.tt.res
