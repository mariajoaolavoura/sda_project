# Logistic Regression

## libraries
library(scorecard) # split_df
require(FSA)
require(MASS)
require(caret)
require(onehot)

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

one.hot.encode = function(dataset){
  y = dataset[,12]
  dataset = one_hot(dataset[,-12])
  dataset$cardio = y
  return(dataset)
}

get.logr.train.test.error = function(model, x.train, x.test, y.train, y.test, title){
  pred.train = predict(model, x.train)
  pred.train = as.factor(ifelse(pred.train > 0.50, 1, 0))
  pred.test = predict(model, x.test)
  pred.test = as.factor(ifelse(pred.test > 0.50, 1, 0))
  
  return(list(title, accFromCm(pred.train, y.train), accFromCm(pred.test, y.test)))
}
#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## factorization
data.set = factorizefeatures(data.set)

## one hot encoding
data.set = one.hot.encode(data.set)
dim(data.set)

## split data
tts = split_df(data.set, ratio=split.ratio, seed=seed)


###
## complete model
logr.mod.1 = glm(cardio ~., data=tts$train, family = "binomial")


### feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
# Warning messages: 1: glm.fit: algorithm did not converge 
logr.mod.2= glm(cardio ~ age  +
                  height +
                  weight +
                  aphi +
                  aplo  +
                  active_0 + active_1+
                  choles_1 + choles_2 + choles_3 +
                  glucose_1 + glucose_2 + glucose_3, 
                  data= tts$train, family = "binomial")


## train test error
logr.tt.res = data.frame(0,0,0)
names(logr.tt.res) = c("method", "train.error", "test.error")

logr.tt.res[1,] = get.logr.train.test.error(logr.mod.1,
                                            tts$train[,-20], tts$test[,-20],
                                            tts$train$cardio,  tts$test$cardio,
                                            'with outliers - complete model')

logr.tt.res[nrow(logr.tt.res)+1,] = get.logr.train.test.error(logr.mod.2,
                                                              tts$train[,-20], tts$test[,-20],
                                                              tts$train$cardio,  tts$test$cardio,
                                                              'with outliers - EDA feature selection')



#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

data.set2 = factorizefeatures(data.set2)

## one hot encoding
## one hot encoding
data.set2 = one.hot.encode(data.set2)
dim(data.set2)

## split data
tts2 = split_df(data.set2, ratio=split.ratio, seed=seed)


### complete model
logr.mod.3 = glm(cardio ~., data=tts2$train, family = "binomial")

### feature selection - based on EDA of cardio.r
logr.mod.4= lm(cardio ~ age  +
               height +
               weight +
               aphi +
               aplo +
               active_0 + active_1+
               choles_1 + choles_2 + choles_3 +
               glucose_1 + glucose_2 + glucose_3, 
               data= tts2$train, family = "binomial")


### train test error

logr.tt.res[nrow(logr.tt.res)+1,] = get.logr.train.test.error(logr.mod.3,
                                                              tts2$train[,-20], tts2$test[,-20],
                                                              tts2$train$cardio,  tts2$test$cardio,
                                                              'without outliers - complete model')

logr.tt.res[nrow(logr.tt.res)+1,] = get.logr.train.test.error(logr.mod.4,
                                                              tts2$train[,-20], tts2$test[,-20],
                                                              tts2$train$cardio,  tts2$test$cardio,
                                                              'without outliers - EDA feature selection')


logr.tt.res
# method train.error test.error
# 1           with outliers - complete model   0.6885570  0.6929198
# 2    with outliers - EDA feature selection   0.6878854  0.6922966
# 3        without outliers - complete model   0.6905479  0.6839308
# 4 without outliers - EDA feature selection   0.4984933  0.5021258


# ROC curve?