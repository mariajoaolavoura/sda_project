# Logistic Regression - df clean

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

factorize.features = function(dataset){
  colNames.qual= c('choles', 'gluc', 'smoke',
                   'alco', 'active', 'cardio')
  dataset[, colNames.qual]= lapply(dataset[, colNames.qual], factor)
  
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
data.set = read.csv("./data/cardio-clean.csv")
headtail(data.set)

## factorization
data.set = factorize.features(data.set)

## one hot encoding
cardio= data.set$cardio
gender= data.set$gender
data.set['gender']= ifelse(data.set['gender'] == 'woman', 0, 1)
encoder= onehot(data.set[, -12])
data.dmy= as.data.frame(predict(encoder, data.set[, -12]))
data.dmy$cardio= cardio
head(data.dmy)

## train/test split
tts = split_df(data.dmy, ratio=split.ratio, seed=seed)

### complete model
logr.mod.1 = glm(cardio ~., data=tts$train, family = "binomial")

### remove gender
logr.mod.2 = glm(cardio ~. -gender, data=tts$train, family = "binomial")
# logr.mod.2= lm(cardio ~age  +
#                  height +
#                  weight +
#                  aphi +
#                  aplo +
#                  `active=0` + `active=1` +
#                  `choles=1` + `choles=2` + `choles=3` +
#                  `gluc=1` + `gluc=2` + `gluc=3` +
#                  `smoke=0`, 
#                data = tts$train, family = "binomial")

### remove height
logr.mod.3 = glm(cardio ~. -height, data=tts$train, family = "binomial")
# logr.mod.3= lm(cardio ~age  +
#                  gender+
#                  weight +
#                  aphi +
#                  aplo +
#                  `active=0` + `active=1` +
#                  `choles=1` + `choles=2` + `choles=3` +
#                  `gluc=1` + `gluc=2` + `gluc=3` +
#                  `smoke=0`, 
#                data = tts$train, family = "binomial")



### remove gender + height
logr.mod.4 = glm(cardio ~. -gender-height, data=tts$train, family = "binomial")
# logr.mod.4= lm(cardio ~age  +
#                  weight +
#                  aphi +
#                  aplo +
#                  `active=0` + `active=1` +
#                  `choles=1` + `choles=2` + `choles=3` +
#                  `gluc=1` + `gluc=2` + `gluc=3` +
#                  `smoke=0`, 
#                data = tts$train, family = "binomial")




## train test error
logr.tt.res = data.frame(0,0,0)
names(logr.tt.res) = c("method", "train.accuracy", "test.accuracy")

logr.tt.res[1,] = get.logr.train.test.error(logr.mod.1,
                                            tts$train[,-19], tts$test[,-19],
                                            tts$train$cardio,  tts$test$cardio,
                                            'complete model')

logr.tt.res[nrow(logr.tt.res)+1,] = get.logr.train.test.error(logr.mod.2,
                                                              tts$train[,-19], tts$test[,-19],
                                                              tts$train$cardio,  tts$test$cardio,
                                                              'remove gender')


logr.tt.res[nrow(logr.tt.res)+1,] = get.logr.train.test.error(logr.mod.3,
                                                              tts$train[,-19], tts$test[,-19],
                                                              tts$train$cardio,  tts$test$cardio,
                                                              'remove height')

logr.tt.res[nrow(logr.tt.res)+1,] = get.logr.train.test.error(logr.mod.4,
                                                              tts$train[,-19], tts$test[,-19],
                                                              tts$train$cardio,  tts$test$cardio,
                                                              'remove gender + height')


logr.tt.res
#                   method train.accuracy test.accuracy
# 1         complete model      0.7046423     0.7026200
# 2          remove gender      0.7047107     0.7025663
# 3          remove height      0.7042777     0.7024052
# 4 remove gender + height      0.7044828     0.7018684
