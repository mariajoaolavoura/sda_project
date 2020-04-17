# Quadratic Discriminant Analysis - df clean

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

get.qda.train.test.error = function(model, x.train, x.test, y.train, y.test, title){
  pred.train = predict(model, x.train)$class
  pred.test = predict(model, x.test)$class
  
  return( list(title, accFromCm(pred.train, y.train), accFromCm(pred.test, y.test)))
}

#############################################
data.set = read.csv("./data/cardio-clean.csv")
headtail(data.set)

## factorization
data.set = factorize.features(data.set)

## one hot encoding
# cardio= data.set$cardio
# gender= data.set$gender
# data.set['gender']= ifelse(data.set['gender'] == 'woman', 0, 1)
# encoder= onehot(data.set[, -12])
# data.dmy= as.data.frame(predict(encoder, data.set[, -12]))
# data.dmy$cardio= cardio
# head(data.dmy)
# dim(data.dmy)

# if encoded, gives rank deficiency error 

## train/test split
tts = split_df(data.set, ratio=split.ratio, seed=seed)

## complete model
qda.mod.1 = qda(cardio ~ ., data=tts$train)

### remove gender
qda.mod.2 = qda(cardio ~ . -gender, data=tts$train)

### remove height
qda.mod.3 = qda(cardio ~ . -height, data=tts$train)

### remove gender + height
qda.mod.4 = qda(cardio ~ . -height
                , data=tts$train)


### train test error
qda.tt.res = data.frame(0,0,0)
names(qda.tt.res) = c("method", "train.accuracy", "test.accuracy")

qda.tt.res[1,] = get.qda.train.test.error( qda.mod.1,
                                           tts$train[,-12], tts$test[,-12],
                                           tts$train$cardio, tts$test$cardio,
                                           'complete model')

qda.tt.res[nrow(qda.tt.res)+1,] = get.qda.train.test.error(qda.mod.2,
                                                           tts$train[,-12], tts$test[,-12],
                                                           tts$train$cardio, tts$test$cardio,
                                                           'remove gender')

qda.tt.res[nrow(qda.tt.res)+1,] = get.qda.train.test.error(qda.mod.3,
                                                           tts$train[,-12], tts$test[,-12],
                                                           tts$train$cardio, tts$test$cardio,
                                                           'remove height')

qda.tt.res[nrow(qda.tt.res)+1,] = get.qda.train.test.error(qda.mod.4,
                                                           tts$train[,-12], tts$test[,-12],
                                                           tts$train$cardio, tts$test$cardio,
                                                           'remove gender + height')

qda.tt.res
#                   method train.accuracy test.accuracy
# 1         complete model      0.6868434     0.6836680
# 2          remove gender      0.6862280     0.6836680
# 3          remove height      0.6882563     0.6872114
# 4 remove gender + height      0.6882563     0.6872114