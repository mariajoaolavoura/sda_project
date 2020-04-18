# Lasso Regression - df clean

## libraries
library(scorecard) # split_df
require(FSA)
require(MASS)
require(caret)
require(onehot)
require (glmnet) #lasso

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

get.lasso.train.test.error = function(model, best.lambda, x.train, x.test, y.train, y.test, title){
  pred.train = predict(model, s=best.lambda, newx=x.train)
  pred.train = as.factor(ifelse(pred.train > 0.50, 1, 0))
  pred.test = predict(model, s=best.lambda, newx=x.test)
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
dim(data.dmy)

## train/test split
tts = split_df(data.dmy, ratio=split.ratio, seed=seed)


###
## complete model
lasso.x.train.1 = model.matrix(cardio~., tts$train)[,-1]
lasso.x.test.1 = model.matrix(cardio~., tts$test)[,-1]

# find best lambda
cv.out.1 = cv.glmnet(lasso.x.train.1, as.numeric(as.character(tts$train$cardio)), alpha=1)
best.lambda.1 = cv.out.1$lambda.min

# model
lasso.mod.1 = glmnet(lasso.x.train.1, as.numeric(as.character(tts$train$cardio)), alpha=1)


###
## remove gender
lasso.x.train.2 = model.matrix(cardio~. -gender, tts$train)[,-1]
lasso.x.test.2 = model.matrix(cardio~. -gender, tts$test)[,-1]

# find best lambda
cv.out.2 = cv.glmnet(lasso.x.train.2, as.numeric(as.character(tts$train$cardio)), alpha=1)
best.lambda.2 = cv.out.2$lambda.min

# model
lasso.mod.2 = glmnet(lasso.x.train.2, as.numeric(as.character(tts$train$cardio)), alpha=1)


###
## remove height
lasso.x.train.3 = model.matrix(cardio~. -height, tts$train)[,-1]
lasso.x.test.3 = model.matrix(cardio~. -height, tts$test)[,-1]

# find best lambda
cv.out.3 = cv.glmnet(lasso.x.train.3, as.numeric(as.character(tts$train$cardio)), alpha=1)
best.lambda.3 = cv.out.3$lambda.min

# model
lasso.mod.3 = glmnet(lasso.x.train.3, as.numeric(as.character(tts$train$cardio)), alpha=1)



###
## remove gender + height
lasso.x.train.4 = model.matrix(cardio~. -height, tts$train)[,-1]
lasso.x.test.4 = model.matrix(cardio~. -height, tts$test)[,-1]

# find best lambda
cv.out.4 = cv.glmnet(lasso.x.train.4, as.numeric(as.character(tts$train$cardio)), alpha=1)
best.lambda.4 = cv.out.4$lambda.min

# model
lasso.mod.4 = glmnet(lasso.x.train.4, as.numeric(as.character(tts$train$cardio)), alpha=1)




### train test error
lasso.tt.res = data.frame(0,0,0)
names(lasso.tt.res) = c("method", "train.accuracy", "test.accuracy")

lasso.tt.res[1,] = get.lasso.train.test.error(lasso.mod.1, 
                                              best.lambda.1,
                                              lasso.x.train.1, lasso.x.test.1, 
                                              tts$train$cardio, tts$test$cardio, 
                                              'complete model')
lasso.tt.res[nrow(lasso.tt.res)+1,] = get.lasso.train.test.error(lasso.mod.2, 
                                                                 best.lambda.2,
                                                                 lasso.x.train.2, lasso.x.test.2, 
                                                                 tts$train$cardio, tts$test$cardio, 
                                                                 'remove gender')

lasso.tt.res[nrow(lasso.tt.res)+1,] = get.lasso.train.test.error(lasso.mod.3, 
                                                                 best.lambda.3,
                                                                 lasso.x.train.3, lasso.x.test.3, 
                                                                 tts$train$cardio, tts$test$cardio, 
                                                                 'remove height')

lasso.tt.res[nrow(lasso.tt.res)+1,] = get.lasso.train.test.error(lasso.mod.4, 
                                                                 best.lambda.4,
                                                                 lasso.x.train.4, lasso.x.test.4, 
                                                                 tts$train$cardio, tts$test$cardio,
                                                                 'remove gender + height')




lasso.tt.res
#                   method train.accuracy test.accuracy
# 1         complete model      0.7240366     0.7232363
# 2          remove gender      0.7240366     0.7235048
# 3          remove height      0.7237859     0.7231826
# 4 remove gender + height      0.7237859     0.7231826
