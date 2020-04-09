# Lasso Regression

## libraries
library(scorecard) # split_df
require(FSA) #headtail
library (glmnet) #lasso
require(caret) #confusion matrix

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

get.lasso.train.test.error = function(model, best.lambda, x.train, x.test, y.train, y.test, title){
  pred.train = predict(model, s=best.lambda, newx=x.train)
  pred.train = as.factor(ifelse(pred.train > 0.50, 1, 0))
  pred.test = predict(model, s=best.lambda, newx=x.test)
  pred.test = as.factor(ifelse(pred.test > 0.50, 1, 0))
  
  return(list(title, accFromCm(pred.train, y.train), accFromCm(pred.test, y.test)))
}

#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## factorization
data.set = factorizefeatures(data.set)

## split data
tts = split_df(data.set, ratio=split.ratio, seed=seed)

lasso.y.train.1 = tts$train$cardio
lasso.y.test.1 = tts$test$cardio

###
## complete model
lasso.x.train.1 = model.matrix(cardio~., tts$train)[,-1]
lasso.x.test.1 = model.matrix(cardio~., tts$test)[,-1]

# find best lambda
cv.out.1 = cv.glmnet(lasso.x.train.1, as.numeric(as.character(lasso.y.train.1)), alpha=1)
best.lambda.1 = cv.out.1$lambda.min

# model
lasso.mod.1 = glmnet(lasso.x.train.1, as.numeric(as.character(lasso.y.train.1)), alpha=1)


###
## feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
lasso.x.train.2 = model.matrix(cardio~ age  +
                                     height +
                                     weight +
                                     aphi +
                                     aplo +
                                     choles +
                                     glucose +
                                     active, tts$train)[,-1]
lasso.x.test.2 = model.matrix(cardio~ age  +
                               height +
                               weight +
                               aphi +
                               aplo +
                               choles +
                               glucose +
                               active, tts$test)[,-1]

# find best lambda
cv.out.2 = cv.glmnet(lasso.x.train.2, as.numeric(as.character(lasso.y.train.1)), alpha=1)
best.lambda.2 = cv.out.2$lambda.min

# model
lasso.mod.2 = glmnet(lasso.x.train.2, as.numeric(as.character(lasso.y.train.1)), alpha=1)


###
## train test error
lasso.tt.res = data.frame(0,0,0)
names(lasso.tt.res) = c("method", "train.error", "test.error")


# prediction using the best lambda
lasso.tt.res[1,] = get.lasso.train.test.error(lasso.mod.1, 
                                              best.lambda.1,
                                              lasso.x.train.1, lasso.x.test.1, 
                                              lasso.y.train.1, lasso.y.test.1, 
                                              'with outliers - complete model')
  

lasso.tt.res[nrow(lasso.tt.res)+1,] = get.lasso.train.test.error(lasso.mod.2, 
                                                                 best.lambda.2,
                                                                 lasso.x.train.2, lasso.x.test.2, 
                                                                 lasso.y.train.1, lasso.y.test.1, 
                                                                 'with outliers - EDA feature selection')


#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

data.set2 = factorizefeatures(data.set2)

## split data
tts2 = split_df(data.set2, ratio=split.ratio, seed=seed)

lasso.y.train.2 = tts2$train$cardio
lasso.y.test.2 = tts2$test$cardio


## complete model
lasso.x.train.3 = model.matrix(cardio~., tts2$train)[,-1]
lasso.x.test.3 = model.matrix(cardio~., tts2$test)[,-1]

# find best lambda
cv.out.3 = cv.glmnet(lasso.x.train.3, as.numeric(as.character(lasso.y.train.2)), alpha=1)
best.lambda.3 = cv.out.3$lambda.min

# model
lasso.mod.3 = glmnet(lasso.x.train.3, as.numeric(as.character(lasso.y.train.2)), alpha=1)


## feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
lasso.x.train.4 = model.matrix(cardio~ age  +
                                 height +
                                 weight +
                                 aphi +
                                 aplo +
                                 choles +
                                 glucose +
                                 active, tts2$train)[,-1]
lasso.x.test.4 = model.matrix(cardio~ age  +
                                height +
                                weight +
                                aphi +
                                aplo +
                                choles +
                                glucose +
                                active, tts2$test)[,-1]

#find best lambda
cv.out.4 = cv.glmnet(lasso.x.train.4, as.numeric(as.character(lasso.y.train.2)), alpha=1)
best.lambda.4 = cv.out.4$lambda.min

#model
lasso.mod.4 = glmnet(lasso.x.train.4, as.numeric(as.character(lasso.y.train.2)), alpha=1)


#Erros nos accFromCm

### train test error
#prediction using the best lambda
lasso.tt.res[nrow(lasso.tt.res)+1,] = get.lasso.train.test.error(lasso.mod.3, 
                                                                 best.lambda.3,
                                                                 lasso.x.train.3, lasso.x.test.3, 
                                                                 lasso.y.train.2, lasso.y.test.2, 
                                                                 'without outliers - complete model')

lasso.tt.res[nrow(lasso.tt.res)+1,] = get.lasso.train.test.error(lasso.mod.4, 
                                                                 best.lambda.4,
                                                                 lasso.x.train.4, lasso.x.test.4, 
                                                                 lasso.y.train.2, lasso.y.test.2, 
                                                                 'without outliers - EDA feature selection')

lasso.tt.res

# method train.error test.error
# 1           with outliers - complete model   0.6460245  0.6515987
# 2    with outliers - EDA feature selection   0.6452512  0.6516945
# 3        without outliers - complete model   0.6499600  0.6440236
# 4 without outliers - EDA feature selection   0.6501035  0.6422843


