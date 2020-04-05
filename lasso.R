# Lasso Regression

## libraries
require(FSA) #headtail
library (glmnet) #lasso
require(caret) #confusion matrix

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
                      #dataset$cardio = as.factor(dataset$cardio)
                      
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

lasso.y.train.1 = train$cardio
lasso.y.test.1 = test$cardio


## complete model
lasso.x.train.1 = model.matrix(cardio~., train)[,-1]
lasso.x.test.1 = model.matrix(cardio~., test)[,-1]

# find best lambda
cv.out.1 = cv.glmnet(lasso.x.train.1, lasso.y.train.1, alpha=1)
best.lambda.1 = cv.out.1$lambda.min

# model
lasso.mod.1 = glmnet(lasso.x.train.1, lasso.y.train.1, alpha=1)


## feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
lasso.x.train.2 = model.matrix(cardio~ age  +
                                     height +
                                     weight +
                                     aphi +
                                     aplo +
                                     choles +
                                     glucose +
                                     active, train)[,-1]
lasso.x.test.2 = model.matrix(cardio~ age  +
                               height +
                               weight +
                               aphi +
                               aplo +
                               choles +
                               glucose +
                               active, test)[,-1]

#find best lambda
cv.out.2 = cv.glmnet(lasso.x.train.2, lasso.y.train.1, alpha=1)
best.lambda.2 = cv.out.2$lambda.min

#model
lasso.mod.2 = glmnet(lasso.x.train.2, lasso.y.train.1, alpha=1)



### train test error
lasso.tt.res = data.frame(0,0,0)
names(lasso.tt.res) = c("method", "train.error", "test.error")

#Erros nos accFromCm
#prediction using the best lambda
lasso.pred.train.1 = predict(lasso.mod.1, s=best.lambda.1, newx=lasso.x.train.1 )[,1]
lasso.pred.train.1 = ifelse(lasso.pred.train.1 > 0.50, 1, 0)
lasso.pred.test.1 = predict(lasso.mod.1, s=best.lambda.1, newx=lasso.x.test.1 )[,1]
lasso.pred.test.1 = ifelse(lasso.pred.test.1 > 0.50, 1, 0)
lasso.tt.res[1,] = list('with outliers - complete model',
                      accFromCm(lasso.pred.train.1, lasso.y.train.1),
                      accFromCm(lasso.pred.test.1, lasso.y.test.1))

lasso.pred.train.2 = predict(lasso.mod.2, s=best.lambda.2, newx=lasso.x.train.2 )[,1]
lasso.pred.train.2 = ifelse(lasso.pred.train.2 > 0.50, 1, 0)
lasso.pred.test.2 = predict(lasso.mod.2, s=best.lambda.2, newx=lasso.x.test.2 )[,1]
lasso.pred.test.2 = ifelse(lasso.pred.test.2 > 0.50, 1, 0)
lasso.tt.res[nrow(lasso.tt.res)+1,] = list('with outliers - with feature selection',
                                       accFromCm(lasso.pred.train.2, lasso.y.train.1),
                                       accFromCm(lasso.pred.test.2, lasso.y.test.1))




#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

data.set2 = factorizefeatures(data.set2)

## split data
train.idx2 = sample(1:length(data.set2$cardio), ceiling(length(data.set2$cardio) * 0.7))
train2 = data.set2[train.idx2, ]
test2 = data.set2[-train.idx2, ]

lasso.y.train.2 = train2$cardio
lasso.y.test.2 = test2$cardio


## complete model
lasso.x.train.3 = model.matrix(cardio~., train2)[,-1]
lasso.x.test.3 = model.matrix(cardio~., test2)[,-1]

# find best lambda
cv.out.3 = cv.glmnet(lasso.x.train.3, lasso.y.train.2, alpha=1)
best.lambda.3 = cv.out.3$lambda.min

# model
lasso.mod.3 = glmnet(lasso.x.train.3, lasso.y.train.2, alpha=1)


## feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
lasso.x.train.4 = model.matrix(cardio~ age  +
                                 height +
                                 weight +
                                 aphi +
                                 aplo +
                                 choles +
                                 glucose +
                                 active, train2)[,-1]
lasso.x.test.4 = model.matrix(cardio~ age  +
                                height +
                                weight +
                                aphi +
                                aplo +
                                choles +
                                glucose +
                                active, test2)[,-1]

#find best lambda
cv.out.4 = cv.glmnet(lasso.x.train.4, lasso.y.train.2, alpha=1)
best.lambda.4 = cv.out.4$lambda.min

#model
lasso.mod.4 = glmnet(lasso.x.train.4, lasso.y.train.2, alpha=1)


#Erros nos accFromCm

### train test error
#prediction using the best lambda
lasso.pred.train.3 = predict(lasso.mod.3, s=best.lambda.3, newx=lasso.x.train.3 )[,1]
lasso.pred.train.3 = ifelse(lasso.pred.train.3 > 0.50, 1, 0)
lasso.pred.test.3 = predict(lasso.mod.3, s=best.lambda.3, newx=lasso.x.test.3 )[,1]
lasso.pred.test.3 = ifelse(lasso.pred.test.3 > 0.50, 1, 0)
lasso.tt.res[nrow(lasso.tt.res)+1,] = list('with outliers - complete model',
                                          accFromCm(lasso.pred.train.3, lasso.y.train.2),
                                          accFromCm(lasso.pred.test.3, lasso.y.test.2))

lasso.pred.train.4 = predict(lasso.mod.4, s=best.lambda.4, newx=lasso.x.train.4 )[,1]
lasso.pred.train.4 = ifelse(lasso.pred.train.4 > 0.50, 1, 0)
lasso.pred.test.4 = predict(lasso.mod.4, s=best.lambda.4, newx=lasso.x.test.4 )[,1]
lasso.pred.test.4 = ifelse(lasso.pred.test.4 > 0.50, 1, 0)
lasso.tt.res[nrow(lasso.tt.res)+1,] = list('with outliers - with feature selection',
                                         accFromCm(lasso.pred.train.4, lasso.y.train.2),
                                         accFromCm(lasso.pred.test.4, lasso.y.test.2))


lasso.tt.res




