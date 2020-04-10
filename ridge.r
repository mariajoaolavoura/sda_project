###
## Ridge Regression
###

## libraries
require(glmnet)
require(FSA)

## seed
set.seed(123)

## functions

#' Function \code{train_test_split}
#'
#' Split data set into train and test sets
#' @author Nuno R. C. Gomes <nunorcgomes@@gmail.com> (2020/03/18)
#' @param dataset A data set
#' @param target Target variable (name or index)
#' @param frac Proportion of data set for training
#' @param seed A seed for the sampling; Default: 42
#' @return Vector \code{tts} of mode "list"
#' @examples
#' train_test_split(pid, "diabetes", 0.8)
train_test_split= function(dataset, target, frac, seed= 42) {
  set.seed(seed)
  nr= nrow(dataset)
  seqnr= 1:nr
  # target variable and index
  if (typeof(target) == "character") {
    target.idx= which(colnames(dataset) == target)
  } else if (typeof(target) == "double") {
    target.idx= target
    target= colnames(dataset[target.idx])
  } else {
    stop("Invalid `target` argument.")
  }
  
  # train and test sets
  train.idx= sample(seqnr, frac*nr)
  test.idx=  setdiff(seqnr, train.idx)
  X.train=   data.set[train.idx, -target.idx]
  y.train=   data.set[train.idx,  target.idx]
  X.test=    data.set[test.idx,  -target.idx]
  y.test=    data.set[test.idx,   target.idx]
  
  # list of data frames
  tts= vector('list', 4)
  names(tts)= c('X_train', 'y_train', 'X_test', 'y_test')
  tts[[1]]= X.train
  tts[[2]]= y.train
  tts[[3]]= X.test
  tts[[4]]= y.test
  
  return(tts)
}

## read data
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## list of categorical/binary variables
## gender, choles, glucose, smoke, alcohol, active, cardio

## factorise categorical/binary variables
colNames.cat= c('gender', 'choles', 'glucose', 'smoke',
                'alcohol', 'active', 'cardio')
data.set[, colNames.cat]= lapply(data.set[, colNames.cat], factor)

## train/test split
tts= train_test_split(data.set, "cardio", 0.7, seed= 123)
x.train= tts$X_train
y.train= tts$y_train
x.test=  tts$X_test
y.test=  tts$y_test

ridge.train= model.matrix(y.train ~ ., data= x.train)[, -1] # remove intercept
ridge.test=  model.matrix(y.test ~ ., data= x.test)[, -1]

## convert target sets from integer to double (for glmnet)
y.train= as.numeric(as.character(y.train))
y.test=  as.numeric(as.character(y.train))

## optimal values for lambda (10-fold cross-validation)
#alpha0.fit= cv.glmnet(ridge.train, y.train,
#                      type.measure= "deviance", alpha= 0, family= "binomial")
alpha0.fit= cv.glmnet(ridge.train, y.train, alpha= 0)
lambda= alpha0.fit$lambda.min

## predict values on test set
#alpha0.predict= predict(alpha0.fit, s= alpha0.fit$lambda.1se, newx= ridge.test)
# alpha0.predict gives values in ]-2.01, 90.4[
# Maybe these are log(odds)
# TODO: convert predictions to factor/binary (0 or 1)

alpha0.predict= predict(alpha01.fit, s= lambda, newx= ridge.test)
ridge.vals= as.factor(ifelse(alpha0.predict[, 1] > 0.5, 1, 0))
## errors
# TODO: estimate accuracy or error of model