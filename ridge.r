###
## Statistics and Data Analysis project 1
## Ridge Regression
###

## libraries
require(FSA)
require(ggplot2)
require(glmnet)

## seed
SEED= 123
set.seed(SEED)


## load auxiliary functions
source('functions.r')
  

## read clean data set
data.clean= read.csv("./data/cardio-clean.csv")
headtail(data.clean)
data.clean['gender']= ifelse(data.clean['gender'] == 'woman', 0, 1)
data.tib= as_tibble(data.clean)
data.tib


## train/test split
tts= train_test_split(data.clean, "cardio", 0.7, seed= SEED)
x.train= tts$X_train
y.train= as.numeric(as.character(tts$y_train))
x.test=  tts$X_test
y.test=  as.numeric(as.character(tts$y_test))


## use x.train for model fitting
### train.set: dataframe, categorical variables as integers
### train.tib: tibble, categorical variables as factors
train.set= x.train
train.tib= as_tibble(train.set)
colNames.qual= c('choles', 'gluc', 'smoke', 'alco', 'active')
train.tib[, colNames.qual]= lapply(train.tib[, colNames.qual], factor)
train.tib


## prepare x.test for one hot encoding
### test.set: dataframe, categorical variables as integers
### test.tib: tibble, categorical variables as factors
test.set= x.test
test.tib= as_tibble(test.set)
test.tib[, colNames.qual]= lapply(test.tib[, colNames.qual], factor)
test.tib


## convert target sets from integer to double (for glmnet)
y.train= as.numeric(as.character(y.train))
y.test=  as.numeric(as.character(y.test))


## ridge model (train.set as is)
ridge.train.asis= model.matrix(y.train ~ ., data= train.set)[, -1] # remove intercept
ridge.test.asis=  model.matrix(y.test ~ ., data= test.set)[, -1]

### optimal values for lambda (10-fold cross-validation)
#alpha0.fit= cv.glmnet(ridge.train, y.train,
#                      type.measure= "deviance", alpha= 0, family= "binomial")
alpha0.fit.asis= cv.glmnet(ridge.train.asis, y.train, alpha= 0)
lambda.asis= alpha0.fit.asis$lambda.min

### predict values on train set
alpha0.predict.asis.train= predict(alpha0.fit.asis,
                                   s= lambda.asis,
                                   newx= ridge.train.asis)
ridge.vals.asis.train= as.factor(
  ifelse(alpha0.predict.asis.train[, 1] > 0.5, 1, 0)
  )
#### accuracy
cm.ridge.asis.train= as.matrix(
  table(actual= y.train,
        predicted= ridge.vals.asis.train)
)
accu.ridge.train.asis= sum(diag(cm.ridge.asis.train)) / length(y.train)
accu.ridge.train.asis
# Accuracy on the train set: 0.721

### predict values on test set
#alpha0.predict= predict(alpha0.fit, s= alpha0.fit$lambda.1se, newx= ridge.test)
# alpha0.predict gives values in ]-2.01, 90.4[
# Maybe these are log(odds)
# TODO: convert predictions to factor/binary (0 or 1)
alpha0.predict.asis.test= predict(alpha0.fit.asis,
                                  s= lambda.asis,
                                  newx= ridge.test.asis)
ridge.vals.asis.test= as.factor(
  ifelse(alpha0.predict.asis.test[, 1] > 0.5, 1, 0)
)
#### accuracy
cm.ridge.asis.test= as.matrix(
  table(actual= y.test,
        predicted= ridge.vals.asis.test)
)
accu.ridge.test.asis= sum(diag(cm.ridge.asis.test)) / length(y.test)
accu.ridge.test.asis
# Accuracy on the train set: 0.723


## plots
lda.plot(test.set, ridge.vals.asis.test, "age", "weight")
lda.plot(test.set, ridge.vals.asis.test, "age", "height")
lda.plot(test.set, ridge.vals.asis.test, "aplo", "aphi")
lda.plot(test.set, ridge.vals.asis.test, "weight", "height")
