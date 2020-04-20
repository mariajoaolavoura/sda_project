###
## Statistics and Data Analysis project 1
## Linear Discriminat Analysis (LDA)
###


## libraries
require(FSA)
require(MASS)
require(ggplot2)


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
colNames.qual= c('choles', 'gluc', 'smoke', 'alco', 'active')
train.tib= as_tibble(train.set)
train.tib[, colNames.qual]= lapply(train.tib[, colNames.qual], factor)
train.tib


## prepare x.test for one hot encoding
### test.set: dataframe, categorical variables as integers
### test.tib: tibble, categorical variables as factors
test.set= x.test
test.tib= as_tibble(test.set)
test.tib[, colNames.qual]= lapply(test.tib[, colNames.qual], factor)
test.tib


## lda model (train.set as is)
cardio= y.train
lda.model.asis= lda(cardio ~ ., data= train.set)
## predictions
### train set
lda.data.asis.train= predict(lda.model.asis, newdata= train.set)
lda.values.asis.train= lda.data.asis.train$class
#### accuracy
cm.lda.asis.train= as.matrix(
  table(actual= y.train,
        predicted= lda.values.asis.train)
  )
accu.lda.train.asis= sum(diag(cm.lda.asis.train)) / length(y.train)
accu.lda.train.asis
# Accuracy on the train set: 0.722
### test set
lda.data.asis.test= predict(lda.model.asis, newdata= test.set)
lda.values.asis.test= lda.data.asis.test$class
#### accuracy
cm.lda.asis.test= as.matrix(
  table(actual= y.test,
        predicted= lda.values.asis.test)
  )
accu.lda.test.asis= sum(diag(cm.lda.asis.test)) / length(y.test)
accu.lda.test.asis
# Accuracy on the test set: 0.723

## plots
lda.plot(test.set, lda.values.asis, "age", "weight")
lda.plot(test.set, lda.values.asis, "age", "height")
lda.plot(test.set, lda.values.asis, "aplo", "aphi")
lda.plot(test.set, lda.values.asis, "weight", "height")


## lda model (train.tib)
lda.model.tib= lda(cardio ~ ., data= train.tib)
## predictions
### train set
lda.data.tib.train= predict(lda.model.tib, newdata= train.tib)
lda.values.tib.train= lda.data.tib.train$class
#### accuracy
cm.lda.tib.train= as.matrix(
  table(actual= y.train,
        predicted= lda.values.tib.train)
  )
accu.lda.train.tib= sum(diag(cm.lda.tib.train)) / length(y.train)
accu.lda.train.tib
# Accuracy on the train set: 0.723
### test set
lda.data.tib.test= predict(lda.model.tib, newdata= test.tib)
lda.values.tib.test= lda.data.tib.test$class
#### accuracy
cm.lda.tib.test= as.matrix(
  table(actual= y.test,
        predicted= lda.values.tib.test)
  )
accu.lda.test.tib= sum(diag(cm.lda.tib.test)) / length(y.test)
accu.lda.test.tib
# Accuracy on the test set: 0.724

## plots
lda.plot(test.tib, lda.values.tib, "age", "weight")
lda.plot(test.tib, lda.values.tib, "weight", "height")


## one hot encoding
### one hot encoding on train set
encoder.train= onehot(train.tib)
train.enc= as.data.frame(predict(encoder.train, train.tib))
train.enc$cardio= y.train
head(train.enc)
train.enc.tib= as_tibble(train.enc)
train.enc.tib


## one hot encoding on test set
encoder.test= onehot(test.tib)
test.enc= as.data.frame(predict(encoder.test, test.tib))
test.enc$cardio= y.test
head(test.enc)
test.enc.tib= as_tibble(test.enc)
test.enc.tib


## correlations
ggcorr(
  train.enc.tib,
  name= "Correlation",
  label= T
  ) +
  labs(title= "Correlation matrix (train set, one hot enconded variables)") +
  theme(plot.title= element_text(face= "bold", hjust= 0.5))
ggcorr(
  test.enc.tib,
  name= "Correlation",
  label= T
  ) +
  labs(title= "Correlation matrix (test set, one hot enconded variables)") +
  theme(plot.title= element_text(face= "bold", hjust= 0.5))


## lda model on one hot encoded train set
lda.model.enc= lda(cardio ~ ., data= train.enc.tib)
# There is a warning, complaining that there are collinear variables.
# We saw from the correlation matrix that many predictors are correlated after
# one hot encoding. This is might yield bad results because LDA, like
# regression techniques involves matrix inversion, which is inaccurate if the
# determinant is close to 0 (i.e. two or more variables are almost a linear
# combination of each other).
## predictions
### train set
lda.data.enc.tib.train= predict(lda.model.enc, newdata= train.enc.tib)
lda.values.enc.tib.train= lda.data.enc.tib.train$class
#### accuracy
cm.lda.enc.tib.train= as.matrix(
  table(actual= y.train,
        predicted= lda.values.enc.tib.train)
  )
accu.lda.train.enc.tib= sum(diag(cm.lda.enc.tib.train)) / length(y.train)
accu.lda.train.enc.tib
# Accuracy on the train set: 0.723
### test set
lda.data.enc.tib.test= predict(lda.model.enc, newdata= test.enc.tib)
lda.values.enc.tib.test= lda.data.enc.tib.test$class
#### accuracy
cm.lda.enc.tib.test= as.matrix(
  table(actual= y.test,
        predicted= lda.values.enc.tib.test)
  )
accu.lda.test.enc.tib= sum(diag(cm.lda.enc.tib.test)) / length(y.test)
accu.lda.test.enc.tib
# Accuracy on the test set: 0.724
# No change for the case where the variables are not encoded.


## plots
lda.plot(test.enc.tib, lda.values.enc.tib.test, "age", "weight")
lda.plot(test.enc.tib, lda.values.enc.tib.test, "weight", "height")


