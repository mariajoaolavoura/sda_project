## libraries
require(scorecard)
require(FSA)
require(MASS)
require(caret)
require(pROC)
require(ggplot2)
require(glmnet)
require(cluster)
require(caret)
require(dplyr)
require(class)
require(onehot)


## seed
seed=123
set.seed(seed)

## split ratio
split.ratio = c(0.7, 0.3)

## colors
cols= c("firebrick2", "olivedrab4", "lightseagreen", "slateblue1", "mediumorchid1")

## functions
factorize.features = function(dataset){
  dataset$gender= ifelse(dataset['gender'] == 'woman', 0, 1)
  colNames.qual= c('gender', 'choles', 'gluc', 'smoke',
                   'alco', 'active', 'cardio')
  dataset[, colNames.qual]= lapply(dataset[, colNames.qual], factor)
  
  return(dataset)
}

reduce.data.set = function(dataset, leng, seed){
  set.seed(seed)
  dataset$cardio = as.factor(dataset$cardio)
  dataset = dataset %>%
    group_by(cardio) %>%
    sample_n(size = (leng/2) )
  return(dataset)
}

unfactorize.features = function(dataset){
  dataset = as_tibble(dataset)
  
  colNames.qual= c('gender', 'choles', 'gluc', 'smoke',
                   'alco', 'active', 'cardio')
  
  dataset[, colNames.qual]= lapply(dataset[, colNames.qual], as.character)
  dataset[, colNames.qual]= lapply(dataset[, colNames.qual], as.numeric)
  
  return(dataset)
}

standardize.data.set = function(dataset){
  # cannot have factorized data, must be numeric
  dataset = unfactorize.features(dataset)
  
  # standardization
  dataset = scale(dataset)
  
  return(dataset) # return(data.frame(dataset))
}

#################################################
## read data set
data.set= read.csv("./data/cardio-clean.csv")
headtail(data.set)

## factorization
data.set = factorize.features(data.set)

## one hot encoding
encoder= onehot(data.set[, -12])
data.dmy= as.data.frame(predict(encoder, data.set[, -12]))
data.dmy$cardio= data.set$cardio
head(data.dmy)

## split data
tts = split_df(data.set, ratio=split.ratio, seed=seed)
tts.dmy = split_df(data.dmy, ratio=split.ratio, seed=seed)


## complete models
#LogR
logr.mod = glm(cardio ~., data=tts.dmy$train, family = "binomial")
logr.pred= predict(logr.mod, tts.dmy$test)

#LDA
lda.mod = lda(cardio ~ ., data= tts$train)
lda.pred = predict(lda.mod, newdata= tts$test)$posterior

#QDA
# if encoded, gives error
qda.mod = qda(cardio ~ ., data= tts$train)
qda.pred = predict(qda.mod, newdata= tts$test)$posterior

# matrixes
x.train = model.matrix(cardio~., tts.dmy$train)[,-1]
x.test = model.matrix(cardio~., tts.dmy$test)[,-1]
y.train = as.numeric(as.character(tts.dmy$train$cardio))
y.test = as.numeric(as.character(tts.dmy$test$cardio))

#Lasso
lasso.cv = cv.glmnet(x.train, y.train, alpha=1)
lasso.best.lambda = lasso.cv$lambda.min
lasso.mod = glmnet(x.train, y.train, alpha=1)
lasso.pred = predict(lasso.mod, s=lasso.best.lambda, newx=x.test)

#Ridge
ridge.cv = cv.glmnet(x.train, y.train, alpha=0)
ridge.best.lambda = ridge.cv$lambda.min
ridge.mod = glmnet(x.train, y.train, alpha=0)
ridge.pred = predict(ridge.mod, s=ridge.best.lambda, newx=x.test)


## ROC
y.test = as.numeric(tts$test$cardio)
logr.roc= roc(y.test, logr.pred, quiet=T)
lda.roc=  roc(y.test, lda.pred[, 2], quiet=T)
qda.roc=  roc(y.test, qda.pred[, 2], quiet=T)
lasso.roc= roc(y.test, lasso.pred, quiet=T)
ridge.roc= roc(y.test, ridge.pred, quiet=T)

roc.mods= list(
  LogR  = logr.roc,
  LDA   = lda.roc,
  QDA   = qda.roc,
  Lasso = lasso.roc,
  Ridge = ridge.roc
)


# # AUCs
auc.logr= auc(logr.roc)
auc.logr.per= paste0("AUC~(LogR):~", round(100*auc.logr, 1), "*\'%\'")
auc.lda= auc(lda.roc)
auc.lda.per= paste0("AUC~(LDA):~~", round(100*auc.lda, 1), "*\'%\'")
auc.qda= auc(qda.roc)
auc.qda.per= paste0("AUC~(QDA):~~", round(100*auc.qda, 1), "*\'%\'")
auc.lasso= auc(lasso.roc)
auc.lasso.per= paste0("AUC~(Ridge):~", round(100*auc.lasso, 1), "*\'%\'")
auc.ridge= auc(ridge.roc)
auc.ridge.per= paste0("AUC~(Lasso):~", round(100*auc.ridge, 1), "*\'%\'")


auc.labels= list(
  auc.logr.per,
  auc.lda.per,
  auc.qda.per,
  auc.lasso.per,
  auc.ridge.per
)

theme_set(theme_bw())
ggroc(
  roc.mods,
  size= 0.8,
  legacy.axes= T #normal order axes
) +
  annotate(
    geom= "text",
    x= c(0.6, 0.6, 0.6, 0.6, 0.6),
    y= c(0.75, 0.7, 0.65, 0.6, 0.55),
    size=2,
    label= auc.labels,
    col= cols,
    parse= T,
    hjust= 0
  ) +
  labs(
    x= "False Positive Ratio",
    y= "True Positive Ratio",
    col= "Models"
  ) +
  ggtitle("ROC curves and respective AUCs") +
  theme(legend.title= element_text(size= 12)) +
  scale_x_continuous(labels= scales::percent) +
  scale_y_continuous(labels= scales::percent) +
  coord_fixed()

