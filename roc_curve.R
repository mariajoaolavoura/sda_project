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
require(dplyr) # %>%
require(class)


## seed
seed=123
set.seed(seed)

## split ratio
split.ratio = c(0.7, 0.3)

## colors
cols= c("firebrick2", "olivedrab4", "lightseagreen", "slateblue1", "mediumorchid1")

## functions
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

reduce.data.set = function(dataset, leng, seed){
  set.seed(seed)
  dataset$cardio = as.factor(dataset$cardio)
  dataset = dataset %>%
    group_by(cardio) %>%
    sample_n(size = (leng/2) )
  return(dataset)
}

## read data set
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## factorization
data.set = factorizefeatures(data.set)

## split data
tts = split_df(data.set, ratio=split.ratio, seed=seed)


## models
#LogR
logr.mod = glm(cardio ~., data=tts$train, family = "binomial")
logr.pred= predict(logr.mod, tts$test)
#logr.pred = as.factor(ifelse(logr.pred > 0.50, 1, 0))

#LDA
lda.mod = lda(cardio ~ ., data= tts$train)
lda.pred = predict(lda.mod, newdata= tts$test)$posterior

#QDA
qda.mod = qda(cardio ~ ., data= tts$train)
qda.pred = predict(qda.mod, newdata= tts$test)$posterior

# matrixes
x.train = model.matrix(cardio~., tts$train)[,-1]
x.test = model.matrix(cardio~., tts$test)[,-1]
y.train = as.numeric(as.character(tts$train$cardio))
y.test = as.numeric(as.character(tts$test$cardio))

#Lasso
lasso.cv = cv.glmnet(x.train, y.train, alpha=1)
lasso.best.lambda = lasso.cv$lambda.min
lasso.mod = glmnet(x.train, y.train, alpha=1)
lasso.pred = predict(lasso.mod, s=lasso.best.lambda, newx=x.test)
#lasso.pred = as.factor(ifelse(lasso.pred > 0.50, 1, 0))

#Ridge
ridge.cv = cv.glmnet(x.train, y.train, alpha=0)
ridge.best.lambda = ridge.cv$lambda.min
ridge.mod = glmnet(x.train, y.train, alpha=0)
ridge.pred = predict(ridge.mod, s=ridge.best.lambda, newx=x.test)
#ridge.pred = as.factor(ifelse(ridge.pred > 0.50, 1, 0))


# #KMeans
# kmeans.mod
# kmeans.pred
# 

# #Hierarchical Clust
## dimension reduction
ds = reduce.data.set(data.set, 35000, seed)
## split data
tts2 = split_df(ds, ratio=split.ratio, seed=seed)

gower.dist = daisy(tts2$train[,-12], metric ="gower")
hc.mod = hclust(gower.dist, method="ward.D2")
groups = cutree(hc.mod, k=2)-1
hc.pred = knn(train=tts2$train[,-12], test=tts2$test[,-12], cl=groups, k=1, prob= T)
hc.pred = attributes(hc.pred)$prob



## ROC
y.test = as.numeric(tts$test$cardio)
logr.roc= roc(y.test, logr.pred, quiet=T)
lda.roc=  roc(y.test, lda.pred[, 2], quiet=T)
qda.roc=  roc(y.test, qda.pred[, 2], quiet=T)
lasso.roc= roc(y.test, lasso.pred, quiet=T)
ridge.roc= roc(y.test, ridge.pred, quiet=T)
#kmeans.roc= roc(y.test, kmens.pred, quiet=T)
hc.roc = roc(as.numeric(tts2$test$cardio), hc.pred, quiet=T)

roc.mods= list(
  LogR  = logr.roc,
  LDA   = lda.roc,
  QDA   = qda.roc,
  Lasso = lasso.roc,
  Ridge = ridge.roc,
  #KMeans = kmeans.roc,
  HC    = hc.roc
)
# LDA == Lasso == Ridge
# LogR != LDA != QDA != HC


# # AUCs
# auc.lda= auc(lda.roc)
# auc.lda.per= paste0("AUC~(LDA):~~", round(100*auc.lda, 1), "*\'%\'")
# auc.qda= auc(qda.roc)
# auc.qda.per= paste0("AUC~(QDA):~~", round(100*auc.qda, 1), "*\'%\'")
# auc.logr= auc(logr.roc)
# auc.logr.per= paste0("AUC~(LogR):~", round(100*auc.logr, 1), "*\'%\'")
# 
# auc.labels= list(
#   auc.logr.per,
#   auc.lda.per,
#   auc.qda.per
# )

theme_set(theme_bw())
ggroc(
  roc.mods,
  size= 0.8,
  legacy.axes= T #normal order axes
) +
  # annotate(
  #   geom= "text",
  #   x= c(0.6, 0.6, 0.6, 0.6, 0.6),
  #   y= c(0.75, 0.7, 0.65, 0.6, 0.55),
  #   size=2,
  #   label= auc.labels,
  #   col= cols5,
  #   parse= T,
  #   hjust= 0
  # ) +
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

