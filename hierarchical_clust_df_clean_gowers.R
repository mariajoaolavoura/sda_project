# Hierarchical clustering - df clean

## libraries
require(scorecard) # split_df
require(FSA)
require(factoextra) # fviz_dend
require(dplyr) # %>%
require(class)
require(caret)
require(dendextend) #circle dendogram
require(circlize) #circle dendogram
require(cluster)


## seed
seed = 123
set.seed(seed)

## split ratio
split.ratio = c(0.7, 0.3)

## number of classes in target variable
n.groups = 2

## method
method = "ward.D2"

## distance metric
metric = "gower"

## colors
colors =  c("#00AFBB","#FC4E07")

## functions
accFromCm = function(pred, true) { confusionMatrix(pred, true)$overall[1] }

factorize.features = function(dataset){
  dataset = as_tibble(dataset)
  
  colNames.qual= c('gender', 'choles', 'gluc', 'smoke',
                   'alco', 'active', 'cardio')
  dataset[, colNames.qual]= lapply(dataset[, colNames.qual], factor)
  
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

reduce.data.set = function(dataset, leng, seed){
  set.seed(seed)
  dataset$cardio = as.factor(dataset$cardio)
  dataset = dataset %>%
    group_by(cardio) %>%
    sample_n(size = (leng/2) )
  return(dataset)
}

standardize.data.set = function(dataset){
  # cannot have factorized data, must be numeric
  dataset = unfactorize.features(dataset)
  
  # standardization
  dataset = scale(dataset)
  
  return(dataset) # return(data.frame(dataset))
}


feature.selection.with.t.stat = function(dataset){
  dataset = as_tibble(dataset)
  s=c(rep(0,11)) # vector to store the values of t statistic
  for(i in 1:11){
    s[i] = t.test(dataset[dataset$cardio==0,i],
                  dataset[dataset$cardio==1,i],
                  var.equal=TRUE)$statistic
  }
  
  # we want the biggest t statistic
  b = order(abs(s))
  print(names(dataset[,b[1:3]])) # removed ones
  return(names(dataset[,b[4:11]])) #removing the 3 lowest
}

get.hclust.train.test.error = function(model, n.groups, x.train, x.test, y.train, y.test, title){
  # based on https://stackoverflow.com/questions/21064315/how-do-i-predict-new-datas-cluster-after-clustering-training-data
  groups = cutree(model, k=n.groups)
  groups = groups-1
  #table(groups)
  pred.train = knn(train=x.train, test=x.train, cl=groups, k=1)
  pred.test = knn(train=x.train, test=x.test, cl=groups, k=1)
  
  return(list(title, accFromCm(pred.train, y.train),accFromCm(pred.test, y.test)))
}

plot.image.plot = function(x, features, main){
  
  image(1:ncol(x), 1:nrow(x), t(x), axes=F)
  axis(2)
  image.plot(1:ncol(x), 1:nrow(x),
             t(x),
             col = tim.colors(500),
             #legend.only = T,
             xlab = "Features",
             ylab="Patients",
             main = main,
             cex.lab=1, xaxt='n')
  axis(1, at=seq(1,length(features),1), labels=features)
}

#############################################
#############################################
# Gowers distance
setwd("C:/Users/mjlav/MEOCloud/Universidade/mestrado_up/ano1/statistic_data_analysis/project/sda_project")

## read data
data.set= read.csv("./data/cardio-clean.csv")
data.set['gender']= ifelse(data.set['gender'] == 'woman', 0, 1)
headtail(data.set)

## dimension reduction
data.set = reduce.data.set(data.set, 500, seed)

## factorization
data.set = factorize.features(data.set)

## split data
tts = split_df(data.set, ratio = split.ratio, seed = seed)


## label colors represent true value
maped.true.values = as.numeric(tts$train$cardio) # as.numeric because colors must be positive

#par( oma= c(0, 0, 0, 0))

##################################
## COMPLETE MODEL
x.train.1 = tts$train[,-12]
x.test.1 = tts$test[,-12]

# model using the ward.D2 method and gower dist
gower.dist.1 = daisy(x.train.1, metric =metric)
hc.mod.1 = hclust(gower.dist.1, method=method)


# color using kmeans cluster
km.clust.1 = kmeans(x.train.1, n.groups)$cluster

label.colors.1 = colors[km.clust.1[hc.mod.1$order]]
fviz_dend(hc.mod.1, k = n.groups, 
          k_colors = colors, 
          label_cols = label.colors.1,
          cex = 0.6, main="Labels K-Means coloring")



## PATIENTS ORDER
patients.order.1 = hc.mod.1$order
label.colors.1 = colors[maped.true.values[hc.mod.1$order]]
# draw the dendrogram.
fviz_dend(hc.mod.1, k=n.groups, cex=0.5, 
          k_colors = colors, 
          label_cols = label.colors.1,
          ggtheme=theme_minimal(),  main="Labels true value coloring ")



##################################
## REMOVE GENDER
x.train.2 = tts$train[,-c(2, 12)]
x.test.2 = tts$test[,-c(2, 12)]


# model using the ward.D2 method and gower dist
gower.dist.2 = daisy(x.train.2, metric =metric)
hc.mod.2 = hclust(gower.dist.2, method=method)


# color using kmeans cluster
km.clust.2 = kmeans(x.train.2, n.groups)$cluster

label.colors.2 = colors[km.clust.2[hc.mod.2$order]]
fviz_dend(hc.mod.2, k = n.groups, 
          k_colors = colors, 
          label_cols = label.colors.2,
          cex = 0.6, main="Labels k means coloring")



## PATIENTS ORDER
patients.order.2 = hc.mod.2$order

label.colors.2 = colors[maped.true.values[hc.mod.2$order]]
# draw the dendrogram.
fviz_dend(hc.mod.2, k=n.groups, cex=0.5, 
          k_colors = colors, 
          label_cols = label.colors.2,
          ggtheme=theme_minimal(),  main="Labels true value coloring ")



##################################
## REMOVE HEIGHT
x.train.3 = tts$train[,-c(3, 12)]
x.test.3 = tts$test[,-c(3, 12)]


# model using the ward.D2 method and gower dist
gower.dist.3 = daisy(x.train.3, metric =metric)
hc.mod.3 = hclust(gower.dist.3, method=method)


# color using kmeans cluster
km.clust.3 = kmeans(x.train.3, n.groups)$cluster

label.colors.3 = colors[km.clust.3[hc.mod.3$order]]
fviz_dend(hc.mod.3, k = n.groups, 
          k_colors = colors, 
          label_cols = label.colors.3,
          cex = 0.6, main="Labels k means coloring")



## PATIENTS ORDER
patients.order.3 = hc.mod.3$order

label.colors.3 = colors[maped.true.values[hc.mod.3$order]]
# draw the dendrogram.
fviz_dend(hc.mod.3, k=n.groups, cex=0.5, 
          k_colors = colors, 
          label_cols = label.colors.3,
          ggtheme=theme_minimal(),  main="Labels true value coloring ")



##################################
## REMOVE GENDER AND HEIGHT

x.train.4 = tts$train[,-c(2,3,12)]
x.test.4 = tts$test[,-c(2,3,12)]


# model using the ward.D2 method and gower dist
gower.dist.4 = daisy(x.train.4, metric =metric)
hc.mod.4 = hclust(gower.dist.4, method=method)



# Plot with kmeans cluster coloring
km.clust.4 = kmeans(x.train.4, n.groups)$cluster

label.colors.4 = colors[km.clust.4[hc.mod.4$order]]
fviz_dend(hc.mod.4, k = n.groups, 
          k_colors = colors, 
          label_cols = label.colors.4,
          cex = 0.6, main="Labels k means coloring")



## PATIENTS ORDER
patients.order.4 = hc.mod.4$order

label.colors.4 = colors[maped.true.values[hc.mod.4$order]]
# draw the dendrogram.
fviz_dend(hc.mod.4, k=n.groups, cex=0.5, 
          k_colors = colors, 
          label_cols = label.colors.4,
          ggtheme=theme_minimal(),  main="Labels true value coloring ")



##################################
### train test error

hc.tt.res = data.frame(0,0,0)
names(hc.tt.res) = c("method", "train.accuracy", "test.accuracy")


hc.tt.res[1,] = get.hclust.train.test.error(hc.mod.1, n.groups,
                                            x.train.1, x.test.1,
                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                            'complete model')

hc.tt.res[nrow(hc.tt.res)+1,] = get.hclust.train.test.error(hc.mod.2, n.groups,
                                                            x.train.2, x.test.2,
                                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                                            'remove gender')


hc.tt.res[nrow(hc.tt.res)+1,] = get.hclust.train.test.error(hc.mod.3, n.groups,
                                                            x.train.3, x.test.3,
                                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                                            'remove height')

hc.tt.res[nrow(hc.tt.res)+1,] = get.hclust.train.test.error(hc.mod.4, n.groups,
                                                            x.train.4, x.test.4,
                                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                                            'remove gender and height')
## Gower distance
hc.tt.res
#                     method train.accuracy test.accuracy
# 1           complete model      0.5126050     0.5594406
# 2            remove gender      0.5826331     0.5174825
# 3            remove height      0.5182073     0.5874126
# 4 remove gender and height      0.5826331     0.4825175
