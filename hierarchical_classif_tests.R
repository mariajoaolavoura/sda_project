# Hierarchical classification

## libraries
require(scorecard) # split_df
require(FSA)
require(factoextra) # fviz_dend
require(fields) # image.plot
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

unfactorizefeatures = function(dataset){
  dataset$gender = as.numeric(as.character(dataset$gender))
  dataset$choles  = as.numeric(as.character(dataset$choles))
  dataset$glucose = as.numeric(as.character(dataset$glucose))
  dataset$smoke = as.numeric(as.character(dataset$smoke))
  dataset$alcohol = as.numeric(as.character(dataset$alcohol))
  dataset$active  = as.numeric(as.character(dataset$active))
  dataset$cardio = as.numeric(as.character(dataset$cardio))
  
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
  dataset$cardio = as.numeric(as.character(dataset$cardio))
  
  # standardization (imperative)
  dataset = scale(dataset)
  
  return(dataset) # return(data.frame(dataset))
}


feature.selection.with.t.stat = function(dataset){
  dataset = data.frame(dataset)
  s=c(rep(0,11)) # vector to store the values of t statistic
  for(i in 1:11){
    s[i] = t.test(dataset[dataset$cardio==0,i],
                  dataset[dataset$cardio==1,i],
                  var.equal=TRUE)$statistic
  }
  
  # we want the biggest t statistic
  b = order(abs(s))
  print(names(dataset[,b[1:3]])) # removed ones
  return(dataset[,b[4:11]]) #removing the 3 lowest
}

get.hclust.train.test.error = function(model, n.groups, x.train, x.test, y.train, y.test){
  # based on https://stackoverflow.com/questions/21064315/how-do-i-predict-new-datas-cluster-after-clustering-training-data
  groups = cutree(model, k=n.groups)
  groups = groups-1
  #table(groups)
  pred.train = knn(train=x.train, test=x.train, cl=groups, k=1)
  pred.test = knn(train=x.train, test=x.test, cl=groups, k=1)
  
  return(list(accFromCm(pred.train, y.train),accFromCm(pred.test, y.test)))
}

plot.image.plot = function(x, xlab, main){
  image.plot(1:ncol(x), 1:nrow(x), 
             t(x),
             col = tim.colors(500), 
             xlab = xlab,
             ylab="Patients", 
             main = main,
             cex.lab=1)
}

#############################################
#############################################
# Euclidean distance

setwd("C:/Users/mjlav/MEOCloud/Universidade/mestrado_up/ano1/statistic_data_analysis/project/sda_project")
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## dimension reduction
data.set = reduce.data.set(data.set, 50, seed)

## split data
tts = split_df(data.set, ratio = split.ratio, seed = seed)

## standardize the data for eucidean distance
std.train = standardize.data.set(tts$train)
std.test = standardize.data.set(tts$test)

###
## complete model
x.train.1 = std.train[,-12]
x.test.1 = std.test[,-12]

# plot the data, diseases in rows and predictors in columns
plot.image.plot(x.train.1,
                "age,gender,height,weight,aphi,aplo,choles,glucose,smoke,alcohol,active",
                "main" )
heatmap(x.train.1)
# The rows are ordered based on the order of the hierarchical clustering. 
# The colored bar indicates the cardio category each row belongs to. 
# The color in the heatmap indicates the length of each measurement 
# (from light yellow to dark red).


# plot dendograms for different methods
# label colors represent true value
colors =  c("#00AFBB","#FC4E07")
maped.true.values = as.numeric(tts$train$cardio) # as.numeric because colors must be positive

df = data.frame(0,0,0,0)
names(df) = c("method", "order", "dendogram", "accuracy")

methods.list = list("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
dist = daisy(x.train.1, metric="euclidean")

for(m in methods.list){
  print(m)
  hier.mod = hclust(dist, method=m)
  label.colors = colors[maped.true.values[hier.mod$order]]
  d = fviz_dend(hier.mod, k=n.groups, cex=0.5, 
                #k_colors = colors, 
                label_cols = label.colors,
                #horiz = T,
                ggtheme=theme_minimal(),
                main=m)
  
  print(d)
  df[nrow(df)+1,] = list(m,
                         list(hier.mod$order),
                         list(d),
                         list(get.hclust.train.test.error(hier.mod, n.groups,
                                                          x.train.1, x.test.1,
                                                          as.factor(tts$train$cardio),as.factor(tts$test$cardio))))
}
# by looking at plots (data.set size = 50), ward and ward.d2 are the best
df$accuracy
# the accuracy of Ward.D2 is the best

euclidean.dist = daisy(x.train.1, metric ="euclidean")
hier.mod = hclust(euclidean.dist, method="ward.D2")

# color using kmeans cluster
km.clust <- kmeans(x.train.1, n.groups)$cluster

label.colors = colors[km.clust[hier.mod$order]]
fviz_dend(hier.mod, k = n.groups, 
          #k_colors = colors, 
          label_cols = label.colors,
          cex = 0.6, main="Ward.D2 - k means coloring")



# do hierarchical classification using the ward.D2 method
# patients order
patients.order = hier.mod$order

label.colors = colors[maped.true.values[hier.mod$order]]
# draw the dendrogram.
fviz_dend(hier.mod, k=n.groups, cex=0.5, 
          #k_colors = colors, 
          label_cols = label.colors,
          ggtheme=theme_minimal(),  main="Ward.D2 - true coloring")

dend = as.dendrogram(hier.mod)
par(mar = rep(0,4))
circlize_dendrogram(dend)

# heatmap
plot.image.plot(x.train.1[patients.order,],
                "age,gender,height,weight,aphi,aplo,choles,glucose,smoke,alcohol,active",
                "patients order" )


# predictors order
euclid.dist.pred.1 = dist(t(x.train.1)) # euclidean distance
hier.mod.pred.1 = hclust(euclid.dist.pred.1, method="average")
predictors.order = hier.mod.pred.1$order

# draw the dendrogram.
fviz_dend(hier.mod.pred.1, k=n.groups, cex=0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k=TRUE, ggtheme=theme_minimal())

dend.pred.1 = as.dendrogram(hier.mod.pred.1)
par(mar = rep(0,4))
circlize_dendrogram(dend.pred.1)

# heatmap
plot.image.plot(x.train.1[,predictors.order],
                "aplo,aphi,age,choles,glucose,active,weight,gender,height,smoke,alcohol", 
                "predictors order" )


# xlab must be equal to names(data.set[,predictors.order])
# patients and predictors order
plot.image.plot(x.train.1[patients.order,predictors.order],
                "aplo,aphi,age,choles,glucose,active,weight,gender,height,smoke,alcohol", 
                "patients and predictors order" )




######################################################################
######################################################################
# Gowers distance

setwd("C:/Users/mjlav/MEOCloud/Universidade/mestrado_up/ano1/statistic_data_analysis/project/sda_project")
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## dimension reduction
data.set = reduce.data.set(data.set, 50, seed)

## factorize the data for gowers distance (so the categorical variables are treated with nominal scale)
data.set = factorizefeatures(data.set)

## split data
tts = split_df(data.set, ratio = split.ratio, seed = seed)

###
## complete model
x.train.1 = tts$train[,-12]
x.test.1 = tts$test[,-12]

# plot the data, diseases in rows and predictors in columns
# plot.image.plot(x.train.1,
#                 "age,gender,height,weight,aphi,aplo,choles,glucose,smoke,alcohol,active",
#                 "main" )
# heatmap(x.train.1)
# The rows are ordered based on the order of the hierarchical clustering. 
# The colored bar indicates the cardio category each row belongs to. 
# The color in the heatmap indicates the length of each measurement 
# (from light yellow to dark red).


# plot dendograms for different methods
colors =  c("#00AFBB","#FC4E07")
maped.true.values = as.numeric(tts$train$cardio) # as.numeric because colors must be positive
label.colors = colors[maped.true.values[hier.mod$order]]

df = data.frame(0,0,0,0)
names(df) = c("method", "order", "dendogram", "accuracy")

methods.list = list("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
gower.dist = daisy(x.train.1, metric ="gower")

for(m in methods.list){
  print(m)
  hier.mod = hclust(gower.dist, method=m)
  d = fviz_dend(hier.mod, k=n.groups, cex=0.5, 
                k_colors = colors,
                label_cols = label.colors,
                #horiz = T,
                ggtheme=theme_minimal(),
                main=m)
  
  print(d)
  df[nrow(df)+1,] = list(m,
                         list(hier.mod$order),
                         list(d),
                         list(get.hclust.train.test.error(hier.mod, n.groups,
                                                     x.train.1, x.test.1,
                                                     as.factor(tts$train$cardio),as.factor(tts$test$cardio))))
}


df$accuracy


# color using kmeans cluster
km.clust <- kmeans(x.train.1, n.groups)$cluster
gower.dist = daisy(x.train.1, metric ="gower")
hier.mod = hclust(gower.dist, method="mcquitty")
fviz_dend(hier.mod, k = n.groups, 
          k_colors = c("#00AFBB","#FC4E07"),
          label_cols =  km.clust[hier.mod$order], cex = 0.6)



# do hierarchical classification using the average link
# patients order
method="average"
gower.dist.pat.1 = daisy(x.train.1, metric ="gower")
hier.mod.pat.1 = hclust(gower.dist.pat.1, method=method)
patients.order = hier.mod.pat.1$order

# draw the dendrogram.
fviz_dend(hier.mod, k=n.groups, cex=0.5, 
          k_colors = colors,
          label_cols = label.colors,
          #horiz = T,
          ggtheme=theme_minimal(),
          main=method)

# dend.pat.1 = as.dendrogram(hier.mod.pat.1)
# par(mar = rep(0,4))
# circlize_dendrogram(dend.pat.1)

# heatmap
# works better with standardized variables...
plot.image.plot(unfactorizefeatures(x.train.1[patients.order,]),
                "age,gender,height,weight,aphi,aplo,choles,glucose,smoke,alcohol,active",
                "patients order" )


# # predictors order
# euclid.dist.pred.1 = dist(t(x.train.1)) # euclidean distance
# hier.mod.pred.1 = hclust(gower.dist.pred.1, method="average")
# predictors.order = hier.mod.pred.1$order
# 
# # draw the dendrogram.
# fviz_dend(hier.mod.pred.1, k=n.groups, cex=0.5, k_colors = c("#00AFBB","#FC4E07"),
#           color_labels_by_k=TRUE, ggtheme=theme_minimal())
# 
# dend.pred.1 = as.dendrogram(hier.mod.pred.1)
# par(mar = rep(0,4))
# circlize_dendrogram(dend.pred.1)
# 
# # heatmap
# plot.image.plot(x.train.1[,predictors.order],
#                 "aplo,aphi,age,choles,glucose,active,weight,gender,height,smoke,alcohol", 
#                 "predictors order" )
# 
# 
# # xlab must be equal to names(data.set[,predictors.order])
# # patients and predictors order
# plot.image.plot(x.train.1[patients.order,predictors.order],
#                 "aplo,aphi,age,choles,glucose,active,weight,gender,height,smoke,alcohol", 
#                 "patients and predictors order" )



