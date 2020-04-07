# Hierarchical classification

## libraries
require(scorecard) # split_df
require(FSA)
require(factoextra) # fviz_dend
require(fields) # image.plot
require(dplyr) # %>%
require(class)
require(caret)


## seed
seed = 123
set.seed(seed)

## split ratio
split.ratio = c(0.7, 0.3)

## number of classes in target variable
n.groups = 2


## functions
accFromCm = function(pred, true) { confusionMatrix(pred, true)$overall[1] }

reduce.data.set = function(dataset, leng){
                    dataset$cardio = as.factor(dataset$cardio)
                    dataset = dataset %>%
                                group_by(cardio) %>%
                                sample_n(size = (leng/2) )
                    return(dataset)
}

standardize.data.set = function(dataset){
                        # cannot have factorized data, must be numeric
                        dataset$cardio = as.numeric(dataset$cardio)-1
                        
                        # standardization (imperative)
                        dataset = scale(dataset)
                        
                        return(data.frame(data.set))
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
  print(s)
  print(b)
  print(names(dataset[,b[1:3]]))
  return(dataset[,b[4:11]]) #removing the 3 lowest
}

get.train.test.error = function(model, n.groups, x.train, x.test, y.train, y.test, title){
  # based on https://stackoverflow.com/questions/21064315/how-do-i-predict-new-datas-cluster-after-clustering-training-data
  groups = cutree(model, k=n.groups)
  groups = groups-1
  table(groups)
  pred.train = knn(train=x.train, test=x.train, cl=groups, k=1)
  pred.test = knn(train=x.train, test=x.test, cl=groups, k=1)
  
  return(list(title,accFromCm(pred.train, y.train),accFromCm(pred.test, y.test)))
}


#############################################
## read data - no transformations on the data)
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## dimension reduction
# nrow(data.set[data.set$cardio==0,])/nrow(data.set)
# since there is ~50% instances with cardio diseases
# lets create a smaller df (144 instances) keeping that ratio
data.set = reduce.data.set(data.set, 144)

data.set = data.set[1:144, ]

## standardize the data
#data.set = standardize.data.set(data.set)

## split data
tts = split_df(data.set, ratio = split.ratio, seed = seed)




###
## complete model
x.train.1 = tts$train[,-12]

t = data.frame(tts$test)
x.test.1 = t[, names(x.train.1)]

# plot the data, diseases in rows and predictors in columns
image.plot(1:ncol(x.train.1), 1:nrow(x.train.1), t(x.train.1), # t(train) matrix transpose
           col=tim.colors(11),
           xlab="age,gender,height,weight,aphi,aplo,choles,glucose,smoke,alcohol,active", ylab="No. cardio disease", 
           cex.lab=1)


# do hierarchical classification using the average link
euclid.dist.1 = dist(x.train.1) # euclidean distance
hier.mod.1 = hclust(euclid.dist.1, method="average")

# draw the dendrogram.
fviz_dend(hier.mod.1, k =n.groups, cex = 0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())



###
## feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
x.train.2 = tts$train[, -c(2, 9, 10, 12)]
names(x.train.2) # removed gender, height and active

t = data.frame(tts$test)
x.test.2 = t[, names(x.train.2)]

# plot the data, diseases in rows and predictors in columns
image.plot(1:ncol(x.train.2), 1:nrow(x.train.2), t(x.train.2), # t(x) matrix transpose
           col=tim.colors(8),
           xlab="alcohol,glucose,smoke,age,weight,choles,aplo,aphi", ylab="No. cardio disease", 
           cex.lab=1)

# Do hierarchical classification using the average link 
euclid.dist.2 = dist(x.train.2) # euclidean distance
hier.mod.2 = hclust(euclid.dist.2, method="average")

# draw the dendrogram.
fviz_dend(hier.mod.2, k =n.groups, cex = 0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())



###
## feature selection - based on t statistics
x.train.3 = feature.selection.with.t.stat(tts$train)
names(x.train.3)

t = data.frame(tts$test)
x.test.3 = t[, names(x.train.3)]

# plot the data, diseases in rows and predictors in columns
image.plot(1:ncol(x.train.3), 1:nrow(x.train.3), t(x.train.3), # t(x) matrix transpose
           col=tim.colors(8),
           xlab="alcohol,glucose,smoke,age,weight,choles,aplo,aphi", ylab="No. cardio disease", 
           cex.lab=1)

# do hierarchical classification using the average link 
euclid.dist.3 = dist(x.train.3) # euclidean distance
hier.mod.3 = hclust(euclid.dist.3, method="average")

# draw the dendrogram.
fviz_dend(hier.mod.3, k =n.groups, cex = 0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())





### train test error

hier.tt.res = data.frame(0,0,0)
names(hier.tt.res) = c("method", "train.error", "test.error")


hier.tt.res[nrow(hier.tt.res),] = get.train.test.error(hier.mod.1, n.groups,
                                            x.train.1, x.test.1,
                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                            'complete model')

hier.tt.res[nrow(hier.tt.res)+1,] = get.train.test.error(hier.mod.2, n.groups,
                                            x.train.2, x.test.2,
                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                            'feature selection - based on EDA')


hier.tt.res[nrow(hier.tt.res)+1,] = get.train.test.error(hier.mod.3, n.groups,
                                            x.train.3, x.test.3,
                                            as.factor(tts$train$cardio),as.factor(tts$test$cardio),
                                            'feature selection - based on t statistics')

hier.tt.res




#############################################
## read data - no height outliers
data.set2= read.csv("./data/data_set_no_height_out.csv")
headtail(data.set2)

## dimension reduction
data.set2 = data.set2[1:144, ]

## standardize the data
data.set2 = standardize.data.set(data.set2)

## split data
tts2 = split_df(data.set2, ratio = split.ratio, seed = seed)
#indx = sapply(breast, is.factor)
#breast[indx] = lapply(breast[indx], function(x) as.numeric(as.character(x)))



###
## complete model
x.train.4 = tts2$train[,-12]

t = data.frame(tts2$test)
x.test.4 = t[, names(x.train.4)]

# plot the data, diseases in rows and predictors in columns
image.plot(1:ncol(x.train.4), 1:nrow(x.train.4), t(x.train.4), # t(train) matrix transpose
           col=tim.colors(11),
           xlab="age,gender,height,weight,aphi,aplo,choles,glucose,smoke,alcohol,active", ylab="No. cardio disease", 
           cex.lab=1)


# do hierarchical classification using the average link
euclid.dist.4 = dist(x.train.4) # euclidean distance
hier.mod.4 = hclust(euclid.dist.4, method="average")

# draw the dendrogram.
fviz_dend(hier.mod.4, k =n.groups, cex = 0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())


###
## feature selection - based on EDA of cardio.r
# remove gender, smoke and alcohol 
x.train.5 = tts$train[, -c(2, 9, 10, 12)]
names(x.train.5) # removed gender, height and active

t = data.frame(tts2$test)
x.test.5 = t[, names(x.train.5)]

# plot the data, diseases in rows and predictors in columns
image.plot(1:ncol(x.train.5), 1:nrow(x.train.5), t(x.train.5), # t(x) matrix transpose
           col=tim.colors(8),
           xlab="alcohol,glucose,smoke,age,weight,choles,aplo,aphi", ylab="No. cardio disease", 
           cex.lab=1)

# Do hierarchical classification using the average link 
euclid.dist.5 = dist(x.train.5) # euclidean distance
hier.mod.5 = hclust(euclid.dist.5, method="average")

# draw the dendrogram.
fviz_dend(hier.mod.5, k =n.groups, cex = 0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())


###
## feature selection - based on t statistics
x.train.6 = feature.selection.with.t.stat(tts2$train)
names(x.train.6) # removed gender, height and active

t = data.frame(tts2$test)
x.test.6 = t[, names(x.train.6)]

# plot the data, diseases in rows and predictors in columns
image.plot(1:ncol(x.train.6), 1:nrow(x.train.6), t(x.train.6), # t(x) matrix transpose
           col=tim.colors(8),
           xlab="alcohol,glucose,smoke,age,weight,choles,aplo,aphi", ylab="No. cardio disease", 
           cex.lab=1)

# do hierarchical classification using the average link 
euclid.dist.6 = dist(x.train.6) # euclidean distance
hier.mod.6 = hclust(euclid.dist.6, method="average")

# draw the dendrogram.
fviz_dend(hier.mod.6, k =n.groups, cex = 0.5, k_colors = c("#00AFBB","#FC4E07"),
          color_labels_by_k = TRUE, ggtheme = theme_minimal())



hier.tt.res[nrow(hier.tt.res)+1,] = get.train.test.error(hier.mod.4, n.groups,
                                                         x.train.4, x.test.4,
                                                         as.factor(tts2$train$cardio),as.factor(tts2$test$cardio),
                                                         'mod4')

hier.tt.res[nrow(hier.tt.res)+1,] = get.train.test.error(hier.mod.5, n.groups,
                                                         x.train.5, x.test.5,
                                                         as.factor(tts2$train$cardio),as.factor(tts2$test$cardio),
                                                         'mod5')

hier.tt.res[nrow(hier.tt.res)+1,] = get.train.test.error(hier.mod.6, n.groups,
                                                         x.train.6, x.test.6,
                                                         as.factor(tts2$train$cardio),as.factor(tts2$test$cardio),
                                                         'mod6')
hier.tt.res
