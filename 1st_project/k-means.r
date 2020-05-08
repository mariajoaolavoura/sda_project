###
## Statistics and Data Analysis project 1
## K-Means Clustering
###

## libraries
require(FSA)
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


## k-means
kmax= 20
clusters= vector(mode= "list", length= kmax)
clusters[[1]]= kmeans(train.set, 1)
diffs= array(dim= kmax)
diffs[1]= 0
tots= array(dim= kmax)
tots[1]= kmeans(train.set, 1)$tot.withinss

for (k in 2:kmax) {
  tmp= kmeans(train.set, k, nstart= 25)
  clusters[[k]]= tmp
  tots[k]= tmp$tot.withinss
  diffs[k]= clusters[[k-1]]$tot.withinss - tmp$tot.withinss
}


ggplot() +
  geom_line(aes(x= 1:20, y= diffs), size= 1.2, col= "blue") +
  labs(x= "k", y= "Variation")

ggplot() +
  geom_line(aes(x= 1:20, y= b), size= 1.2, col= "blue") +
  labs(x= "k", y= "Variation")
