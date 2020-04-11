###
## K-Means Clustering
###

## libraries
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

## k-means
kmax= 20
clusters= vector(mode= "list", length= kmax)
clusters[[1]]= kmeans(x.train, 1)
diffs= array(dim= kmax)
diffs[1]= 0
tots= array(dim= kmax)
tots[1]= kmeans(x.train, 1)$tot.withinss

for (k in 2:kmax) {
  tmp= kmeans(x.train, k, nstart= 25)
  clusters[[k]]= tmp
  tots[k]= tmp$tot.withinss
  diffs[k]= clusters[[k-1]]$tot.withinss - tmp$tot.withinss
}


ggplot() +
  geom_line(aes(x= 1:20, y= diffs), size= 1.2, col= "blue") +
  labs(x= "k", y= "Variation")

