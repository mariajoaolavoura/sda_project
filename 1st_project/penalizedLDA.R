# Lasso applied to LDA - Penalized LDA

## libraries
require(penalizedLDA)
require(FSA)

## seed
seed = 123
set.seed(seed)

## number of classes in target variable
n.groups = 2

## read data
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## list of categorical/binary variables
## gender, choles, glucose, smoke, alcohol, active, cardio

## convert categorical/binary variables to factor
# colNames.cat= c('gender', 'choles', 'glucose', 'smoke',
#                 'alcohol', 'active', 'cardio')
#data.set[, colNames.cat]= lapply(data.set[, colNames.cat], factor)

# ## extract variables
# age=     data.set$age
# gender=  data.set$gender
# height=  data.set$height/100 # in metres
# weight=  data.set$weight
# aphi=    data.set$aphi
# aplo=    data.set$aplo
# choles=  data.set$choles
# glucose= data.set$glucose
# smoke=   data.set$smoke
# alcohol= data.set$alcohol
# active=  data.set$active
# cardio=  data.set$cardio

## split data
nr= nrow(data.set)
frac= 0.7
train.idx= sample(1:nr, frac*nr)
train= data.set[train.idx, ]
test= data.set[-train.idx, ]

## remove height from data sets
#train$height= NULL
#test$height= NULL

train.x = scale(train[,-12])
train.y = as.matrix(as.numeric(train$cardio+1)) # must be encoded as 1, 2, ...
test.x = scale(test[,-12])
test.y = as.matrix(as.numeric(test$cardio+1))

## model - erro
penaliz_lda.model= PenalizedLDA(x=train.x, y=train.y, xte=test.x, K=n.groups)

