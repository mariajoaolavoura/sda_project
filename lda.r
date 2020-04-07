require(FSA) # for headtail
require(ggplot2)
require(MASS)

## seed
set.seed(123)

## read data
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

gender= as.factor(data.set$gender)
choles= as.factor(data.set$choles)
glucose= as.factor(data.set$glucose)
smoke= as.factor(data.set$smoke)
alcohol= as.factor(data.set$alcohol)
active= as.factor(data.set$active)

## split data
nr= nrow(data.set)
frac= 0.7
train.idx= sample(1:nr, frac*nr)
train= data.set[train.idx, ]
test= data.set[-train.idx, ]

## remove height from data sets
train$height= NULL
test$height= NULL

## lda model
lda.model= lda(cardio ~ ., data= train)

## predictions
data.lda= predict(lda.model, newdata= test)
data.lda.values= data.lda$class

## create plots
#plot.lda= function(dataset, keyX, keyY, colorKey) {
#  # create data frame with data
#  plot.data= data.frame(
#    dataset[keyX],
#    dataset[keyY],
#    dataset[colorKey]
#  )
#  names(plot.data)= c('a', 'b', 'c')
#  
#  # create grid of points
#  grid= expand.grid(
#    x= seq(min(dataset$a), max(dataset$a + 1),
#           (max(dataset$a) - min(dataset$a))/50),
#    y= seq(min(dataset$b - 1), max(dataset$b + 1),
#           (max(dataset$b) - min(dataset$b))/50)
#  )
#  colnames(grid)= c('a', 'b')
#  lda.mod= lda(c ~., data = dataset)
#  grid$class <- predict(lda.mod, newdata = grid)$class
#  grid$class
#  
#  ggplot(
#    grid,
#    aes(a, b, col= class)
#  ) +
#    labs(
#      col= "Classes"
#    ) +
#    scale_colour_manual(
#      values= cols,
#      labels= c("Neg", "Pos")
#    ) +
#    geom_point(
#      data= grid,
#      aes(a, b, col= class),
#      alpha= 0.1
#    ) + 
#    scale_size(range= c(0.5 - 0.2, 2)) +
#    geom_point(
#      data= dataframe[1:250,],
#      aes(a, b, col= c),
#      size= 1,
#    ) +
#    theme(axis.title.x= element_blank(),
#          axis.title.y= element_blank(),
#          axis.text.x=  element_blank(),
#          axis.text.y=  element_blank())
#}

plot.data= data.frame(
  Age= test$age,
  Weight= test$weight,
  Cardio= data.lda.values
)

(p1= ggplot(
  data= plot.data,
  aes(Age, Weight)
  ) +
  geom_point(aes(color= Cardio)) +
  scale_color_manual(values= c("darkseagreen", "firebrick4")) +
  #scale_colour_brewer(palette= 'BuPu', direction= -1) +
  theme_bw()
)
