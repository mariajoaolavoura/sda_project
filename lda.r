###
## Linear Discriminat Analysis (LDA)
###

## libraries
require(FSA) # for headtail
require(ggplot2)
require(MASS)

## seed
set.seed(123)

## read data
data.set= read.csv("./data/cardio_data.csv")
headtail(data.set)

## list of categorical/binary variables
## gender, choles, glucose, smoke, alcohol, active, cardio

## convert categorical/binary variables to factor
colNames.cat= c('gender', 'choles', 'glucose', 'smoke',
                'alcohol', 'active', 'cardio')
data.set[, colNames.cat]= lapply(data.set[, colNames.cat], factor)

## extract variables
age=     data.set$age
gender=  data.set$gender
height=  data.set$height/100 # in metres
weight=  data.set$weight
aphi=    data.set$aphi
aplo=    data.set$aplo
choles=  data.set$choles
glucose= data.set$glucose
smoke=   data.set$smoke
alcohol= data.set$alcohol
active=  data.set$active
cardio=  data.set$cardio

## split data
nr= nrow(data.set)
frac= 0.7
train.idx= sample(1:nr, frac*nr)
train= data.set[train.idx, ]
test= data.set[-train.idx, ]

## remove height from data sets
#train$height= NULL
#test$height= NULL

## lda model
lda.model= lda(cardio ~ ., data= train)

## predictions
lda.data= predict(lda.model, newdata= test)
lda.values= lda.data$class

## create plots
## capitalise strings function
lda.plot= function(testset, classvals, keyX, keyY,
                   xcol= "lemonchiffon2", ycol= "firebrick3") {
  # variables
  vars= c(capFirst(keyX), capFirst(keyY))
  
  for (i in 1:2) {
    if (vars[i] == 'Age') {
      vars[i]= paste(vars[i], "(years)")
    } else if (vars[i] == 'Height') {
      vars[i]= paste(vars[i], "(m)")
    } else if (vars[i] == 'Weight') {
      vars[i]= paste(vars[i], "(kg)")
    } else if (vars[i] == 'Aphi') {
      vars[i]= paste("Systolic Blood Pressure (mm.Hg)")
    } else if (vars[i] == 'Aplo') {
      vars[i]= paste('Diastolic Blood Pressure (mm.Hg)')
    } else if (vars[i] == 'Choles') {
      vars[i]= paste('Cholesterol Level')
    } else if (vars[i] == 'Glucose') {
      vars[i]= "Glucose Level"
    } else if (vars[i] == 'Smoke') {
      vars[i]= paste("Smoking")
    } else if (vars[i] == 'Active') {
      vars[i]= "Physical Activity"
    } else if (vars[i] == 'Cardio') {
      vars[i]= "Cardiovascular Disease"
    }
  }
  
  ## create data frame with data
  plot.data= data.frame(
    testset[keyX],
    testset[keyY],
    classvals
    )
  names(plot.data)= c('x', 'y', 'Cardio')
  
  theme_set(theme_bw())
  cols= c(xcol, ycol)
  
  ggplot(
    data= plot.data,
    aes(x, y)
  ) +
  geom_point(
    aes(colour= Cardio),
    size= 1
  ) +
  labs(
    x= vars[1],
    y= vars[2],
    col= "Disease"
  ) +
  scale_colour_manual(
    values= cols,
    labels= c("No", "Yes")
  )
}

lda.plot(test, lda.values, "age", "weight")
