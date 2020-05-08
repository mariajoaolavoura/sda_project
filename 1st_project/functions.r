###
## Statistics and Data Analysis project 1
## Auxiliary functions for 'main.r'
##
## List of functions:
##  - lda.plot
##  - train_test_split
###

#' Function \code{lda.plot}
#'
#' Scatter plot pairs of variables using ggplot2
#' 
#' This function is optimised for the 'cardio-train.csv' data set, and for
#' the Linear Discriminant Analysis (LDA) regression model.
#' @author Nuno R. C. Gomes <nunorcgomes@@gmail.com> (2020/04/09)
#' @param testset A testing data set
#' @param classvals Array containing target (class) values
#' @param keyX String with name of X-variable
#' @param keyY String with name of Y-variable
#' @param xcol Colour for `keyX`; default: "lemonchiffon2"
#' @param ycol Colour for `keyY`; default: "firebrick3"
#' @examples
#'  lda.plot(test, lda.values, "age", "weight")
lda.plot= function(testset, classvals, keyX, keyY,
                   xcol= "lemonchiffon2", ycol= "firebrick3") {
  # variables
  vars= c(capFirst(keyX), capFirst(keyY))
  
  for (i in 1:2) {
    if (vars[i] == 'Age') {
      vars[i]= paste(vars[i], "(years)")
    } else if (vars[i] == 'Gender') {
      vars[i]= paste("Gender")
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
    } else if (vars[i] == 'Gluc') {
      vars[i]= "Glucose Level"
    } else if (vars[i] == 'Smoke') {
      vars[i]= paste("Smoking")
    } else if (vars[i] == "Alco") {
      vars[i]= paste("Alcohol")
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
  X.train=   dataset[train.idx, -target.idx]
  y.train=   dataset[train.idx,  target.idx]
  X.test=    dataset[test.idx,  -target.idx]
  y.test=    dataset[test.idx,   target.idx]
  
  # list of data frames
  tts= vector('list', 4)
  names(tts)= c('X_train', 'y_train', 'X_test', 'y_test')
  tts[[1]]= X.train
  tts[[2]]= y.train
  tts[[3]]= X.test
  tts[[4]]= y.test
  
  return(tts)
}

