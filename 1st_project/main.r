###
## Statistics and Data Analysis project 1
## Prediction of cardiovascular disease from 11 predictors
## (five quantitative and six qualitative) recorded in 70000 medical exams
##
## By Maria João Lavoura (up201908426) and Nuno Gomes (up199300242)
## Masters of Data Science, 2019/2020
###


## libraries
require(FSA) # for headtail
require(GGally)
require(MASS)
require(e1071) # skewness
require(ggplot2)
require(onehot)
require(plotrix) # for pie3D
require(tidyverse)

## load auxiliary functions
source('functions.r')
  

## seed
SEED= 123


## read data
original.data= read.csv("./data/cardio-train.csv")
headtail(original.data)


## inspect data
# drop 'id' column
data.raw= original.data[-1]
# convert 'age' to years
data.raw['age']= data.raw['age']/365.2422
# convert (1, 2) to ('woman', 'man') in 'gender'
data.raw['gender']= ifelse(data.raw['gender'] == 1, 'woman', 'man')
# convert 'height' from centimetres to metres
data.raw['height']= data.raw['height']/100
# rename 'ap_hi', 'ap_lo', and 'cholesterol' respectively to
# 'aphi', 'aplo', and 'choles'
data.raw= rename(data.raw,
                 c('aphi'= "ap_hi", 'aplo'= "ap_lo", 'choles'= "cholesterol")
                 )
# factorise categorical/binary variables
colNames.qual= c('gender', 'choles', 'gluc', 'smoke',
                 'alco', 'active', 'cardio')
data.raw[, colNames.qual]= lapply(data.raw[, colNames.qual], factor)
# create tibble
data.tib= as_tibble(data.raw)
head(data.tib)


## correlations
ggcorr(
  data.raw,
  name= "Correlation",
  label= T
) +
  labs(title= "Correlation matrix") +
  theme(plot.title= element_text(face= "bold", hjust= 0.5))


## extract variables
age=    data.raw$age
gender= data.raw$gender
height= data.raw$height
weight= data.raw$weight
aphi=   data.raw$aphi
aplo=   data.raw$aplo
choles= data.raw$choles
gluc=   data.raw$glucose
smoke=  data.raw$smoke
alco=   data.raw$alco
active= data.raw$active
cardio= data.raw$cardio


## variances
data.var= tibble(var(age), var(height), var(weight), var(aphi), var(aplo))
colnames(data.var)= c("age", "height", "weight", "aphi", "aplo")
data.var


## histograms and boxplots
### age
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
summary(age)
var(age) # 45.63153
skewness(age) # -0.3070422
hist(age,
     breaks= seq(round(min(age))-5, round(max(age))+5, by= 5))
boxplot(age, yaxt= 'n',
        main= "Boxplot of age")
axis(2, las= 2)
# Most of the individuals are between 50 and 60 years old;
# the youngest is about 30 yeas old,
# and the oldest approximately 65 years old.
# The distribution is slightly left skewed. 

### gender
n.gender= length(gender)
gender.woman= gender[gender == "woman"]
n.woman= length(gender.woman)
gender.woman.pct= round(n.woman / n.gender * 100)
gender.man= gender[gender == "man"]
n.man= length(gender.man)
gender.man.pct= round(n.man / n.gender * 100)
gender.labels= c(
   paste('Women:', gender.woman.pct),
   paste('Men:',   gender.man.pct))
gender.labels= paste0(gender.labels, '%')
gender.colours= c("#FA9FB5", "#74A9CF")
par(mfrow= c(1, 1), oma= c(0, 2, 3, 1))
pie3D(c(n.woman, n.man), theta= pi/3,
      labels= gender.labels, labelcex= 1.5,
      col= gender.colours,
      start= pi/2, explode= 0.1)
mtext("Gender spread", side= 3, line= -4, outer= T, cex= 2)
# More women (65%) than men (35%) were considered in this study.
#### women
women= data.raw[which(data.raw$gender == "woman"), ]$cardio
women.0= length(women[which(women == 0)])
women.1= length(women[which(women == 1)])
# sanity check
n.woman == women.0 + women.1 # same as length(women)
women.0.pct= round(women.0/n.woman * 100)
women.1.pct= round(women.1/n.woman * 100)
### men
men= data.raw[which(data.raw$gender == "man"), ]$cardio
men.0= length(men[which(men == 0)])
men.1= length(men[which(men == 1)])
# sanity check
n.man == men.0 + men.1 # same as length(men)
men.0.pct= round(men.0/n.man * 100)
men.1.pct= round(men.1/n.man * 100)
### pie charts gender
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
women.labels= c(
   paste('No CVD:', women.0.pct),
   paste('CVD:',    women.1.pct)
)
women.labels= paste0(women.labels, '%')
women.colours= c('#EDBB99', '#641E16')
pie3D(c(women.0, women.1), theta= pi/3,
      labels= women.labels, labelcex= 0.8,
      col= women.colours,
      start= pi/6, explode= 0.1)
mtext("Women", side= 1, cex= 1)
men.labels= c(
   paste('No CVD:', men.0.pct),
   paste('CVD:',    men.1.pct)
)
men.labels= paste0(men.labels, '%')
men.colours= c('#85C1E9', '#1B4F72')
pie3D(c(men.0, men.1), theta= pi/3,
      labels= men.labels, labelcex= 0.8,
      col= men.colours,
      start= pi/6, explode= 0.1)
mtext("Men", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Gender",
      side= 3, line= -4, outer= T, cex= 2)
# Gender does not have an impact in cardiovascular disease.

### height
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
summary(height)
var(height) # 0.006740617
skewness(height) # -0.6421499
hist(height,
     xlab= "Height (cm)",
     main= "Histogram of height")
boxplot(height, yaxt= 'n',
        main= "Boxplot of height")
axis(2, las= 2)
# Most of individuals with heights approximately between 1.60m and 1.70m.
# Smallest person: 55cm; tallest person: 2.50m
# The distribution is left skewed.
#### remove outliers
height.out= boxplot.stats(height)$out
height.no.out.idx= !(height %in% height.out)
height.no.out= height[height.no.out.idx]
summary(height.no.out)
cardio.height.no.out= cardio[height.no.out.idx]
hist(height.no.out,
     xlab= "Height (cm)",
     main= "Histogram of height")
boxplot(height.no.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1.2,
      expression(paste("Boxplot of height")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")

### weight
summary(weight)
var(weight) # 207.2378
skewness(weight) # 1.012027
hist(weight,
     xlab= "Weight (kg)",
     main= "Histogram of weight")
boxplot(weight, yaxt= 'n',
        main= "Boxplot of weight")
axis(2, las= 2)
# Most of the individuals with weights approximately between 60kg and 80kg.
# Lightest person: 10kg (possibly an outlier, given the age of all people);
# Heaviest person: 200kg.
# Distribution slightly right skewed.
#### remove outliers
weight.out= boxplot.stats(weight)$out
weight.no.out.idx= !(weight %in% weight.out)
weight.no.out= weight[weight.no.out.idx]
summary(weight.no.out)
cardio.weight.no.out= cardio[weight.no.out.idx]
hist(weight.no.out,
     xlab= "Weight (kg)",
     main= "Histogram of weight")
boxplot(weight.no.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1.2,
      expression(paste("Boxplot of weight")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")


### systolic and diastolic blood pressures
summary(aphi)
summary(aplo)
### remove outliers
aphi.out= boxplot.stats(aphi)$out
aphi.no.out.idx= !(aphi %in% aphi.out)
aphi.no.out= aphi[aphi.no.out.idx]
summary(aphi.no.out)
cardio.aphi.no.out= cardio[aphi.no.out.idx]
aplo.out= boxplot.stats(aplo)$out
aplo.no.out.idx= !(aplo %in% aplo.out)
aplo.no.out= aplo[aplo.no.out.idx]
summary(aplo.no.out)
cardio.aplo.no.out= cardio[aplo.no.out.idx]
## boxplots
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
### systolic blood pressure
boxplot(aphi, yaxt= 'n')
mtext(side= 3, line= 2, at= 1.0, cex= 1,
      expression(paste("Systolic blood pressure")))
mtext(side= 3, line= 1, at= 1.0, cex= 0.7,
      paste("No. of outliers: ", length(aphi.out)))
axis(2, las= 2)
## diastolic blood pressure
boxplot(aplo, yaxt= 'n')
mtext(side= 3, line= 2, at= 1, cex= 1,
      expression(paste("Diastolic blood pressure")))
mtext(side= 3, line= 1, at= 1, cex= 0.7,
      paste("No. of outliers: ", length(aplo.out)))
axis(2, las= 2)
## boxplots (outliers removed)
### systolic blood pressure
boxplot(aphi.no.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1.0, cex= 1,
      expression(paste("Systolic blood pressure")))
mtext(side= 3, line= 1, at= 1.0, cex= 0.7, "Outliers removed")
### diastolic blood pressure
boxplot(aplo.no.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1,
      expression(paste("Diastolic blood pressure")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")
### histograms
hist(aphi.no.out, breaks= seq(min(aphi.no.out), max(aphi.no.out), by= 10),
     xlab= 'Systolic blood pressure (mmHg)',
     main= "")
hist(aplo.no.out, breaks= seq(min(aplo.no.out), max(aplo.no.out), by= 10),
     xlab= 'Diastolic blood pressure (mmHg)',
     main= "")
mtext("Histograms of blood pressure",
      side= 3, line= -2, outer= T, cex= 2)


### cholesterol
n.choles= length(data.raw[, 'choles']) # same as nrow(data.set)
#### normal levels
choles.normal= data.raw[which(data.raw$choles == 1), ]$cardio
n.choles.normal= length(choles.normal)
choles.normal.pct= round(n.choles.normal / n.choles * 100, 1)
choles.normal.0= length(choles.normal[which(choles.normal == 0)])
choles.normal.0.pct= round(choles.normal.0 / n.choles.normal * 100, 1)
choles.normal.1= length(choles.normal[which(choles.normal == 1)])
choles.normal.1.pct= round(choles.normal.1 / n.choles.normal * 100, 1)
#### high levels
choles.high= data.raw[which(data.raw$choles == 2), ]$cardio
n.choles.high= length(choles.high)
choles.high.pct= round(n.choles.high / n.choles * 100, 1)
choles.high.0= length(choles.high[which(choles.high == 0)])
choles.high.0.pct= round(choles.high.0 / n.choles.high * 100, 1)
choles.high.1= length(choles.high[which(choles.high == 1)])
choles.high.1.pct= round(choles.high.1 / n.choles.high * 100, 1)
#### very high levels
choles.veryhigh= data.raw[which(data.raw$choles == 3), ]$cardio
n.choles.veryhigh= length(choles.veryhigh)
choles.veryhigh.pct= round(n.choles.veryhigh / n.choles * 100, 1)
choles.veryhigh.0= length(choles.veryhigh[which(choles.veryhigh == 0)])
choles.veryhigh.0.pct= round(choles.veryhigh.0 / n.choles.veryhigh * 100, 1)
choles.veryhigh.1= length(choles.veryhigh[which(choles.veryhigh == 1)])
choles.veryhigh.1.pct= round(choles.veryhigh.1 / n.choles.veryhigh * 100, 1)
#### pie charts cholesterol
labels= c('No CVD:', 'CVD:')
labels.normal= paste(labels, c(choles.normal.0.pct, choles.normal.1.pct))
labels.normal= paste0(labels.normal, '%')
colours.normal= c('#85C1E9', '#F39C12')
labels.high= paste(labels, c(choles.high.0.pct, choles.high.1.pct))
labels.high= paste0(labels.high, '%')
colours.high= c('#85C1E9', '#D35400')
labels.veryhigh= paste(labels, c(choles.veryhigh.0.pct, choles.veryhigh.1.pct))
labels.veryhigh= paste0(labels.veryhigh, '%')
colours.veryhigh= c('#85C1E9', '#922B21')
par(mfrow= c(1, 3))
pie3D(c(choles.normal.0, choles.normal.1), theta= pi/3,
      labels= labels.normal, labelcex= 0.8,
      col= colours.normal,
      start= pi/6, explode= 0.1)
mtext("Normal", side= 1, cex= 1)
pie3D(c(choles.high.0, choles.high.1), theta= pi/3,
      labels= labels.high, labelcex= 0.8,
      col= colours.high,
      start= 0, explode= 0.1)
mtext("Above normal", side= 1, cex= 1)
pie3D(c(choles.veryhigh.0, choles.veryhigh.1), theta= pi/3,
      labels= labels.veryhigh, labelcex= 0.8,
      col= colours.veryhigh,
      start= pi/2, explode= 0.1)
mtext("Well above normal", side= 1, cex= 1)
mtext("Cardiovascular disease per cholesterol levels",
      side= 3, line= -6, outer= T, cex= 2)
# Strong correlation between cholesterol levels and cardiovascular disease.
#### total percentages
labels.tot= c("Normal\nNo CVD\n",
              "Above normal\nNo CVD\n",
              "Well above normal\nNo CVD\n",
              "Normal\nCVD\n",
              "Above normal\nCVD\n",
              "Well above normal\nCVD\n")
choles.normal.0.pct.tot= round(choles.normal.0 / n.choles * 100, 1)
choles.normal.1.pct.tot= round(choles.normal.1 / n.choles * 100, 1)
choles.high.0.pct.tot= round(choles.high.0 / n.choles * 100, 1)
choles.high.1.pct.tot= round(choles.high.1 / n.choles * 100, 1)
choles.veryhigh.0.pct.tot= round(choles.veryhigh.0 / n.choles * 100, 1)
choles.veryhigh.1.pct.tot= round(choles.veryhigh.1 / n.choles * 100, 1)
labels.tot= paste(labels.tot,
                  c(choles.normal.0.pct.tot,
                    choles.high.0.pct.tot,
                    choles.veryhigh.0.pct.tot,
                    choles.normal.1.pct.tot,
                    choles.high.1.pct.tot,
                    choles.veryhigh.1.pct.tot)
                 )
labels.tot= paste0(labels.tot, '%')
colours.tot= c("#1B4F72", "#2874A6", "#3498DB",
               colours.normal[2], colours.high[2], colours.veryhigh[2])
par(mfrow= c(1, 1), oma= c(0, 2, 3, 1))
pie3D(
  c(choles.normal.0.pct.tot,
    choles.high.0.pct.tot,
    choles.veryhigh.0.pct.tot,
    choles.normal.1.pct.tot,
    choles.high.1.pct.tot,
    choles.veryhigh.1.pct.tot),
  theta= pi/3,
  labels= labels.tot,
  labelcex= 0.8,
  col= colours.tot,
  start= pi/2,
  explode= 0.03,
  shade= 0.7,
  labelpos= c(3, 4.1, 4.6, 5.6, 7.0, 7.6)
)
mtext("Cholesterol levels and\nCardio Vascular Disease incidence",
      side= 3, line= -2.5, outer= T, cex= 2)


### glucose
n.gluc= length(data.raw[, 'gluc']) # same as nrow(data.set)
#### normal levels
gluc.normal= data.raw[which(data.raw$gluc == 1), ]$cardio
n.gluc.normal= length(gluc.normal)
gluc.normal.pct= round(n.gluc.normal / n.gluc * 100, 1)
gluc.normal.0= length(gluc.normal[which(gluc.normal == 0)])
gluc.normal.0.pct= round(gluc.normal.0 / n.gluc.normal * 100, 1)
gluc.normal.1= length(gluc.normal[which(gluc.normal == 1)])
gluc.normal.1.pct= round(gluc.normal.1 / n.gluc.normal * 100, 1)
#### high levels
gluc.high= data.raw[which(data.raw$gluc == 2), ]$cardio
n.gluc.high= length(gluc.high)
gluc.high.pct= round(n.gluc.high / n.gluc * 100, 1)
gluc.high.0= length(gluc.high[which(gluc.high == 0)])
gluc.high.0.pct= round(gluc.high.0 / n.gluc.high * 100, 1)
gluc.high.1= length(gluc.high[which(gluc.high == 1)])
gluc.high.1.pct= round(gluc.high.1 / n.gluc.high * 100, 1)
#### very high levels
gluc.veryhigh= data.raw[which(data.raw$gluc == 3), ]$cardio
n.gluc.veryhigh= length(gluc.veryhigh)
gluc.veryhigh.pct= round(n.gluc.veryhigh / n.gluc * 100, 1)
gluc.veryhigh.0= length(gluc.veryhigh[which(gluc.veryhigh == 0)])
gluc.veryhigh.0.pct= round(gluc.veryhigh.0 / n.gluc.veryhigh * 100, 1)
gluc.veryhigh.1= length(gluc.veryhigh[which(gluc.veryhigh == 1)])
gluc.veryhigh.1.pct= round(gluc.veryhigh.1 / n.gluc.veryhigh * 100, 1)
#### pie charts glucose
labels= c('No CVD:', 'CVD:')
labels.normal= paste(labels, c(gluc.normal.0.pct, gluc.normal.1.pct))
labels.normal= paste0(labels.normal, '%')
colours.normal= c('#85C1E9', '#F39C12')
labels.high= paste(labels, c(gluc.high.0.pct, gluc.high.1.pct))
labels.high= paste0(labels.high, '%')
colours.high= c('#85C1E9', '#D35400')
labels.veryhigh= paste(labels, c(gluc.veryhigh.0.pct, gluc.veryhigh.1.pct))
labels.veryhigh= paste0(labels.veryhigh, '%')
colours.veryhigh= c('#85C1E9', '#922B21')
par(mfrow= c(1, 3))
pie3D(c(gluc.normal.0, gluc.normal.1), theta= pi/3,
      labels= labels.normal, labelcex= 0.8,
      col= colours.normal,
      start= pi/3, explode= 0.1)
mtext("Normal", side= 1, cex= 1)
pie3D(c(gluc.high.0, gluc.high.1), theta= pi/3,
      labels= labels.high, labelcex= 0.8,
      col= colours.high,
      start= pi/2, explode= 0.1,
      labelpos= c(2.2, 5.5))
mtext("Above normal", side= 1, cex= 1)
pie3D(c(gluc.veryhigh.0, gluc.veryhigh.1), theta= pi/3,
      labels= labels.veryhigh, labelcex= 0.8,
      col= colours.veryhigh,
      start= 7*pi/9, explode= 0.1)
mtext("Well above normal", side= 1, cex= 1)
mtext("Cardiovascular disease per glucose levels",
      side= 3, line= -10, outer= T, cex= 2)
# Strong correlation between glucose levels and cardiovascular disease.
#### total percentages
labels.tot= c("Normal\nNo CVD\n",
              "Above normal\nNo CVD\n",
              "Well above normal\nNo CVD\n",
              "Normal\nCVD\n",
              "Above normal\nCVD\n",
              "Well above normal\nCVD\n")
gluc.normal.0.pct.tot= round(gluc.normal.0 / n.gluc * 100, 1)
gluc.normal.1.pct.tot= round(gluc.normal.1 / n.gluc * 100, 1)
gluc.high.0.pct.tot= round(gluc.high.0 / n.gluc * 100, 1)
gluc.high.1.pct.tot= round(gluc.high.1 / n.gluc * 100, 1)
gluc.veryhigh.0.pct.tot= round(gluc.veryhigh.0 / n.gluc * 100, 1)
gluc.veryhigh.1.pct.tot= round(gluc.veryhigh.1 / n.gluc * 100, 1)
labels.tot= paste(labels.tot,
                  c(gluc.normal.0.pct.tot,
                    gluc.high.0.pct.tot,
                    gluc.veryhigh.0.pct.tot,
                    gluc.normal.1.pct.tot,
                    gluc.high.1.pct.tot,
                    gluc.veryhigh.1.pct.tot)
                 )
labels.tot= paste0(labels.tot, '%')
colours.tot= c("#1B4F72", "#2874A6", "#3498DB",
               colours.normal[2], colours.high[2], colours.veryhigh[2])
par(mfrow= c(1, 1), oma= c(0, 2, 3, 1))
pie3D(
  c(gluc.normal.0.pct.tot,
    gluc.high.0.pct.tot,
    gluc.veryhigh.0.pct.tot,
    gluc.normal.1.pct.tot,
    gluc.high.1.pct.tot,
    gluc.veryhigh.1.pct.tot),
  theta= pi/3,
  labels= labels.tot,
  labelcex= 0.8,
  col= colours.tot,
  start= pi/2,
  explode= 0.03,
  shade= 0.7,
  labelpos= c(3.1, 4.3, 4.8, 5.6, 7.4, 7.8)
)
mtext("Glucose levels and\nCardio Vascular Disease incidence",
      side= 3, line= -2.5, outer= T, cex= 2)

### smoking, alcohol, physical activity
#### smoking
n.smoke= length(data.raw[, 'smoke']) # same as nrow(data.set)
##### non-smokers
smoke.no= data.raw[which(data.raw$smoke == 0), ]$cardio
n.smoke.no= length(smoke.no)
smoke.no.pct= round(n.smoke.no / n.smoke.no * 100, 1)
smoke.no.0= length(smoke.no[which(smoke.no == 0)])
smoke.no.0.pct= round(smoke.no.0 / n.smoke.no * 100, 1)
smoke.no.1= length(smoke.no[which(smoke.no == 1)])
smoke.no.1.pct= round(smoke.no.1 / n.smoke.no * 100, 1)
##### smokers
smoke.yes= data.raw[which(data.raw$smoke == 1), ]$cardio
n.smoke.yes= length(smoke.yes)
smoke.yes.pct= round(n.smoke.yes / n.smoke.yes * 100, 1)
smoke.yes.0= length(smoke.yes[which(smoke.yes == 0)])
smoke.yes.0.pct= round(smoke.yes.0 / n.smoke.yes * 100, 1)
smoke.yes.1= length(smoke.yes[which(smoke.yes == 1)])
smoke.yes.1.pct= round(smoke.yes.1 / n.smoke.yes * 100, 1)
##### pie-charts smoking
smoke.no.labels= c(
   paste('No CVD:', smoke.no.0.pct),
   paste('CVD:',    smoke.no.1.pct)
)
smoke.no.labels= paste0(smoke.no.labels, '%')
smoke.no.colours= c('#85C1E9', '#F8C471')
smoke.yes.labels= c(
   paste('No CVD:', smoke.yes.0.pct),
   paste('CVD:',    smoke.yes.1.pct)
)
smoke.yes.labels= paste0(smoke.yes.labels, '%')
smoke.yes.colours= c('#B2BABB', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(smoke.no.0, smoke.no.1), theta= pi/3,
      labels= smoke.no.labels, labelcex= 0.8,
      col= smoke.no.colours,
      start= pi/6, explode= 0.1)
mtext("Non-smokers", side= 1, cex= 1)
pie3D(c(smoke.yes.0, smoke.yes.1), theta= pi/3,
      labels= smoke.yes.labels, labelcex= 0.8,
      col= smoke.yes.colours,
      start= pi/6, explode= 0.1)
mtext("Smokers", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Smoking",
      side= 3, line= -4, outer= T, cex= 2)
# No apparent meaningful correlation between smoking and CVD.

#### alcohol
n.alco= length(data.raw[, 'alco']) # same as nrow(data.set)
##### non-drinkers
alco.no= data.raw[which(data.raw$alco == 0), ]$cardio
n.alco.no= length(alco.no)
alco.no.pct= round(n.alco.no / n.alco.no * 100, 1)
alco.no.0= length(alco.no[which(alco.no == 0)])
alco.no.0.pct= round(alco.no.0 / n.alco.no * 100, 1)
alco.no.1= length(alco.no[which(alco.no == 1)])
alco.no.1.pct= round(alco.no.1 / n.alco.no * 100, 1)
##### drinkers
alco.yes= data.raw[which(data.raw$alco == 1), ]$cardio
n.alco.yes= length(alco.yes)
alco.yes.pct= round(n.alco.yes / n.alco.yes * 100, 1)
alco.yes.0= length(alco.yes[which(alco.yes == 0)])
alco.yes.0.pct= round(alco.yes.0 / n.alco.yes * 100, 1)
alco.yes.1= length(alco.yes[which(alco.yes == 1)])
alco.yes.1.pct= round(alco.yes.1 / n.alco.yes * 100, 1)
### pie-charts alcohol intake
alco.no.labels= c(
   paste('No CVD:', alco.no.0.pct),
   paste('CVD:',    alco.no.1.pct)
)
alco.no.labels= paste0(alco.no.labels, '%')
alco.no.colours= c('#85C1E9', '#F8C471')
alco.yes.labels= c(
   paste('No CVD:', alco.yes.0.pct),
   paste('CVD:',    alco.yes.1.pct)
)
alco.yes.labels= paste0(alco.yes.labels, '%')
alco.yes.colours= c('#D7BDE2', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(alco.no.0, alco.no.1), theta= pi/3,
      labels= alco.no.labels, labelcex= 0.8,
      col= alco.no.colours,
      start= pi/6, explode= 0.1)
mtext("Non-drinkers", side= 1, cex= 1)
pie3D(c(alco.yes.0, alco.yes.1), theta= pi/3,
      labels= alco.yes.labels, labelcex= 0.8,
      col= alco.yes.colours,
      start= pi/6, explode= 0.1)
mtext("Drinkers", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Alcohol",
      side= 3, line= -4, outer= T, cex= 2)
# No apparent correlation between alcohol intake and CVD.

#### physical activity
n.active= length(data.raw[, 'active']) # same as nrow(data.set)
##### sedentaries
active.no= data.raw[which(data.raw$active == 0), ]$cardio
n.active.no= length(active.no)
active.no.pct= round(n.active.no / n.active.no * 100, 1)
active.no.0= length(active.no[which(active.no == 0)])
active.no.0.pct= round(active.no.0 / n.active.no * 100, 1)
active.no.1= length(active.no[which(active.no == 1)])
active.no.1.pct= round(active.no.1 / n.active.no * 100, 1)
### physically active
active.yes= data.raw[which(data.raw$active == 1), ]$cardio
n.active.yes= length(active.yes)
active.yes.pct= round(n.active.yes / n.active.yes * 100, 1)
active.yes.0= length(active.yes[which(active.yes == 0)])
active.yes.0.pct= round(active.yes.0 / n.active.yes * 100, 1)
active.yes.1= length(active.yes[which(active.yes == 1)])
active.yes.1.pct= round(active.yes.1 / n.active.yes * 100, 1)
### pie charts physical activity
active.no.labels= c(
   paste('No CVD:', active.no.0.pct),
   paste('CVD:',    active.no.1.pct)
)
active.no.labels= paste0(active.no.labels, '%')
active.no.colours= c('#FFEDA0', '#F8C471')
active.yes.labels= c(
   paste('No CVD:', active.yes.0.pct),
   paste('CVD:',    active.yes.1.pct)
)
active.yes.labels= paste0(active.yes.labels, '%')
active.yes.colours= c('#A9DFBF', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(active.no.0, active.no.1), theta= pi/3,
      labels= active.no.labels, labelcex= 0.8,
      col= active.no.colours,
      start= pi/6, explode= 0.1)
mtext("Sedentaries", side= 1, cex= 1)
pie3D(c(active.yes.0, active.yes.1), theta= pi/3,
      labels= active.yes.labels, labelcex= 0.8,
      col= active.yes.colours,
      start= pi/6, explode= 0.1)
mtext("Physical active", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Physical activity",
      side= 3, line= -4, outer= T, cex= 2)
# Very small apparent correlation between physical activity and CVD.


## remove all outliers
base.seq= 1:nrow(data.raw)
height.idx= base.seq[height.no.out.idx]
weight.idx= base.seq[weight.no.out.idx]
aphi.idx= base.seq[aphi.no.out.idx]
aplo.idx= base.seq[aplo.no.out.idx]
aplo.idx= base.seq[aplo.no.out.idx]
idx= intersect(height.idx, weight.idx)
idx= intersect(idx, aphi.idx)
idx= intersect(idx, aplo.idx)
data.set= data.raw[idx, ]
data.tib= as_tibble(data.set[idx, ])


## save clean data set
write.csv(data.set, "./data/cardio-clean.csv", row.names= F)


## read clean data set
data.clean= read.csv("./data/cardio-clean.csv")


## train/test split
tts= train_test_split(data.clean, "cardio", 0.7, seed= SEED)
x.train= tts$X_train
y.train= as.numeric(as.character(tts$y_train))
x.test=  tts$X_test
y.test=  as.numeric(as.character(tts$y_test))


## use x.train for model fitting
train.set= x.train
colNames.qual= c('choles', 'gluc', 'smoke', 'alco', 'active')
train.set[, colNames.qual]= lapply(train.set[, colNames.qual], factor)
train.tib= as_tibble(train.set)
train.tib


## prepare x.test for one hot encoding
test.set= x.test
test.set[, colNames.qual]= lapply(test.set[, colNames.qual], factor)
test.tib= as_tibble(test.set)
test.tib


## correlations
data.clean['gender']= ifelse(data.clean['gender'] == 'woman', 0, 1)
colNames.qual= c('gender', 'choles', 'gluc', 'smoke', 'alco', 'active')
data.clean[, colNames.qual]= lapply(data.clean[, colNames.qual], factor)
### all variables
ggcorr(
  data.clean,
  name= "Correlation",
  label= T) +
  labs(title= "Correlation matrix (all variables)") +
  theme(plot.title= element_text(face= "bold", hjust= 0.5))
### quantitative variables
ggcorr(
  train.set,
  name= "Correlation",
  label= T
  ) +
  labs(title= "Correlation matrix") +
  theme(plot.title= element_text(face= "bold", hjust= 0.5))

cor(train.set[, c(1, 3, 4, 5, 6)])
## quantitative variables
ggpairs(data.clean[, c(1, 3, 4, 5, 6, 12)])
## qualitative variables
ggpairs(data.clean[, c(2, 7, 8, 9, 10, 11, 12)])
train.seq= 1:nrow(train.set)
set.seed(SEED)
idx.pairs= sample(train.seq, 10000)
ggpairs(train.set[idx.pairs, c(1, 3, 4, 5, 6)])


## one hot encoding on train set
gender= train.set$gender
train.set['gender']= ifelse(train.set['gender'] == 'woman', 0, 1)
encoder.train= onehot(train.set)
train.dmy= as.data.frame(predict(encoder.train, train.set))
train.dmy$cardio= y.train
head(train.dmy)
train.tib.enc= as_tibble(train.dmy)
train.tib.enc


## one hot encoding on test set
gender= test.set$gender
test.set['gender']= ifelse(test.set['gender'] == 'woman', 0, 1)
encoder.test= onehot(test.set)
test.dmy= as.data.frame(predict(encoder.test, test.set))
test.dmy$cardio= y.test
head(test.dmy)
test.tib.enc= as_tibble(test.dmy)
test.tib.enc


## linear regression
cardio.num= as.numeric(as.character(y.train))
### null model
model.null= lm(cardio.num ~ 1, data= train.dmy)
model.null
anova(model.null)
# intercept: 0.4923 ==> mean = 0.4923 ==> 49.23% of pacients have CVD
# TSS = 10936
# MSE = 0.24995

### full model
model.linear.full= lm(cardio.num ~ ., data= train.dmy[, -ncol(train.dmy)])
model.linear.full
anova(model.linear.full)
# TSS = 8406
# MSE = 0.19
# non-significant variables: gender, height

### remove gender
model.linear.1= lm(cardio.num ~ age+height+weight+aphi+aplo+
                     `choles=1`+`choles=2`+`gluc=1`+`gluc=2`+
                     `smoke=0`+`alco=0`+`active=0`,
                   data= train.dmy)
model.linear.1
anova(model.linear.1)
# Height is not significant.

### remove gender+height
model.linear.2= lm(cardio.num ~ age+weight+aphi+aplo+
                     `choles=1`+`choles=2`+`gluc=1`+`gluc=2`+
                     `smoke=0`+`alco=0`+`active=0`,
                   data= train.dmy)
model.linear.2
anova(model.linear.2)
# All variables are significant.

### remove height
model.linear.3= lm(cardio.num ~ age+gender+weight+aphi+aplo+
                     `choles=1`+`choles=2`+`gluc=1`+`gluc=2`+
                     `smoke=0`+`alco=0`+`active=0`,
                   data= train.dmy)
model.linear.3
anova(model.linear.3)
# Gender is not significant.
# Since 'aphi' and 'aplo' are highly correlated, we will try to remove
# one of them at a time.

## remove aphi
model.linear.4= lm(cardio.num ~ age+weight+aplo+
                     `choles=1`+`choles=2`+`gluc=1`+`gluc=2`+
                     `smoke=0`+`alco=0`+`active=0`,
                   data= train.dmy)
model.linear.4
anova(model.linear.4)
# The TTS and MSE are worse than in model.linear.2.


## remove aplo
model.linear.5= lm(cardio.num ~ age+weight+aphi+
                     `choles=1`+`choles=2`+`gluc=1`+`gluc=2`+
                     `smoke=0`+`alco=0`+`active=0`,
                   data= train.dmy)
model.linear.5
anova(model.linear.5)
# TTS is slightly worse than in model.linear.2.
# So, model.linear.2 is the one with all significant variables.
model.linear= model.linear.2


## cook's distance
par(mfrow= c(1, 1), oma= c(0, 2, 3, 1))
plot(hatvalues(model.linear),
     xlab= '', ylab= '', yaxt= 'n',
     main= "Cook's distance")
axis(2, las= 2)
# All Cook's distances are way below 0.1. Therefore, they are of no concern,
# and we will not look at the leverages.


## residuals
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow= T))
hist(rstandard(model.linear),
     xlab= "Residuals",
     main= "Histogram of the residuals")
boxplot(rstandard(model.linear), yaxt= 'n',
        main= "Boxplot of the residuals"); axis(2, las= 2)
qqnorm(rstandard(model.linear), yaxt= 'n'); axis(2, las= 2)
qqline(rstandard(model.linear), col= 'red', lwd= 1.8)
# The residuals are not normally distributed.


# train model
model.linear= lm(cardio.num ~ age+weight+aphi+aplo+
                     `choles=1`+`choles=2`+`gluc=1`+`gluc=2`+
                     `smoke=0`+`alco=0`+`active=0`,
                   data= train.dmy)

## train accuracy
pred.linear.train= predict(model.linear, train.dmy)
pred.linear.train= ifelse(pred.linear.train > 0.5, 1, 0)
cm.linear.train= as.matrix(table(actual= y.train,
                                 predicted= pred.linear.train)
                          )
accu.linear.train= sum(diag(cm.linear.train)) / length(y.train)
accu.linear.train
# Accuracy on the train set: 0.723


## test accuracy
pred.linear.test= predict(model.linear, test.dmy)
pred.linear.test= ifelse(pred.linear.test > 0.5, 1, 0)
cm.linear.test= as.matrix(table(actual= y.test,
                                predicted= pred.linear.test)
                         )
accu.linear.test= sum(diag(cm.linear.test)) / length(y.test)
accu.linear.test
# Accuracy on the test set: 0.726