## Masters of Data Science, 2019/2020
## Project of Statistics and Data Analysis
## Nuno Gomes + Maria João Lavoura
## Prediction of cardiovascular disease from 11 predictors
## (five numerical and six categorical) recorded in 70000 medical exams

## libraries
require(e1071)
require(fastDummies)
require(FSA)
require(plotrix)

## read data
original.data= read.csv("./data/cardio-train.csv")
headtail(original.data)


## drop 'id' column
cardio.data= original.data[-1]
headtail(cardio.data)
summary(cardio.data)
# convert age to years
cardio.data['age']= cardio.data['age']/365.2422
headtail(cardio.data)

## extract variables
age=     cardio.data$age
gender=  cardio.data$gender
height=  cardio.data$height
weight=  cardio.data$weight
aphi=    cardio.data$ap_hi
aplo=    cardio.data$ap_lo
choles=  cardio.data$cholesterol
glucose= cardio.data$gluc
smoke=   cardio.data$smoke
alcohol= cardio.data$alco
active0=  cardio.data$active
cardio=  cardio.data$cardio
# cardio is the target variable; it is a boolean variable:
# 0: NO CVD (absense of cardiovascular disease),
# 1: CVD (presence of cardiovascular disease)

## correlations
cor(cardio.data)
# The variables are not highly correlated with each other.
# Strongest correlations between height and gender, cholesterol and glucose,
# smoke and alcohol; these are all irrelevant correlations for the study.
# The target variable (cardio) does not strongly correlate with any of the
# features; the strongest correlations are with age, weight, and cholesterol.

## scatter plots
## WARNING: DO NOT RUN THIS INSTRUCTION UNLESS IT IS STRICTLY NECESSARY!
#pairs(data)

## histograms and boxplots
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))

### age
summary(age)
hist(age,
     breaks= seq(round(min(age))-5, round(max(age))+5, by= 5))
boxplot(age, yaxt= 'n',
        main= "Boxplot of age")
axis(2, las= 2)
skewness(age) # -0.3070422
# Most of the individuals are between 50 and 60 years old;
# the youngest is about 30 yeas old,
# and the oldest approximately 65 years old.
# The distribution is slightly left skewed. 

### height
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))

summary(height)
hist(height,
     xlab= "Height (cm)",
     main= "Histogram of height")
boxplot(height, yaxt= 'n',
        main= "Boxplot of height")
axis(2, las= 2)
skewness(height) # -0.6421499
# Most of individuals with heights approximately between 1.60m and 1.70m.
# Smallest person: 55cm; tallest person: 2.50m
# The distribution is left skewed.

### remove outliers (POSSIBLY NOT TO INCLUDE)
height.out= boxplot.stats(height)$out
height.out.idx= !(height %in% height.out)
height.out= height[height.out.idx]
summary(height.out)
cardio.height.out= cardio[height.out.idx]
hist(height.out,
     xlab= "Height (cm)",
     main= "Histogram of height")
boxplot(height.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1.2,
      expression(paste("Boxplot of height")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")

## weight
summary(weight)
hist(weight,
     xlab= "Weight (kg)",
     main= "Histogram of weight")
boxplot(weight, yaxt= 'n',
        main= "Boxplot of weight")
axis(2, las= 2)
skewness(weight) # 1.012027
# Most of the individuals with weights approximately between 60kg and 80kg.
# Lightest person: 10kg (possibly an outlier, given the age of all people);
# Heaviest person: 200kg.
# Distribution slightly right skewed.

# remove outliers (POSSIBLY NOT TO INCLUDE)
weight.out= boxplot.stats(weight)$out
weight.out.idx= !(weight %in% weight.out)
weight.out= weight[weight.out.idx]
summary(weight.out)
cardio.weight.out= cardio[weight.out.idx]
hist(weight.out,
     xlab= "Weight (kg)",
     main= "Histogram of weight")
boxplot(weight.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1.2,
      expression(paste("Boxplot of weight")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")

## gender
gender.women= sum(gender[gender == 1])
n= length(gender)
gender.women.pct= round(gender.women / n * 100)
gender.men= sum(gender[gender == 2] - 1)
gender.men.pct= round(gender.men / n * 100)
gender.labels= c(
   paste('Women:', gender.women.pct),
   paste('Men:',   gender.men.pct))
gender.labels= paste0(gender.labels, '%')
gender.colours= c("#FA9FB5", "#74A9CF")
par(mfrow= c(1, 1), oma= c(0, 2, 3, 1))
pie3D(c(gender.women, gender.men), theta= pi/3,
      labels= gender.labels, labelcex= 1.5,
      col= gender.colours,
      start= pi/2, explode= 0.1)
mtext("Gender spread", side= 3, line= -4, outer= T, cex= 2)
# More women (65%) than men (35%) were considered in this study.

### women
women= cardio.data[which(cardio.data$gender == 1), ]$cardio
women.0= length(women[which(women == 0)])
women.1= length(women[which(women == 1)])
women.total= women.0 + women.1 # same as length(women)
women.0.pct= round(women.0/women.total * 100)
women.1.pct= round(women.1/women.total * 100)
### men
men= cardio.data[which(cardio.data$gender == 2), ]$cardio
men.0= length(men[which(men == 0)])
men.1= length(men[which(men == 1)])
men.total= men.0 + men.1 # same as length(men)
men.0.pct= round(men.0/men.total * 100)
men.1.pct= round(men.1/men.total * 100)
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

## cholesterol
### normal levels
n.choles= cardio.data[which(cardio.data$cholesterol == 1), ]$cardio
normal.choles.1= sum(n.choles)
normal.choles.total= length(n.choles)
normal.choles.0= normal.choles.total - normal.choles.1
normal.choles= c(normal.choles.0, normal.choles.1)
normal.choles.pct= round(normal.choles/normal.choles.total*100)
### high levels
h.choles= cardio.data[which(cardio.data$cholesterol == 2), ]$cardio
high.choles.1= sum(h.choles)
high.choles.total= length(h.choles)
high.choles.0= high.choles.total - high.choles.1
high.choles= c(high.choles.0, high.choles.1)
high.choles.pct= round(high.choles/high.choles.total*100)
### very high levels
vh.choles= cardio.data[which(cardio.data$cholesterol == 3), ]$cardio
veryhigh.choles.1= sum(vh.choles)
veryhigh.choles.total= length(vh.choles)
veryhigh.choles.0= veryhigh.choles.total - veryhigh.choles.1
veryhigh.choles= c(veryhigh.choles.0, veryhigh.choles.1)
veryhigh.choles.pct= round(veryhigh.choles/veryhigh.choles.total*100)
### pie charts cholesterol
labels= c('No CVD:', 'CVD:')
normal.labels= paste(labels, normal.choles.pct)
normal.labels= paste0(normal.labels, '%')
normal.colours= c('#85C1E9', '#A9DFBF')
high.labels= paste(labels, high.choles.pct)
high.labels= paste0(high.labels, '%')
high.colours= c('#85C1E9', '#F8C471')
veryhigh.labels= paste(labels, veryhigh.choles.pct)
veryhigh.labels= paste0(veryhigh.labels, '%')
veryhigh.colours= c('#85C1E9', '#F1948A')
par(mfrow= c(1, 3))
pie3D(normal.choles, theta= pi/3,
      labels= normal.labels, labelcex= 0.8,
      col= normal.colours,
      start= pi/6, explode= 0.1)
mtext("Normal", side= 1, cex= 1)
pie3D(high.choles, theta= pi/3,
      labels= high.labels, labelcex= 0.8,
      col= high.colours,
      start= 0, explode= 0.1)
mtext("Above normal", side= 1, cex= 1)
pie3D(veryhigh.choles, theta= pi/3,
      labels= veryhigh.labels, labelcex= 0.8,
      col= veryhigh.colours,
      start= pi/2, explode= 0.1)
mtext("Well above normal", side= 1, cex= 1)
mtext("Cardiovascular disease per cholesterol levels",
      side= 3, line= -6, outer= T, cex= 2)
# Strong correlation between cholesterol levels and cardiovascular disease.

## glucose
### normal levels
n.glucose= cardio.data[which(cardio.data$gluc == 1), ]$cardio
normal.glucose.1= sum(n.glucose)
normal.glucose.total= length(n.glucose)
normal.glucose.0= normal.glucose.total - normal.glucose.1
normal.glucose= c(normal.glucose.0, normal.glucose.1)
normal.glucose.pct= round(normal.glucose/normal.glucose.total*100)
### high levels
h.glucose= cardio.data[which(cardio.data$gluc == 2), ]$cardio
high.glucose.1= sum(h.glucose)
high.glucose.total= length(h.glucose)
high.glucose.0= high.glucose.total - high.glucose.1
high.glucose= c(high.glucose.0, high.glucose.1)
high.glucose.pct= round(high.glucose/high.glucose.total*100)
### very high levels
vh.glucose= cardio.data[which(cardio.data$gluc == 3), ]$cardio
veryhigh.glucose.1= sum(vh.glucose)
veryhigh.glucose.total= length(vh.glucose)
veryhigh.glucose.0= veryhigh.glucose.total - veryhigh.glucose.1
veryhigh.glucose= c(veryhigh.glucose.0, veryhigh.glucose.1)
veryhigh.glucose.pct= round(veryhigh.glucose/veryhigh.glucose.total*100)
### pie charts glucose
normal.labels= paste(labels, normal.glucose.pct)
normal.labels= paste0(normal.labels, '%')
normal.colours= c('#85C1E9', '#A9DFBF')
high.labels= paste(labels, high.glucose.pct)
high.labels= paste0(high.labels, '%')
high.colours= c('#85C1E9', '#F8C471')
veryhigh.labels= paste(labels, veryhigh.glucose.pct)
veryhigh.labels= paste0(veryhigh.labels, '%')
veryhigh.colours= c('#85C1E9', '#F1948A')
par(mfrow= c(1, 3))
pie3D(normal.glucose, theta= pi/3,
      labels= normal.labels, labelcex= 0.8,
      col= normal.colours,
      start= pi/6, explode= 0.1)
mtext("Normal levels", side= 1, cex= 1)
pie3D(high.glucose, theta= pi/3,
      labels= high.labels, labelcex= 0.8,
      col= high.colours,
      start= 0, explode= 0.1)
mtext("High levels", side= 1, cex= 1)
pie3D(veryhigh.glucose, theta= pi/3,
      labels= veryhigh.labels, labelcex= 0.8,
      col= veryhigh.colours,
      start= pi/2, explode= 0.1)
mtext("Very high levels", side= 1, cex= 1)
mtext("Cardiovascular disease per glucose levels",
      side= 3, line= -6, outer= T, cex= 2)
# Strong correlation between glucose levels and cardiovascular disease.

## smoking, alcohol, physical activity
## smoking
### non-smokers
nosmokers= cardio.data[which(cardio.data$smoke == 0), ]$cardio
nosmokers.0= length(nosmokers[which(nosmokers == 0)])
nosmokers.1= length(nosmokers[which(nosmokers == 1)])
nosmokers.total= nosmokers.0 + nosmokers.1
nosmokers.0.pct= round(nosmokers.0/nosmokers.total*100)
nosmokers.1.pct= round(nosmokers.1/nosmokers.total*100)
### smokers
smokers= cardio.data[which(cardio.data$smoke == 1), ]$cardio
smokers.0= length(smokers[which(smokers == 0)])
smokers.1= length(smokers[which(smokers == 1)])
smokers.total= smokers.0 + smokers.1
smokers.0.pct= round(smokers.0/smokers.total*100)
smokers.1.pct= round(smokers.1/smokers.total*100)
### pie-charts smokers
nosmokers.labels= c(
   paste('No CVD:', nosmokers.0.pct),
   paste('CVD:',    nosmokers.1.pct)
)
nosmokers.labels= paste0(nosmokers.labels, '%')
nosmokers.colours= c('#85C1E9', '#F8C471')
smokers.labels= c(
   paste('No CVD:', smokers.0.pct),
   paste('CVD:',    smokers.1.pct)
)
smokers.labels= paste0(smokers.labels, '%')
smokers.colours= c('#B2BABB', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(nosmokers.0, nosmokers.1), theta= pi/3,
      labels= nosmokers.labels, labelcex= 0.8,
      col= nosmokers.colours,
      start= pi/6, explode= 0.1)
mtext("Non-smokers", side= 1, cex= 1)
pie3D(c(smokers.0, smokers.1), theta= pi/3,
      labels= smokers.labels, labelcex= 0.8,
      col= smokers.colours,
      start= pi/6, explode= 0.1)
mtext("Smokers", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Smoking",
      side= 3, line= -4, outer= T, cex= 2)
# No apparent meaningful correlation between smoking and CVD.

## alcohol
### non-drinkers
nodrinkers= cardio.data[which(cardio.data$alco == 0), ]$cardio
nodrinkers.0= length(nodrinkers[which(nodrinkers == 0)])
nodrinkers.1= length(nodrinkers[which(nodrinkers == 1)])
nodrinkers.total= nodrinkers.0 + nodrinkers.1
nodrinkers.0.pct= round(nodrinkers.0/nodrinkers.total*100)
nodrinkers.1.pct= round(nodrinkers.1/nodrinkers.total*100)
### drinkers
drinkers= cardio.data[which(cardio.data$alco == 1), ]$cardio
drinkers.0= length(drinkers[which(drinkers == 0)])
drinkers.1= length(drinkers[which(drinkers == 1)])
drinkers.total= drinkers.0 + drinkers.1
drinkers.0.pct= round(drinkers.0/drinkers.total*100)
drinkers.1.pct= round(drinkers.1/drinkers.total*100)
### pie-charts alcohol intake
nodrinkers.labels= c(
   paste('No CVD:', nodrinkers.0.pct),
   paste('CVD:',    nodrinkers.1.pct)
)
nodrinkers.labels= paste0(nodrinkers.labels, '%')
nodrinkers.colours= c('#85C1E9', '#F8C471')
drinkers.labels= c(
   paste('No CVD:', drinkers.0.pct),
   paste('CVD:',    drinkers.1.pct)
)
drinkers.labels= paste0(drinkers.labels, '%')
drinkers.colours= c('#D7BDE2', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(nodrinkers.0, nodrinkers.1), theta= pi/3,
      labels= nodrinkers.labels, labelcex= 0.8,
      col= nodrinkers.colours,
      start= pi/6, explode= 0.1)
mtext("Non-drinkers", side= 1, cex= 1)
pie3D(c(drinkers.0, drinkers.1), theta= pi/3,
      labels= drinkers.labels, labelcex= 0.8,
      col= drinkers.colours,
      start= pi/6, explode= 0.1)
mtext("Drinkers", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Alcohol Intake",
      side= 3, line= -4, outer= T, cex= 2)
# No apparent correlation between alcohol intake and CVD.

## physical activity
### sedentaries
sedentary= cardio.data[which(cardio.data$active == 0), ]$cardio
sedentary.0= length(sedentary[which(sedentary == 0)])
sedentary.1= length(sedentary[which(sedentary == 1)])
sedentary.total= sedentary.0 + sedentary.1
sedentary.0.pct= round(sedentary.0/sedentary.total*100)
sedentary.1.pct= round(sedentary.1/sedentary.total*100)
### physically active
active= cardio.data[which(cardio.data$act == 1), ]$cardio
active.0= length(active[which(active == 0)])
active.1= length(active[which(active == 1)])
active.total= active.0 + active.1
active.0.pct= round(active.0/active.total*100)
active.1.pct= round(active.1/active.total*100)
### pie charts physical activity
sedentary.labels= c(
   paste('No CVD:', sedentary.0.pct),
   paste('CVD:',    sedentary.1.pct)
)
sedentary.labels= paste0(sedentary.labels, '%')
sedentary.colours= c('#FFEDA0', '#F8C471')
active.labels= c(
   paste('No CVD:', active.0.pct),
   paste('CVD:',    active.1.pct)
)
active.labels= paste0(active.labels, '%')
active.colours= c('#A9DFBF', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(active.0, active.1), theta= pi/3,
      labels= active.labels, labelcex= 0.8,
      col= active.colours,
      start= pi/6, explode= 0.1)
mtext("Physically active", side= 1, cex= 1)
pie3D(c(sedentary.0, sedentary.1), theta= pi/3,
      labels= sedentary.labels, labelcex= 0.8,
      col= sedentary.colours,
      start= pi/6, explode= 0.1)
mtext("Sedentaries", side= 1, cex= 1)
mtext("Cardiovascular disease ~ Physical Activity",
      side= 3, line= -4, outer= T, cex= 2)
# Very small apparent correlation between physical activity and CVD.

## systolic and diastolic blood pressure
summary(aphi)
summary(aplo)
### remove outliers (POSSIBLY NOT TO INCLUDE)
aphi.out= boxplot.stats(aphi)$out
aphi.out.idx= !(aphi %in% aphi.out)
aphi.out= aphi[aphi.out.idx]
summary(aphi.out)
cardio.aphi.out= cardio[aphi.out.idx]
aplo.out= boxplot.stats(aplo)$out
aplo.out.idx= !(aplo %in% aplo.out)
aplo.out= aplo[aplo.out.idx]
summary(aplo.out)
cardio.aplo.out= cardio[aplo.out.idx]
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
boxplot(aphi.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1.0, cex= 1,
      expression(paste("Systolic blood pressure")))
mtext(side= 3, line= 1, at= 1.0, cex= 0.7, "Outliers removed")
### diastolic blood pressure
boxplot(aplo.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1,
      expression(paste("Diastolic blood pressure")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")
### histograms
hist(aphi.out, breaks= seq(min(aphi.out), max(aphi.out), by= 10),
     xlab= 'Systolic blood pressure (mmHg)',
     main= "")
hist(aplo.out, breaks= seq(min(aplo.out), max(aplo.out), by= 10),
     xlab= 'Diastolic blood pressure (mmHg)',
     main= '')
mtext("Histograms of blood pressure",
      side= 3, line= -2, outer= T, cex= 2)

## create dataframe
data.set= data.frame(age=     age,
                     gender=  gender,
                     height=  height,
                     weight=  weight,
                     aphi=    aphi,
                     aplo=    aplo,
                     choles=  choles,
                     glucose= glucose,
                     smoke=   smoke,
                     alcohol= alcohol,
                     active=  active0,
                     cardio=  cardio)

headtail(data.set)

## linear regression
### null model
mod.lr.0= lm(cardio ~ 1)
summary(mod.lr.0)
anova(mod.lr.0)
### model with all variables
mod.lr= lm(cardio ~ ., data= data.set)
mod.lr
#mod= lm(cardio ~
#           age  +
#           gender +
#           height +
#           weight +
#           aphi +
#           aplo +
#           choles +
#           glucose +
#           smoke +
#           alcohol +
#           active0)
#summary(mod)

# TODO
# - normality test for all features (qqplots and Jarque-Bera test)
# - check aphi and aplo values and possibly remove outliers
# - perform one-hot encoding in cholesterol and glucose levels
# - repeat linear regressions after one-hot encoding
# - check for significant variables and Cook's distance/leverages
# - check for confounding variables
# - apply models studied in SDA (LDA, QDA, etc.)
# - apply models to pairs of variables, using the target variable as classifier