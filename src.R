## Masters of Data Science
## Project of Statistics and Data Analysis, 2019/2020
## Nuno Gomes + Maria Joao Lavoura
## Prediction of coronary disease from nine predictors
## (eight numerical and one categorical) recorded in 462 medical exams.

# source for ideas: https://rpubs.com/lance4869/SAheart

# libraries
require(plotrix)
require(fastDummies)
require(car)

coron.dat0= read.csv("./data/coronary-disease.csv", sep= ';')
head(coron.dat0)
tail(coron.dat0)

# drop 'id' column
coron.dat= subset(coron.dat0, select= -c(ind))
summary(coron.dat)

# convert family history to 1/0
coron.dat$famhist= as.integer(coron.dat$famhist) - 1

# extract variables/features
adip=    coron.dat$adiposity # adiposity (body adiposity index, BAI, in %)
# https://www.intmath.com/functions-and-graphs/bmi-bai-comparison.php
age=     coron.dat$age       # age at time of heart attack
alcohol= coron.dat$alcohol   # current consumption of alcohol
bp=      coron.dat$sbp       # blood pressure
corond=  coron.dat$chd       # coronary disease
famhist= coron.dat$famhist   # family history
ldl=     coron.dat$ldl       # low density lipoprotein cholesterol (mg/dl)
obes=    coron.dat$obesity   # obesity = BMI (kg/m^2)
tobacco= coron.dat$tobacco   # cumulative tobacco (kg)
# https://onlinelibrary.wiley.com/doi/pdf/10.1111/bju.12400
typea=   coron.dat$typea     # behaviour type A
# Type A Behaviour Pattern (TABP): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3477961/

# histograms and boxplots
par(mfrow= c(1, 2))
# adiposity
hist(adip); boxplot(adip)

# age
hist(age); boxplot(age)
# little people in the range 20-25 years old

# alcohol
hist(alcohol);
balco= boxplot(alcohol)
min(balco$out)

# blood pressure
hist(bp); boxplot(bp)

# family history
hist(famhist); boxplot(famhist)

# LDL
hist(ldl); boxplot(ldl)

# obesity
hist(obes); boxplot(obes)

# tobbaco
hist(tobacco);
btob= boxplot(tobacco)
min(btob$out)

# type A
hist(typea);
boxplot(typea)


# test for multicolinearity
cor(coron.dat[c(-5, -10)])
# significant correlation between obesity and adiposity

# pairs
chd1= coron.dat[coron.dat$chd == 1, ]
chd0= coron.dat[coron.dat$chd == 0, ]
pairs(rbind(chd1, chd0), col= c("blue", "red"))

plot(adip, corond)
plot(obes, corond)
plot(adip, obes)
cor(adip, corond)
cor(obes, corond)
# adiposity is more correlated to coronary disease than obesity,
# so maybe remove obesity


mod1 = lm(chd~., data=coron.dat)
summary(model)
# adj R^2 is 0.2208, not at all close to 1, not good
# sbp, adiposity, obesity and alcohol are not statisticaly significant

# test for multicolinearity
vif(mod1)
# no value is grater than 10, so there isn't high multicoliarity, good!

# test for the normality of the residuals
par(mfrow=c(2,2))
hist(rstandard(mod1))
boxplot(rstandard(mod1))
qqnorm(rstandard(mod1))
qqline(rstandard(mod1))
hist(rstandard(mod1), breaks = 40)

par(mfrow=c(1, 2))
qqPlot(residuals(mod1))
qqPlot(mod1)

# test for homocedasticity
plot(fitted.values(mod1), rstandard(mod1))
# ??

# test for influent points
plot(hatvalues(mod1), cooks.distance(mod1))
# cooks distance inferior a 1, there are no influent points, good!


# by the pre selection done and the summary of the model, 
# let's just remove obesity first and analyse the model










# adiposity
summary(adip)
hist(adip,
     breaks= seq(round(min(adip))-5, round(max(adip))+5, by= 5))
boxplot(age, yaxt= 'n',
        main= "Boxplot of adiposity")
axis(2, las= 2)

# age
summary(age)
hist(age,
     breaks= seq(round(min(age))-5, round(max(age))+5, by= 5))
boxplot(age, yaxt= 'n',
        main= "Boxplot of age")
axis(2, las= 2)

# alcohol
# non-drinkers
alc0.chd= coron.dat[which(coron.dat$alcohol == 0), ]$chd
alc0.chd.0= length(alc0.chd[which(alc0.chd == 0)])
alc0.chd.1= length(alc0.chd[which(alc0.chd == 1)])
alc0.tot= alc0.chd.0 + alc0.chd.1
alc0.0.pct= round(alc0.chd.0/alc0.tot*100)
alc0.1.pct= round(alc0.chd.1/alc0.tot*100)
# drinkers
alc1.chd= coron.dat[which(coron.dat$alcohol != 0), ]$chd
alc1.chd.0= length(alc1.chd[which(alc1.chd == 0)])
alc1.chd.1= length(alc1.chd[which(alc1.chd == 1)])
alc1.tot= alc1.chd.0 + alc1.chd.1
alc1.0.pct= round(alc1.chd.0/alc1.tot*100)
alc1.1.pct= round(alc1.chd.1/alc1.tot*100)
# pie-charts alcohol intake
alc0.labs= c(
  paste('No CND:', alc0.0.pct),
  paste('CND:', alc0.1.pct))
alc0.labs= paste0(alc0.labs, '%')
alc0.cols= c('#85C1E9', '#F8C471')
alc1.labs= c(
  paste('No CND:', alc1.0.pct),
  paste('CND:', alc1.1.pct))
alc1.labs= paste0(alc1.labs, '%')
alc1.cols= c('#D7BDE2', '#F8C471')
par(mfrow= c(1, 2))
pie3D(c(alc0.chd.0, alc0.chd.1), theta= pi/3,
      labels= alc0.labs, labelcex= 0.8,
      col= alc0.cols,
      start= pi/6, explode= 0.1)
mtext("Non-drinkers", side= 1, cex= 1)
pie3D(c(alc1.chd.0, alc1.chd.1), theta= pi/3,
      labels= alc1.labs, labelcex= 0.8,
      col= alc1.cols,
      start= pi/6, explode= 0.1)
mtext("Drinkers", side= 1, cex= 1)
mtext("Coronal disease ~ Alcohol Intake",
      side= 3, line= -4, outer= T, cex= 2)

# TODO:
#   *missing values
#   *minimamente simetricos (hist, boxplot)
#   *outliers (hist, boxplot)
#   *linearmente relacionados com a resposta (pairs)
#   *multicolinearidade (corr(vars))

#   *analise modelo
#   *multicolinearidade (vif(model))
#   *homocedasticidade (plot fitted values vs stadard residuals)
#   *pontos influentes (cook dist, leverages(hatvalues) )



