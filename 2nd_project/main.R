library(GGally) # ggpairs
library(e1071) # skewness

set.seed(123)

df = read.csv("./data/webbink-gc.csv")
View(df)
dim(df)
str(df)

summary(df)
# R.GC.kpc has 1 NA
# m.H.metal has 13 NA
# conc has 31 NA
# B.V.mag has 13 NA
# Cent.surf.bright has 1 NA

#ggpairs(df[,-1])


## NA Analysis
range(colMeans(is.na(df)))
# there are no columns with more than 20% missing values
# so there is no need to remove any
colMeans(is.na(df))
# "conc" is the one missing 20% of its values

range(rowMeans(is.na(df)))
unique(rowMeans(is.na(df)))
# no more than 15% of each row is missing

# we can procede to imputation of the centered value
na.columns = which(colMeans(is.na(df))>0)
for(i in na.columns){
  df[is.na(df[,i]), i] = round(mean(df[,i], na.rm = TRUE), 2)
}



## Univariate Analysis

# Indicators (position, dispersion, form..), graphic representations, analysis of (possible) outliers
# Position : mean, trimmed mean, median - and comparison between them ; quartiles,.
# Dispersion: range and inter-quartile range, variance and standard deviation, coefficient of variation (for variables that do not change sign)
# Shape : skewness, kurtosis
summary(df)

boxplot(df[,-1])
# we can see a clear difference in position btw the different features
# The majority of them have outliers

plot.univariate.analysis = function(x, name){
  par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
  print(summary(x))
  print(paste("variance =", var(x)))
  print(paste("skewness =", skewness(x)))
  hist(x,
       breaks= seq(round(min(x))-5, round(max(x))+5, by= 5),
       main= paste("Boxplot of ", name))
  boxplot(x, yaxt= 'n',
          main= paste("Boxplot of ", name))
  axis(2, las= 2)
}


## Gal.long
plot.univariate.analysis(df$Gal.long, "Gal.long")
# No outliers
# u distribution

## Gal.lat
plot.univariate.analysis(df$Gal.lat, "Gal.lat")
# outliers
# normal distribution

## R.sol.kpc
plot.univariate.analysis(df$R.sol.kpc, "R.sol.kpc")
# outliers
# left skewed

## R.GC.kpc
plot.univariate.analysis(df$R.GC.kpc, "R.GC.kpc")
# outliers
# left skewed

## m.H.metal
plot.univariate.analysis(df$m.H.metal, "m.H.metal")
# no outliers
# right skewed?

## Mv
plot.univariate.analysis(df$Mv, "Mv")
# outliers
# normal dist

## r.core.pc
plot.univariate.analysis(df$r.core.pc, "r.core.pc")
# outliers
# left skewed

## r.tidal.pc
plot.univariate.analysis(df$r.tidal.pc, "r.tidal.pc")
# outliers
# left skewed

## conc
plot.univariate.analysis(df$conc, "conc")
# outliers
# normal dist

## log.t.rad
plot.univariate.analysis(df$log.t.rad, "log.t.rad")
# outliers
# normal dist?

## log.rho.cen
plot.univariate.analysis(df$log.rho.cen, "log.rho.cen")
# no outliers
# normal dist?

## s0.km.s
plot.univariate.analysis(df$s0.km.s, "s0.km.s")
# outliers
# left skewed

## V.esc.km.s
plot.univariate.analysis(df$V.esc.km.s, "V.esc.km.s")
# outliers
# left skewed

## VHB.magl
plot.univariate.analysis(df$VHB.magl, "VHB.magl")
# outliers
# normal dist?

## E.B.V.mag
plot.univariate.analysis(df$E.B.V.mag, "E.B.V.mag")
# outliers
# right skewed

## Ellipt
plot.univariate.analysis(df$Ellipt, "Ellipt")
# outliers
# normal dist

## V.t.mag
plot.univariate.analysis(df$V.t.mag, "V.t.mag")
# no outliers
# normal dist?

## Cent.surf.bright
plot.univariate.analysis(df$Cent.surf.bright, "Cent.surf.bright")
# outliers
# left skewed


## Bivariate Analysis

# Analysis of correlations for numerical variables.
# Contingency tables for some pairs (considered of particular interest) of categorical variables.
# Statistical tests (e.g. comparison of means/medians between populations) if pertinent

ggpairs(df[,-1])
ggcorr(df[,-1], label = TRUE)
# Several high correlations, even 100% correlations

