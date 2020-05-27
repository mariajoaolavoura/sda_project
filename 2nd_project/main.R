library(GGally) # ggpairs
library(e1071) # skewness, kurtosis
library(ggplot2)
library(gridExtra)

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
  print(i)
  df[is.na(df[,i]), i] = round(mean(df[,i], na.rm = TRUE), 2)
}

colMeans(is.na(df))
unique(rowMeans(is.na(df)))
# Clean df

## Univariate Analysis

# Indicators (position, dispersion, form..), graphic representations, analysis of (possible) outliers
# Position : mean, trimmed mean, median - and comparison between them; quartiles,... 
# Dispersion: range and inter-quartile range, variance and standard deviation, coefficient of variation (for variables that do not change sign)
# Shape : skewness, kurtosis
summary(df)

boxplot(df[,-1])
# we can see a clear difference in position btw the different features
# The majority of them have outliers

# plot.univariate.analysis = function(x, name){
#   par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
#   print(summary(x))                               # mean - influenced by outliers, not so robust; median - better suited for skewed dist to derive at central tendency
#   #print(paste("range =", range(x)))
#   print(paste("inter-quartile range =", IQR(x)))  # less sensitive to outliers, useful to identify them
#   print(paste("variance =", var(x)))
#   print(paste("st. deviation =", sd(x)))          # how close the values are to the mean
#   print(paste("skewness =", skewness(x)))         # shape
#   print(paste("kurtosis =", kurtosis(x)))         # excess kurtosis describes the tail shape of the distribution. The normal distribution has zero excess kurtosis and thus the standard tail shape. It is said to be mesokurtic. Negative excess kurtosis would indicate a thin-tailed data distribution, and is said to be platykurtic. Positive excess kurtosis would indicate a fat-tailed distribution, and is said to be leptokurtic. 
#   
#   hist(x,
#        breaks= seq(round(min(x))-5, round(max(x))+5, by= 5),
#        main= paste("Boxplot of ", name))
#   boxplot(x, yaxt= 'n',
#           main= paste("Boxplot of ", name))
#   axis(2, las= 2)
# }

plot.univariate.analysis = function(df, ft, label){
  #par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
  print(summary(ft))
  print(paste("variance =", var(ft)))
  print(paste("skewness =", skewness(ft)))
  
  g1 = ggplot(df,aes(y=ft)) +
    labs(title=paste("Boxplot of",label), y=label) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    geom_boxplot()
  
  g2 = ggplot(df,aes(x=ft)) +
    labs(title=paste("Histogram of",label), x=label) +
    geom_histogram()
  
  g = grid.arrange(g1, g2, nrow=1)
  ggsave(paste("hist_and_box_raw",label,".png", sep="_"), g)
  print(g)
}


## Gal.long
plot.univariate.analysis(df, df$Gal.long, "Gal.long")
# neg kurtosis, platykurtic, thin tailed dist, consistent with not bel shaped
# No outliers
# u distribution

## Gal.lat
plot.univariate.analysis(df, df$Gal.lat, "Gal.lat")
# outliers
# normal distribution

## R.sol.kpc
plot.univariate.analysis(df, df$R.sol.kpc, "R.sol.kpc")
# outliers
# left skewed

## R.GC.kpc
plot.univariate.analysis(df, df$R.GC.kpc, "R.GC.kpc")
# outliers
# left skewed

## m.H.metal
plot.univariate.analysis(df, df$m.H.metal, "m.H.metal")
# no outliers
# right skewed?

## Mv
plot.univariate.analysis(df, df$Mv, "Mv")
# outliers
# normal dist

## r.core.pc
plot.univariate.analysis(df, df$r.core.pc, "r.core.pc")
# outliers
# left skewed

## r.tidal.pc
plot.univariate.analysis(df, df$r.tidal.pc, "r.tidal.pc")
# outliers
# left skewed

## conc
plot.univariate.analysis(df, df$conc, "conc")
# outliers
# normal dist

## log.t.rad
plot.univariate.analysis(df, df$log.t.rad, "log.t.rad")
# outliers
# normal dist?

## log.rho.cen
plot.univariate.analysis(df, df$log.rho.cen, "log.rho.cen")
# no outliers
# normal dist?

## s0.km.s
plot.univariate.analysis(df, df$s0.km.s, "s0.km.s")
# outliers
# left skewed

## V.esc.km.s
plot.univariate.analysis(df, df$V.esc.km.s, "V.esc.km.s")
# outliers
# left skewed

## VHB.magl
plot.univariate.analysis(df, df$VHB.magl, "VHB.magl")
# outliers
# normal dist?

## E.B.V.mag
plot.univariate.analysis(df, df$E.B.V.mag, "E.B.V.mag")
# outliers
# right skewed

## Ellipt
plot.univariate.analysis(df, df$Ellipt, "Ellipt")
# outliers
# normal dist

## V.t.mag
plot.univariate.analysis(df, df$V.t.mag, "V.t.mag")
# no outliers
# normal dist?

## Cent.surf.bright
plot.univariate.analysis(df, df$Cent.surf.bright, "Cent.surf.bright")
# outliers
# left skewed


## Outliers


## Bivariate Analysis

# Analysis of correlations for numerical variables.
# Contingency tables for some pairs (considered of particular interest) of categorical variables.
# Statistical tests (e.g. comparison of means/medians between populations) if pertinent

#ggpairs(df[,-1])

# ggcorr(df[,-1], label = TRUE)
# Several high correlations, even 100% correlations
ggcorr(df[,-1], geom = "blank", label = TRUE, hjust = 0.67, size=2.5) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)
ggsave("correlations.png")


# Center the variables

clean = df
clean[,-1] = scale(df[,-1])

write.csv(clean, "./data/cleaned.csv", row.names=F)


