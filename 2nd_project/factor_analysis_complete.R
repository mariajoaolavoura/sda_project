# libraries
library(psych)
library(FactoMineR)
library(car)
library(ggplot2)
library(GGally) # ggcorr
library(ggpubr)

# seed
set.seed(123)

plot_loadings = function(l, label){
  # par(mfrow=c(1,2))
  # plot(l)
  # plot(l, type="n")
  # text(l, labels=label, cex=0.7)
  names(l) = c("RC1", "RC2")
  ggscatter(l, x = "RC1", y = "RC2", 
            size = 1,
            label = label,
            repel = TRUE,
            title = "Factor Analysis - graph of variables")
  
}


## Read data frame
df = readRDS("./data/gc.dat") 

# KMO of corr
df_cor = cor(df)
KMO(df_cor)

# overall MSA not very high but not close to 0.5. (prof)
# MSA should be above 0.8 but above 0.6 is tolerable
# keep in the model: gal.long, r.sol, r.gc, r.core, r,tidal, conc, log.t, log.rho, s0, v.esc, csb
# keep out of the model: gal.lat, metal, mv, vhb, e.bv, bv, ellipt, v.t

# Complete model
pca = PCA(df)
pca$eig 
# keep 5 PC by all rules

# Factor analysis from 5 PCA, so with 5 comon factors
fc = principal(df_cor, 5, rotate="varimax") 

# Comunalities
fc$communality
# our 5 comon factor model explains 
# at least 70% of the total variability of all but gal.long, metal and v.t

# Loadings
# They are not the PC, but the rotated PC (RC)
fc$loadings
print(fc$loadings,cutoff = 0.6)

# gal.long does not explained well by one RC (alone)
# csb and ellipt have 2 that explain well

# RC1
# high positive value s0 (Central velocity dispersion), v.esc (Central escape velocity)
# high negative value mv (Absolute magnitude), ellipt (Ellipticity), csb (Central surface brightness)
# 

# RC4
# high positive value log.t (Logarithm of central relaxation timescale)
# high negative value conc (Core concentration parameter), log.rho (Logarithm of central density)
# 

# RC3
# high positive value r.sol, r.gc, vhb (Level of the horizontal branch), ellipt (Ellipticity)
# high negative value (none)
# 

# RC2
# high positive value e.bv (B-V colour excess), bv (B-V colour index )
# high negative value (none)
# 

# RC5
# high positive value gal.lat
# high negative value (none)
# 

# variance
# Proportion variability = comulative percentage of variance of PC
# = 81.84259

# correlation btw variables and the residuals
# we want a models that estimates correlations as close as to the 
# observed ones
# Residuals
fc
# RMSR is  0.05 which is good as this value should close to 0


####################################################################
# Following the MSA results and removing some variables
# keep in the model: gal.long, r.sol, r.gc, r.core, r,tidal, conc, log.t, log.rho, s0, v.esc, csb
# keep out of the model: gal.lat, metal, mv, vhb, e.bv, bv, ellipt, v.t

s_df = df[,-c(2,5,6,14,15,16,17,18)]

s_df_cor = cor(s_df)

KMO(s_df_cor)

# Complete model
s_pca = PCA(s_df)
s_pca$eig 
# keep 3 PC by all rules

# Factor analysis from 5 PCA, so with 5 comon factors
s_fc = principal(s_df_cor, 3, rotate="varimax") 

# Comunalities
s_fc$communality
# our 5 comon factor model explains 
# at least 85% of the total variability of all but gal.long and r.tidal

# Loadings
# They are not the PC, but the rotated PC (RC)
s_fc$loadings
print(s_fc$loadings,cutoff = 0.6)

# RC1
# high positive value r.core, log.t
# high negative value conc, log.rho
# 

# RC2
# high positive value r.sol, r.gc, r.tidal
# high negative value (none)
# 

# RC3
# high positive value s0 v.esc
# high negative value (none)
# 


# variance
# Proportion variability = comulative percentage of variance of PC
# = 82.59543

# correlation btw variables and the residuals
# we want a models that estimates correlations as close as to the observed ones


# Residuals
s_fc
s_fc$residual
# (RMSR) is  0.07 


####################################################################
# Removing the variables that showed no high correlation with any other
df2 = df[,-c(1,2,5)]

df_cor2 = cor(df2)

ggcorr(df2, geom = "blank", label = TRUE, hjust = 0.67, size=2.5) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)
#ggsave("fa_correlations2.png")


KMO(df_cor2)

# Complete model
pca2 = PCA(df2)
pca2$eig 
# keep 4 PC by all rules

# Factor analysis from 4 PCA, so with 4 comon factors
fc2 = principal(df_cor2, 4, rotate="varimax") 

# Comunalities
fc2$communality

# Loadings
# They are not the PC, but the rotated PC (RC)
fc2$loadings
print(fc2$loadings,cutoff = 0.6)

# Residuals
fc2
# RMSR is  0.03 

# Plot
plot_loadings(data.frame(fc2$loadings[,1:2]), names(df2))

