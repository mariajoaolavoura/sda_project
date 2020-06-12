# libraries
library(psych)
library(FactoMineR)
#library(car)
library(ggplot2)
library(GGally) # ggcorr
library(ggpubr)

# seed
set.seed(123)

plot_loadings = function(l, label){
  names(l) = c("RC1", "RC2")
  ggscatter(l, x = "RC1", y = "RC2", 
            size = 1,
            label = label,
            repel = TRUE,
            title = "Factor Analysis - graph of variables")
  
}

df = readRDS("./data/location.dat")

ggcorr(df, geom = "blank", label = TRUE, hjust = 0.67, size=2.5) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)
#ggsave("fa_correlations2.png")

# Makes no sense perform factor analysis on localization data set



df = readRDS("./data/dynamics.dat")

ggcorr(df, geom = "blank", label = TRUE, hjust = 0.67, size=2.5) +
  geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)
#ggsave("fa_dyn_correlations.png")

df_cor = cor(df)

KMO(df_cor)

# Complete model
pca = PCA(df)
pca$eig 
# keep 4 PC by all rules

# Factor analysis from 4 PCA, so with 4 comon factors
fc = principal(df_cor, 4, rotate="varimax") 

# Comunalities
fc$communality

# Loadings
# They are not the PC, but the rotated PC (RC)
fc$loadings
print(fc$loadings,cutoff = 0.6)

# Residuals
fc
# RMSR is  0.05

# Plot
plot_loadings(data.frame(fc$loadings[,1:2]), names(df))

