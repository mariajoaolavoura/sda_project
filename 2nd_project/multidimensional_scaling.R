
# Following the website
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/

# libraries
library(smacof) 
library(magrittr)
library(dplyr)
library(ggpubr)
library(MASS)

# seed
set.seed(123)

## Read data frame
#df = readRDS("./data/gc.dat") 
#df = readRDS("./data/location.dat") 
df = readRDS("./data/dynamics.dat") 

# correlation matrix
df_cor = cor(df)

# dissimilarities
dissim = sim2diss(df_cor, method = "corr", to.dist = TRUE)

mds = #(1 - df_cor) %>%
      dissim %>%
        cmdscale() #classical MDS
mds
summary(mds)
mds.tb = mds %>% 
           as_tibble()

colnames(mds.tb) = c("Dim.1", "Dim.2")
ggscatter(mds.tb, x = "Dim.1", y = "Dim.2", 
          size = 1,
          label = colnames(df_cor),
          repel = TRUE,
          title = "Classical MDS - dynamics data set")

# K-means clustering
# clust = kmeans(mds.tb, 2)$cluster %>%
#                             as.factor()
# mds.clust = mds.tb %>%
#                mutate(groups = clust)
# # Plot and color by groups
# g = ggscatter(mds.clust, x = "Dim.1", y = "Dim.2", 
#           label = rownames(df_cor),
#           color = "groups",
#           palette = "jco",
#           size = 1, 
#           ellipse = TRUE,
#           ellipse.type = "convex",
#           repel = TRUE,
#           title = "Classical MDS")
#g
#g %>% ggexport(filename="msd_clust.png")


# There are different types of MDS algorithms, including
# 
# Classical multidimensional scaling
# 
# Preserves the original distance metric, between points, as well as possible. That is the fitted distances on the MDS map and the original distances are in the same metric. Classic MDS belongs to the so-called metric multidimensional scaling category.
# 
# It's also known as principal coordinates analysis. It's suitable for quantitative data.
# 
# Non-metric multidimensional scaling
# 
# It's also known as ordinal MDS. Here, it's not the metric of a distance value that is important or meaningful, but its value in relation to the distances between other pairs of objects.
# 
# Ordinal MDS constructs fitted distances that are in the same rank order as the original distance. For example, if the distance of apart objects 1 and 5 rank fifth in the original distance data, then they should also rank fifth in the MDS configuration.
# 
# It's suitable for qualitative data.


# Mathematically and conceptually, there are close correspondences between MDS and other methods used to reduce the dimensionality of complex data, such as Principal components analysis (PCA) and factor analysis.
# 
# PCA is more focused on the dimensions themselves, and seek to maximize explained variance, whereas MDS is more focused on relations among the scaled objects.
# 
# MDS projects n-dimensional data points to a (commonly) 2-dimensional space such that similar objects in the n-dimensional space will be close together on the two dimensional plot, while PCA projects a multidimensional space to the directions of maximum variability using covariance/correlation matrix to analyze the correlation between data points and variables.
