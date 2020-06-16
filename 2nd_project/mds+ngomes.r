# header ------------------------------------------------------------------
###
# Statistical and Data Analysis
# Project 2
# Nuno R. C. Gomes & Maria João Lavoura
###


# Classical multidimensional scaling



# packages ----------------------------------------------------------------
require(ggpubr)
require(smacof)
require(tidyverse)



# data sets ---------------------------------------------------------------
dat.dyn= readRDS("./data/dynamics.dat")
dat.loc= readRDS("./data/location.dat")
dat.num= readRDS("./data/gc.dat")



# mds ---------------------------------------------------------------------
# dissimilarity matrix
diss.num= dist(dat.num)

# classic mds
mds.classic.num= torgerson(diss.num, 2)
plot(
  mds.classic.num,
  pch= 20,
  xlab= "",
  yaxt= 'n', ylab= ""
)
title(main= "Classic MDS", line= 0.2)
title(xlab= "Dimension 1", line= 2)
title(ylab= "Dimension 2", line= 2.2)
axis(2, las= 2)
text(mds.classic.num, labels= rownames(mds.classic.num), pos= 2)

# scaled mds (interval)
mds.scaled.interv.num= mds(diss.num, type= "interval")
mds.scaled.interv.num
# coordinates and stress per unit
summary(mds.scaled.interv.num)

plot(
  mds.scaled.interv.num,
  plot.type= "stressplot",
  xlab= "",
  ylab= "", yaxt= 'n',
  main= ""
)
title(main= "Stress Decomposition Chart", line= 0.4)
title(xlab= "Objects", line= 2.2)
axis(2, las= 2)
title(ylab= "Stress Proportion (%)", line= 2.4)

plot(
  mds.scaled.interv.num,
  plot.type= "bubbleplot",
  xlab= "",
  ylab= "", yaxt= 'n',
  main= ""
)
title(main= "Bubble Plot", line= 0.2)
title(xlab= "Dimension 1", line= 2)
title(ylab= "Dimension 2", line= 2.2)
axis(2, las= 2)
# no big contributions for stress
# dim1 separates 5, 13, 6 from the rest (maybe look at 63)
# dim2 separates 7 from 28, 26, 21, 19, 22



# alternative approach ----------------------------------------------------
mds.classic= dat.num %>%
  dist() %>%
  cmdscale() %>%
  as_tibble()
colnames(mds.classic)= c("Dim1", "Dim2")

ggscatter(
  mds.classic,
  label= rownames(mds.classic),
  main= "Classic MDS",
  x= "Dim1", y= "Dim2",
  xlab= "Dimension 1", ylab= "Dimension 2",
  size= 1,
  repel= T,
  ggtheme= theme_bw()
)

# K-means clustering
K= 5
clust.num= kmeans(mds.classic, K)$cluster %>%
  as.factor()
mds.classic= mds.classic %>%
  mutate(groups= clust.num)

# Plot and colour by groups
ggscatter(
  mds.classic,
  label= rownames(dat.num),
  main= "Classic MDS with K-means clustering",
  x= "Dim1", y= "Dim2",
  xlab= "Dimension 1", ylab= "Dimension 2",
  col= "groups",
  palette= "jco",
  size = 1,
  ellipse= T,
  ellipse.type= "convex",
  repel= T,
  ggtheme= theme_bw()
)
