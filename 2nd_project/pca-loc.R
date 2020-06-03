## libraries
library(FactoMineR)
library(ggplot2)

set.seed(123)

## Read data frame
df = readRDS("./data/location.dat") 

# gal.long  Galactic longitude (degrees)                          numeric
# gal.lat   Galactic latitude (degrees)                           numeric
# r.sol     Distance from the Sun (kpc)                           numeric
# r.gc      Distance from the Galactic Centre (kpc)   

# View(df)
# dim(df)
# str(df)
summary(df)

## PCA
pca = PCA(df)

## How many components should be kept? (look at the eigenvalues)
# 1) Pearson's criterion:
#   Keep a number q of components such that they explain at least 
#   80% of the total dispersion.
# 2) Cattell's criterion:
#   elbow's rule for the eigenvalues
# 3) Kaiser criterion:
#   Normed PCA : only keep the eigenvalues above 1 

# If a PC has >67% contribution, is an outlier. 
# Run again the PC without those cases.
# The outlier in the original data set might be defining by 
# themselves PC and therefore provide a solution that is not general

## Eigenvalues
pca$eig 

## easier to see the "elbow"
#png("loc_eigen_barplot.png")
barplot(pca$eig[,1],
        main="Eigenvalues",
        names.arg=1:nrow(pca$eig))
#dev.off()


# There are no contributions above 67%, no PC is an outlier.
# By 1) keep 3 PC
# By 2) keep 3 PC
# By 3) keep 2
# By "majority of vote" we keep the first 3 PC
# with the first 3 PC we explain 99% of the total variance



# In general, for each column/dimension/PC see which rows have high 
# values. The respective rows will allow us to know what does the 
# current. PC explains better and what it opposes.

## Interpretation - In regard of the variables
#png("loc_pca_graph_var_dim12.png")
plot(pca, axes = c(1,2), choix = c("var"), col.var="black")
#dev.off()
#png("loc_pca_graph_var_dim13.png")
plot(pca, axes = c(1,3), choix = c("var"), col.var="black")
#dev.off()

# the signs of the arrows correspond to the sign of the eigenvectors

# Dim 1 has about the same high positive value for r.sol and r.gc. 
# A smaller but still positive value for gal.long and something close
# to gal.long simetric for gal.lat. Dim 1 represents completely globular 
# clusters that are further away to the sun or the galaxy center and
# expresses, not so well, the clusters at the "bottom center left" of
# the galaxy. Therefore, Dim 1 oposes globular clusters that are 
# closer to our Sun or to the galactic center, in other words, clusters
# that are closer to the galactic disk, being more exposed to dust and 
# radiation, which are harder to see i.e. have greater extinction.

# Dim 2 has a high positive value for gal.long, the symmetric for
# gal.lat and zero for r.sol and r.gc. It expresses well the coordinates
# of the globular clusters that are in the "right upper corner" of the 
# galaxy. It expresses nothing of r.sol and r.gc (as expected, since 
# Dim1 already explained almost everything). It oposes the clusters 
# that are in the "left lower corner". This dimension represents well
# clusters that are far away from the galactic disk, meaing they are 
# less exposed to dust and are easier to see, i.e less extinction. Its 
# oposition, "left lower corner", represents the same situation,
# globular clusters with lower extinction.

# Dim 3 has a relatively high negative value for gal.long, a 
# relatively high positive value for gal.lat and a small positive 
# value for r.sol and r.gc. This dimension represents the symmetric of
# Dim 2, coordinates wise. It expresses relatively well the 
# coordinates of the globular clusters that are in the "right lower
# corner" of the galaxy and expresses, not so well, the clusters that
# are not so far away from the galactic center and our sun. It oposes 
# the clusters that are in the "left upper corner". It expresses 
# relatively well clusters that are far away from the galactic disk, 
# meaing they are less exposed to dust and are easier to see, i.e less
# extinction. Its oposition, "left upper corner", represents the same
# situation, globular clusters with lower extinction. 



## correlations variables - dimensions/PC
pca$var$cor
# Dim 1
# -high positive corr r.sol and r.gc, 0.984 and 0.985
# -explains well the ones further away from the sun and galatic center
# -oposes the ones that are closer to the galatic disk

# Dim 2
# -high positive corr gal.long and g.lat, 0.736 and 0.735
# -explains well the coordinates of the globular clusters
# that are in the "right upper corner" of the galaxy
# -oposes the ones that are "left lower corner" the galaxy

# Dim 3
# -relatively hight negative correlation for gal.long, -0.652
# relatively hight positive correlation for gal.lat, 0.652
# -explains the inverse of Dim 2
# explains well the coordinates of the globular clusters [review]
# that are "right lower corner" the galaxy 
# -oposes the ones that are "left upper corner" the galaxy

# Dim 4 
# was not chosen and displays very small correlations

## Contributions
pca$var$contrib
# the variables which have the highest correlation with each PC
# will be those that contribute more
# Dim 1 has ~48% for r.sol and r.gc
# Dim 2 has ~50% for gal.long and gal.lat
# Dim 3 has ~48% for gal.long and gal.lat and ~2% for r.sol and r.gc


## Interpretation - In regard of the individuals
g = ggplot(df, aes(x=gal.lat, y=gal.long))+
     geom_point()+
      geom_point(x=0,y=0)+
       geom_hline(yintercept=0)+
        labs(title="Distribution of globular clusters through space",
           x="latitude",
           y="longitude")
g
#ggsave("loc_clusters_coord.png", g)


## coordinates of the individuals
## which ones are more extreme in each PC
pca$ind$coord
#png("loc_pca_graph_ind_dim12.png")
plot(pca, axes = c(1,2), choix = c("ind","var","varcor"), col.var="black")
#dev.off()
#png("loc_pca_graph_ind_dim13.png")
plot(pca, axes = c(1,3), choix = c("ind","var","varcor"), col.var="black")
#dev.off()

# Dim 1 vs Dim 2 - Clusters 5, 6, 13, 19, 20 are more extreme
# Dim 1 vs Dim 3 - Clusters 5, 6, 13, 22, 8 are more extreme

# Analysing 5,6,13,19 and 22
# 5 and 6 are positive and high in Dim 1, meaning they are further 
# away from our sun and the galactic center. They are negative and 
# small in Dim 2, meaning they might be closer to the "center" but in 
# the "left" side of the galaxy. Both of them are next to zero in 
# Dim 3, which is the symmetric of Dim 2, this way it reinforces the 
# conclusions of being in the center of the plot? or
# doesnt have any influence over them? . So, 5 and 6 seem to
# be on the "center left" not so close to the galactic center or sun.

# 13 is positive and relatively high in Dim 1, meaning it's not so 
# further away from our sun and the galactic center. It is positive and
# relatively small in Dim 2, meaning it might be more at the "right 
# side" of the "center". It is positive and relatively high in Dim 3,
# which is the symetric of Dim 2, meaning it might be closer to the 
# "right lower corner" of the galaxy. Because of the contradictions
# caused by Dim 2 and 3 and since Dim 2 explains a bigger percentage
# of the total variance, the conclusions of Dim 2 will be taken into
# consideration over the Dim 3's. So, 13 seems to be on the "center 
# right" and relatively close to the sun or galactic center.

# 19 has small positive value in Dim 1, meaning it's not so further
# away of our sun and the galactic center. It's positive and high 
# in Dim 2 meaning it might be in the "right upper corner" of the 
# galaxy. It's positive and relatively small in Dim 3, meaning it 
# might be in between the "center right" and the "right lower corner"
# of the galaxy. This way, 19 seems to be closer to the "right upper 
# corner" than the "right lower corner" of the galaxy and somewhat 
# away from the galactic center and our sun.

# 22 has small negative value in Dim 1, meaning it's closer to our 
# sun and the galactic center. It's positive and relatively small
# in Dim 2 meaning it would be in between of "right upper corner" and 
# "center right" of the galaxy. It's positive and high in Dim 3, 
# meaning it would be on the "right lower corner" of the galaxy. 
# This way, 22 seems to be close to the "right lower corner" of the 
# galaxy and somewhat close to the galactic center and our sun.

# as proof of what was concluded is write... (I'm very clever ehehe)
g = ggplot(df[c(5,6,13,19,22),], aes(x=gal.lat, y=gal.long))+
      geom_hline(yintercept=0)+
      geom_point()+
      geom_text(aes(label=c("5","6","13","19","22")),hjust=0, vjust=0)+
      geom_point(x=0,y=0)+
      lims(x=c(-3,2.8), y=c(-1.1,1.25))+
      labs(title="Distribution of globular clusters through space",
           x="latitude",
           y="longitude")
g
#ggsave("loc_clusters_specific_coord.png", g)


## Contributions
## the more extreme will have the higher contribution
pca$ind$contrib
#dev.off()
#png("loc_pca_graph_ind_dim12_contr5.png")
plot(pca, axes = c(1,2), select="contrib 5") # plot the 5 individuals with the highest contribution
#dev.off()
#png("loc_pca_graph_ind_dim13_contr5.png")
plot(pca, axes = c(1,3), select="contrib 5") # plot the 5 individuals with the highest contribution
#dev.off()

pca$ind$contrib[c(5,6,13,19,22),]
#          Dim.1     Dim.2       Dim.3     Dim.4
# 5  39.04292366 0.4590550 0.005917569 0.3220284
# 6  20.15196533 0.4803917 0.001392067 0.5775253
# 13 19.21680635 1.6023957 3.732356358 0.1542186
# 19  0.05815655 6.2899501 1.676790230 0.4472714
# 22  0.11902428 1.5250545 6.294405448 1.3400676

## quality of representation - cos2
pca$ind$cos2
#png("loc_pca_graph_ind_dim12_cos0.8.png")
plot(pca,select="cos2 0.8")  # plot the individuals with cos2 greater than 0.8
#dev.off()
#png("loc_pca_graph_ind_dim12_cos5.png")
plot(pca,select="cos2 5")    # plot the 5 individuals with the highest cos2 
#dev.off()

pca$ind$cos2[c(5,6,29,99,104),]
#          Dim.1       Dim.2        Dim.3        Dim.4
# 5   0.99349858 0.006304476 6.622176e-05 0.0001307202
# 6   0.98682259 0.012696281 2.997881e-05 0.0004511453
# 29  0.02042638 0.969580345 4.739072e-03 0.0052542078
# 99  0.19758399 0.796017482 2.020026e-03 0.0043785066
# 104 0.03543445 0.952402252 7.329558e-03 0.0048337413

pca$ind$cos2[c(5,6,13,19,22),]
#         Dim.1       Dim.2        Dim.3        Dim.4
# 5  0.99349858 0.006304476 6.622176e-05 0.0001307202
# 6  0.98682259 0.012696281 2.997881e-05 0.0004511453
# 13 0.88452778 0.039806995 7.555199e-02 0.0001132376
# 19 0.01385522 0.808763369 1.756816e-01 0.0016998468
# 22 0.03189613 0.220570355 7.418049e-01 0.0057286574
# All globular clusters are well represented with only one dimension.
# with 3 dimensions all get to a cos2 of 0.99.
# 5, 6 and 13 are represented really well, 0.99, 0.98 and 0.88, in
# Dim 1. With the 3 PC they have a cos2 of 0.9998693, 0.9995489 and 
# 0.9998868.
# 19 is well represented, 0.81, in Dim 2. With 3 PC it has a cos2 of
# 0.9983002
# 22 is well represented, 0.74, in Dim 3. With the 3 PC it has a cos2
# of 0.9942713


## Interpretation of the principal components
# look into the eigenvectors (Vij)
# variance explained by PC and its relative importance accessed 
# by eigenvalues (pca$eig)

# repeated (to remember)
# In general, for each column/dimension/PC see which rows have high 
# values. The respective rows will allow us to know what does the 
# current. PC explains better and what it opposes.


## PC
# Linear combinations of the standardized original variables
# c1 = V11*Z1 + V12*Z2 + ...

## Eigenvectors
pca$svd$V 

# Vector 1 has basically the same high positive value of 0.695 and 
# 0.696 for r.sol and r.gc. It has a small positive value of 0.127 
# for gal.long and a small negative value, almost symmetric, of -0.129
# for gal.lat. From this vector we can define the 1st PC as linear 
# combinations of the standardized original variables. So, PC1 is 
# PC1 = 0.127*Z_gal.long-0.128*Z_gal.lat+0.695*Z_r.sol+0.695*Z_r.gc.
# Tis PC represents very well globular 
# clusters that are further away to the sun or the galaxy center.
# Therefore, Dim 1 oposes globular clusters that are closer to our 
# Sun or to the galactic center, in other words, clusters that are 
# closer to the galactic disk, being more exposed to dust and 
# radiation, so harder to see i.e. have greater extinction.

# PC2 = 0.708*Z_gal.long+0.706*Z_gal.lat+0.002*Z_r.sol-0.0005*Z_r.gc.
# Dim 2 expresses well the coordinates of the globular clusters? [review]
# that are in the "right upper corner" of the galaxy and expresses 
# nothing of r.sol and r.gc (as expected). 
# It oposes the clusters that are in the "left lower corner".

# be PC3 = -0.695*Z_gal.long-0.696*Z_gal.lat+0.133*Z_r.sol+0.122*Z_r.gc.
# relatively hight negative correlation for gal.long, -0.652
# relatively hight positive correlation for gal.lat, 0.652
# explains the inverse of Dim 2
# explains well the coordinates of the globular clusters [review]
# that are "right lower corner" the galaxy 
# oposes the ones that are "left upper corner" the galaxy


## Eigenvalues
pca$eig
# PC1 has a 50% of total variance
# PC2 has a 27% of total variance
# PC3 has a 22% of total variance
# The 3 together explain 99% of the total variance

