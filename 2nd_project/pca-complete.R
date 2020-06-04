## libraries
library(FactoMineR)
library(ggplot2)

set.seed(123)

## Read data frame
loc = readRDS("./data/location.dat") 
dyn = readRDS("./data/dynamics.dat") 

df = cbind(loc, dyn)

# metal     Log of metallicity wrt solar metallicity              numeric
# mv        Absolute magnitude                                    numeric
# r.core    Core radius (pc)                                      numeric
# r.tidal   Tidal radius (parsec)                                 numeric
# conc      Core concentration parameter                          numeric
# log.t     Logarithm of central relaxation timescale (yr)        numeric
# log.rho   Logarithm of central density (M_sun/pc^3)             numeric
# s0        Central velocity dispersion (km/s)                    numeric
# v.esc     Central escape velocity (km/s)                        numeric
# vhb       Level of the horizontal branch (mag)                  numeric
# e.bv      B-V colour excess (mag)                               numeric
# bv        B-V colour index (mag)                                numeric
# ellipt    Ellipticity                                           numeric
# v.t       Integrated V magnitude (mag)                          numeric
# csb       Central surface brightness (mag/arcsec^2)             numeric
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
png("comp_eigen_barplot.png")
barplot(pca$eig[,1],
        main="Eigenvalues",
        names.arg=1:nrow(pca$eig))
dev.off()


# There are no contributions above 67%, no PC is an outlier.
# By 1) keep 5 PC
# By 2) keep 5 or 6 PC
# By 3) keep 5 PC
# By "majority of vote" we keep the first 5 PC
# With the first 5 PC we explain ~82% of the total variance.



# In general, for each column/dimension/PC see which rows have high 
# values. The respective rows will allow us to know what does the 
# current. PC explains better and what it opposes.

## Interpretation - In regard of the variables
png("comp_pca_graph_var_dim12.png")
plot(pca, axes = c(1,2), choix = c("var"), col.var="black")
dev.off()
png("comp_pca_graph_var_dim13.png")
plot(pca, axes = c(1,3), choix = c("var"), col.var="black")
dev.off()
png("comp_pca_graph_var_dim14.png")
plot(pca, axes = c(1,4), choix = c("var"), col.var="black")
dev.off()
png("comp_pca_graph_var_dim15.png")
plot(pca, axes = c(1,5), choix = c("var"), col.var="black")
dev.off()

# the signs of the arrows correspond to the sign of the eigenvectors

# [review]
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

# [review]
# Dim 1
# -high positive corr r.core and csb
#  relatively high corr mv, log.t, ellipt
#  high negative corr conc, log.rho, s0 and v.esc
# -explains well ...
#  explains relatively well ...
# -oposes the ones ...

# Dim 2
# -high positive corr e.bv and bv
#  relatively high negative r.tidal
# -explains well ...
# -oposes the ...

# Dim 3
# -relatively hight positive corr vhb
#  close to 0.5 corr s0
#  close to -0.5 corr mv
#  overall small correlations
# -explains ...
# -oposes ...

# Dim 4 
# -relatively hight positive corr vhb
#  close to 0.5 corr conc, vhb
#  overall small correlations
# -explains ...
# -oposes ...

# Dim 5
# -hight positive corr v.t
#  all small correlations but v.t
# -explains ...
# -oposes ...


## Contributions
pca$var$contrib
# the variables which have the highest correlation with each PC
# will be those that contribute more
# Dim 1 has ~% for 
# Dim 2 has ~% for 
# Dim 3 has ~% for 
# Dim 4 has ~% for 
# Dim 5 has ~% for 


## Interpretation - In regard of the individuals
# g = ggplot(df, aes(x=gal.lat, y=gal.long))+
#      geom_point()+
#       geom_point(x=0,y=0)+
#        geom_hline(yintercept=0)+
#         labs(title="Distribution of globular clusters through space",
#            x="latitude",
#            y="longitude")
# g
#ggsave("comp_clusters_coord.png", g)


## coordinates of the individuals
## which ones are more extreme in each PC
pca$ind$coord
png("comp_pca_graph_ind_dim12.png")
plot(pca, axes = c(1,2), choix = c("ind","var","varcor"), col.var="black")
dev.off()
png("comp_pca_graph_ind_dim13.png")
plot(pca, axes = c(1,3), choix = c("ind","var","varcor"), col.var="black")
dev.off()
png("comp_pca_graph_ind_dim15.png")
plot(pca, axes = c(1,4), choix = c("ind","var","varcor"), col.var="black")
dev.off()
png("comp_pca_graph_ind_dim15.png")
plot(pca, axes = c(1,5), choix = c("ind","var","varcor"), col.var="black")
dev.off()

# [review]
# Dim 1 vs Dim 2 - Clusters 110, 6, 5, 13, 20 are more extreme
# Dim 1 vs Dim 3 - Clusters 13, 5, 6, 20, 63 are more extreme
# Dim 1 vs Dim 4 - Clusters 110, 6, 5, 13, 20 are more extreme
# Dim 1 vs Dim 5 - Clusters 110, 6, 5, 13, 20 are more extreme

# Analysing  ?
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
# g = ggplot(df[c(5,6,13,19,22),], aes(x=gal.lat, y=gal.long))+
#       geom_hline(yintercept=0)+
#       geom_point()+
#       geom_text(aes(label=c("5","6","13","19","22")),hjust=0, vjust=0)+
#       geom_point(x=0,y=0)+
#       lims(x=c(-3,2.8), y=c(-1.1,1.25))+
#       labs(title="Distribution of globular clusters through space",
#            x="latitude",
#            y="longitude")
# g
#ggsave("comp_clusters_specific_coord.png", g)


## Contributions
## the more extreme will have the higher contribution
pca$ind$contrib
dev.off()
png("comp_pca_graph_ind_dim12_contr5.png")
plot(pca, axes = c(1,2), select="contrib 5") # plot the 5 individuals with the highest contribution
dev.off()
png("comp_pca_graph_ind_dim13_contr5.png")
plot(pca, axes = c(1,3), select="contrib 5") # plot the 5 individuals with the highest contribution
dev.off()
png("comp_pca_graph_ind_dim14_contr5.png")
plot(pca, axes = c(1,4), select="contrib 5") # plot the 5 individuals with the highest contribution
dev.off()
png("comp_pca_graph_ind_dim15_contr5.png")
plot(pca, axes = c(1,5), select="contrib 5") # plot the 5 individuals with the highest contribution
dev.off()

pca$ind$contrib[c(5,6,13,20,63,110),]
#        Dim.1        Dim.2     Dim.3     Dim.4     Dim.5
# 5   9.041953 0.0327590724 1.0808625 3.2314396 0.6003686
# 6   6.227958 0.0003235155 0.7080374 2.9115575 1.0275141
# 13  8.144851 0.6774888968 3.1838859 2.9858328 0.1085088
# 20  5.399127 1.5786450153 0.1147228 0.1917435 0.4502503
# 63  3.368248 0.0790843542 7.0651195 0.2343696 2.1359336
# 110 4.236755 0.8433024479 0.6206252 0.1259535 2.5524507

## quality of representation - cos2
pca$ind$cos2
png("comp_pca_graph_ind_dim12_cos0.8.png")
plot(pca, axes = c(1,2), select="cos2 0.8")  # plot the individuals with cos2 greater than 0.8
dev.off()

png("comp_pca_graph_ind_dim12_cos5.png")
plot(pca, axes = c(1,2), select="cos2 5")    # plot the 5 individuals with the highest cos2 
dev.off()

pca$ind$cos2[c(24,29,35,107,111),]
#         Dim.1      Dim.2       Dim.3        Dim.4        Dim.5
# 24  0.6435605 0.28239257 0.006674570 0.0001956212 2.456637e-02
# 29  0.7914627 0.10209750 0.004367784 0.0606051430 1.787566e-05
# 35  0.6577576 0.25435749 0.003043227 0.0019793248 5.042810e-02
# 107 0.3449233 0.60967884 0.012559207 0.0011021431 1.162959e-02
# 111 0.8902896 0.01308037 0.043511433 0.0054274561 5.546657e-04

pca$ind$cos2[c(5,6,13,20,63,110),]
#         Dim.1       Dim.2        Dim.3        Dim.4      Dim.5
# 5   0.8462196 0.0016475141 0.028374647 0.071549596 0.008719236
# 6   0.8313423 0.0000232063 0.026511212 0.091949611 0.021284414
# 13  0.7717523 0.0344963939 0.084623594 0.066934589 0.001595510
# 20  0.7582621 0.1191398347 0.004519439 0.006370987 0.009812722
# 63  0.5391404 0.0068024458 0.317217031 0.008875428 0.053054869
# 110 0.7304440 0.0781292554 0.030013883 0.005137529 0.068288986

# [review]
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

#[review]
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

# [review]
# PC1 has a 41% of total variance
# PC2 has a 22% of total variance
# PC3 has a 11% of total variance
# PC4 has a 10% of total variance
# PC5 has a 6% of total variance
# The 5 together explain 90% of the total variance

