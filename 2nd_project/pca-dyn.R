## libraries
library(FactoMineR)
library(ggplot2)

set.seed(123)

## Read data frame
df = readRDS("./data/dynamics.dat") 

# metal     Log of metallicity wrt solar metallicity              numeric -  refer to these heavier elements as metals and to the proportions of these elements as the metallicity. Hence the proportion of metals can be an indication of the age of a star, with older stars typically having a lower metallicity.
# mv        Absolute magnitude                                    numeric
# r.core    Core radius (pc)                                      numeric - core radius is the distance at which the apparent surface luminosity has dropped by half [https://en.wikipedia.org/wiki/Globular_cluster]
# r.tidal   Tidal radius (parsec)                                 numeric - the distance from the center of the globular cluster at which the external gravitation of the galaxy has more influence over the stars in the cluster than does the cluster itself. This is the distance at which the individual stars belonging to a cluster can be separated away by the galaxy. [https://en.wikipedia.org/wiki/Globular_cluster]
# conc      Core concentration parameter                          numeric
# log.t     Logarithm of central relaxation timescale (yr)        numeric - Logarithm of time interval for stars lose any history of their original velocity
# log.rho   Logarithm of central density (M_sun/pc^3)             numeric
# s0        Central velocity dispersion (km/s)                    numeric
# v.esc     Central escape velocity (km/s)                        numeric
# vhb       Level of the horizontal branch (mag)                  numeric
# e.bv      B-V colour excess (mag)                               numeric
# bv        B-V colour index (mag)                                numeric - is the difference between the magnitude of the star in blue light, or B, and the magnitude in visual light (green-yellow), or V. Large positive values indicate a red star with a cool surface temperature, while negative values imply a blue star with a hotter surface. 
# ellipt    Ellipticity                                           numeric - Although globular clusters generally appear spherical in form, ellipticities can occur due to tidal interactions.
# v.t       Integrated V magnitude (mag)                          numeric
# csb       Central surface brightness (mag/arcsec^2)             numeric

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
#png("dyn_eigen_barplot.png")
barplot(pca$eig[,1],
        main="Eigenvalues",
        names.arg=1:nrow(pca$eig))
#dev.off()


# There are no contributions above 67%, no PC is an outlier.
# By 1) keep 4 PC
# By 2) keep 4 or 5 PC
# By 3) keep 4
# By "majority of vote" we keep the first 4 PC
# we explain ~83% of the total variance.



# In general, for each column/dimension/PC see which rows have high 
# values. The respective rows will allow us to know what does the 
# current. PC explains better and what it opposes.

## Interpretation - In regard of the variables
#png("dyn_pca_graph_var_dim12.png")
plot(pca, axes = c(1,2), choix = c("var"), col.var="black")
#dev.off()
#png("dyn_pca_graph_var_dim13.png")
plot(pca, axes = c(1,3), choix = c("var"), col.var="black")
#dev.off()
#png("dyn_pca_graph_var_dim14.png")
plot(pca, axes = c(1,4), choix = c("var"), col.var="black")
#dev.off()

# the signs of the arrows correspond to the sign of the eigenvectors


## correlations variables - dimensions/PC
pca$var$cor
# Dim 1
# -high positive corr 
#             r.core (core radius) 
#             csb (central surface brightness)
#  relatively high corr 
#             mv (Absolute magnitude), 
#             log.t (Logarithm of central relaxation timescale), 
#             ellipt (Ellipticity)
#  high negative corr
#             conc (Core concentration parameter), 
#             log.rho (Logarithm of central density), 
#             s0 (Central velocity dispersion) and 
#             v.esc (Central escape velocity)
# -explains well globular clusters 
#             with big core radious, and high central surface brightness
#             with low core concentration, low density and slow velocities
#  explains relatively well gobular clusters with
#             high absolute magnitude, 
#             high Logarithm of central relaxation timescale
#             high ellipticity
# -oposes the ones that
#             are small, less bright, 
#             high core concentration, high densities and fast
#             low magnitude, low relaxation and less ellipticity

# Dim 2
# -high positive corr e.bv (B-V colour excess) and bv (B-V colour index)
#  relatively high negative r.tidal (Tidal radius)
# -explains well ...
# -oposes the ...

# Dim 3
# -relatively hight positive corr vhb (Level of the horizontal branch)
#  close to 0.5 corr s0 (Central velocity dispersion)
#  close to -0.5 corr mv (Absolute magnitude)
#  overall small correlations
# -explains ...
# -oposes ...

# Dim 4 
# -relatively hight positive corr vhb (Level of the horizontal branch)
#  close to 0.5 corr conc (Core concentration parameter)
#  overall small correlations
# -explains ...
# -oposes ...

# Dim 5
# -hight positive corr v.t (Integrated V magnitude)
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
#ggsave("dyn_clusters_coord.png", g)


## coordinates of the individuals
## which ones are more extreme in each PC
pca$ind$coord
#png("dyn_pca_graph_ind_dim12.png")
plot(pca, axes = c(1,2), choix = c("ind","var","varcor"), col.var="black")
#dev.off()
#png("dyn_pca_graph_ind_dim13.png")
plot(pca, axes = c(1,3), choix = c("ind","var","varcor"), col.var="black")
#dev.off()
#png("dyn_pca_graph_ind_dim15.png")
plot(pca, axes = c(1,4), choix = c("ind","var","varcor"), col.var="black")
#dev.off()
#png("dyn_pca_graph_ind_dim15.png")
plot(pca, axes = c(1,5), choix = c("ind","var","varcor"), col.var="black")
#dev.off()

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
#ggsave("dyn_clusters_specific_coord.png", g)


## Contributions
## the more extreme will have the higher contribution
pca$ind$contrib
#dev.off()
#png("dyn_pca_graph_ind_dim12_contr5.png")
plot(pca, axes = c(1,2), select="contrib 5") # plot the 5 individuals with the highest contribution
#dev.off()
#png("dyn_pca_graph_ind_dim13_contr5.png")
plot(pca, axes = c(1,3), select="contrib 5") # plot the 5 individuals with the highest contribution
#dev.off()
#png("dyn_pca_graph_ind_dim14_contr5.png")
plot(pca, axes = c(1,4), select="contrib 5") # plot the 5 individuals with the highest contribution
#dev.off()
#png("dyn_pca_graph_ind_dim15_contr5.png")
plot(pca, axes = c(1,5), select="contrib 5") # plot the 5 individuals with the highest contribution
#dev.off()

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
#png("dyn_pca_graph_ind_dim12_cos0.8.png")
plot(pca, axes = c(1,2), select="cos2 0.8")  # plot the individuals with cos2 greater than 0.8
#dev.off()
#png("dyn_pca_graph_ind_dim13_cos0.8.png")
plot(pca, axes = c(1,3), select="cos2 0.8")  # plot the individuals with cos2 greater than 0.8
#dev.off()
#png("dyn_pca_graph_ind_dim14_cos0.8.png")
plot(pca, axes = c(1,4), select="cos2 0.8")  # plot the individuals with cos2 greater than 0.8
#dev.off()

#png("dyn_pca_graph_ind_dim12_cos5.png")
plot(pca,select="cos2 5")    # plot the 5 individuals with the highest cos2 
#dev.off()

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

length(which(pca$ind$cos2>0.8))/111

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


## Eigenvalues
pca$eig
# PC1 has a 41% of total variance
# PC2 has a 22% of total variance
# PC3 has a 11% of total variance
# PC4 has a 10% of total variance
# PC5 has a 6% of total variance
# The 5 together explain 90% of the total variance

