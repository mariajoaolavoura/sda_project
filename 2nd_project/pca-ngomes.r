# header ------------------------------------------------------------------
###
# Statistical and Data Analysis
# Project 2
# Nuno R. C. Gomes & Maria João Lavoura
###



# packages ----------------------------------------------------------------
require(FactoMineR)
require(GGally)
require(tidyverse)



# data sets ---------------------------------------------------------------
dat.loc= readRDS("./data/location.dat")
dat.dyn= readRDS("./data/dynamics.dat")
dat.num= readRDS("./data/gc.dat")


# principal component analysis --------------------------------------------
pca.tot= PCA(dat.num)
pca.dyn= PCA(dat.dyn)
pca.loc= PCA(dat.loc)



# eigenvectors ------------------------------------------------------------
## all numeric variables
pca.tot$svd$V
v1= pca.tot$svd$V[, 1]
v2= pca.tot$svd$V[, 2]
v3= pca.tot$svd$V[, 3]
v4= pca.tot$svd$V[, 4]
v5= pca.tot$svd$V[, 5]
sum(v1^2)
sum(v1*v2)

pca.tot$var

## The first PC has stronger weights for the distance from the Sun, the distance from the Galactic centre, the absolute magnitude, the core radius, the logarithm of the central relaxation timescale, the ellipticity, and the central surface brightness, and then negative weights for the core concentration parameter, the logarithm of the central density, the central velocity dispersion, and the central escape velocity.
# So, looking to the picture, we can infer that globular clusters nos. 5, 13, and 6, which have high positive values in the first PC, will be globular clusters with high values of r.sol, r.gc, mv, r.core, log.t, ellipt, and csb, and low values of r.core, log.rho, s0, and v.esc.

## Perhaps it is easier to interpret the results by computing the PCs for only the dynamic variables.

## dynamic variables
pca.dyn$var
## The first PC is dominated by r.core (the core radius), conc (concentration), log.rho (central star density), s0 (central velocity dispersion), v.esc (escape velocity), and csb (central surface brightness). These variables are physical interrelated and, thus, it was expected all of them to have a similar contribution to the PC.
## mv (absolute magnitude) and ellipt (ellipticity) have a weaker and less obvious contribution.

## The second PC has hight coefficients for bv (B-V colour index), e.bv (B-V colour excess), and r.tidal (tidal radius). A combination of dynamical and luminosity variables.

## The third component is dominated by vhb (level of the horizontal branch in the Hertzsprung-Russel diagram), s0 (central velocity dispersion), mv (total luminosity), v.esc (escape velocity), and log.t (central relaxation timescale). This is a combination of dynamical, luminosity, and metallicity variables which are difficult to interpret.

## Similar for PC4 and PC5.

pca.dyn$ind



# eigenvalues -------------------------------------------------------------
## dynamic varialbes
pca.dyn$eig
sum(pca.dyn$eig[1:3, 2])
### The first three components together account for almost 74% of the sample variance in the standardised variables.

barplot(
  pca.dyn$eig[, 1],
  main= "Eigenvalues (dynamic variables)",
  names.arg= 1:nrow(pca.dyn$eig)
)

