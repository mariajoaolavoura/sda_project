# header ------------------------------------------------------------------
###
# Statistical and Data Analysis
# Project 2
# Nuno R. C. Gomes & Maria João Lavoura
###



# packages ----------------------------------------------------------------
require(GGally)
require(latex2exp)
require(psych)
require(tidyverse)
library(FactoMineR)
library(ggpubr)


# functions ---------------------------------------------------------------
plot_loadings = function(l, label){
  names(l) = c("RC1", "RC2")
  ggscatter(l, x = "RC1", y = "RC2", 
            size = 1,
            label = label,
            repel = TRUE,
            title = "Factor Analysis - graph of variables")
  
}


# data sets ---------------------------------------------------------------
dat.loc= readRDS("./data/location.dat")
dat.dyn= readRDS("./data/dynamics.dat")
dat.num= readRDS("./data/gc.dat")



# variables ---------------------------------------------------------------
var.units= c(
  TeX("$\\textbf{gal.long}$ ($\\degree$)"),
  TeX("$\\textbf{gal.lat}$ ($\\degree$)"),
  TeX("$\\textbf{r.sol}$ (kpc)"),
  TeX("$\\textbf{r.gc}$ (kpc)"),
  TeX("$\\textbf{metal}$"),
  TeX("$\\textbf{mv}$ (mag)"),
  TeX("$\\textbf{r.core}$ (pc)"),
  TeX("$\\textbf{r.tidal} (pc)"),
  TeX("$\\textbf{conc}$"),
  TeX("$\\textbf{log.t}$ (yr)"),
  TeX("$\\textbf{log.rho}$ ($M_{sun}/pc^3$)"),
  TeX("$\\textbf{s0}$ (km/s)"),
  TeX("$\\textbf{v.esc}$ (km/s)"),
  TeX("$\\textbf{vhb}$ (mag)"),
  TeX("$\\textbf{e.bv} (mag)"),
  TeX("$\\textbf{bv}$ (mag)"),
  TeX("$\\textbf{ellipt}$"),
  TeX("$\\textbf{v.t} (mag)"),
  TeX("$\\textbf{csb}$ ($mag/arcsec^2$)")
)



# correlations ------------------------------------------------------------
dat.num.corr= cor(dat.num)

# all variables
dat.num %>%
  ggcorr(
    geom= "blank",
    label= T,
    hjust= 0.55
  ) +
  geom_point(
    size= 10,
    aes(
      colour= coefficient > 0,
      alpha= abs(coefficient) > 0.5
    )
  ) +
  scale_alpha_manual(values= c("TRUE"= 0.25, "FALSE"= 0)) +
  guides(
    colour= F,
    alpha= F
  )

# v.t (integrated V magnitude), gal.lat (galactic latitude), and gal.long (galactic longitude) are not correlated with any variable. Therefore, they can be excluded from the factor analysis.

dat= dat.num[, -c(1, 2, 18)]
dat.corr= cor(dat)

# all variables except v.t (18), gal.lat (2), and gal.long(1)
dat %>%
  ggcorr(
    geom= "blank",
    label= T,
    hjust= 0.75
  ) +
  geom_point(
    size= 10,
    aes(
      colour= coefficient > 0,
      alpha= abs(coefficient) > 0.5
    )
  ) +
  scale_alpha_manual(values= c("TRUE"= 0.25, "FALSE"= 0)) +
  guides(
    colour= F,
    alpha= F
  )




# kmo ---------------------------------------------------------------------
# analyse goodness-of-fit test of the model
KMO(dat.corr)
# Measure of Sampling Adequacy (MSA) not very large (MSA = 0.71) ==> medium, but not approaching 0.5, which would be unnaceptable for our analysis.
# The most appropriate variables for the model are:
# 1. r.core (core radius, in pc) -- 0.96
# 2. r.tidal (tidal radius, in pc) -- 0.87
# 3. r.gc (distance from the Galactic centre, in kpc) -- 0.79
# 4. r.sol (distance from the Sun, in kpc) -- 0.78
# 5. conc (core concentration parameter) -- 0.76
# 6. log.rho (logarithm of central density, in M_sun/pc^3) -- 0.76
# 7. v.esc (central escape velocity, in km/s) -- 0.75
# 8. s0 (central velocity dispersion, in km/s) -- 0.75



# factor analysis model extraction ----------------------------------------
pca = PCA(dat)
pca$eig 

# extract principal components (model with five common factors)
dat.pcs= principal(dat.corr, 4, rotate= "varimax")
# communalities with a five common factors model
dat.pcs$communality
# The values obtained are relatively high.
# Our model explains most of the total variability of the variables. The one explained the least is the tidal radius, with about 72.4%.

# rotated components
dat.pcs$loadings
print(dat.pcs$loadings,cutoff = 0.6)
# RC1: opposes central velocity disp4ersion (s0) and central escape velocity (v.esc) to absolute magnitude (mv)
# RC4: opposes the logarithm of central relaxation time scale (log.t) and the core radius (r.core) to the core concentration parameter (conc)
# RC3: high values in distance from the Sun (r.sun), distance from the Galactic Centre (r.gc), and level of the horizontal branch (vhb).
# RC2: high values of B-V colour excess (e.bv) and B-V colour index (B-V)
# RC5: high value of logarithm of metallicity with respect to solar metallicity (metal)
plot_loadings(data.frame(dat.pcs$loadings[,1:2]), names(dat))

# residuals
dat.pcs$residual
dat.pcs


# alternatives for model extraction ---------------------------------------
# principal axis
dat.pa= fa(
  dat.corr,
  nfactors= 2,
  n.obs= 50,
  fm= "pa",
  rotate= "varimax",
  SMC= F)

# min residuals
dat.minres= fa(
  dat.corr,
  nfactors= 2,
  n.obs= 50,
  fm= "minres",
  max.iter= 100,
  rotate= "varimax",
  SMC= F
)

# checking normality graphically
scatterplotMatrix(dat)

# maximum likelihood
dat.ml= fa(
  dat.corr,
  nfactors= 5,
  n.obs= 50,
  fm= "ml",
  rotate= "varimax",
  SMC= F
)
