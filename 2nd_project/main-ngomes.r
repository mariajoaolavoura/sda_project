# header ------------------------------------------------------------------
###
# Statistical and Data Analysis
# Project 2
# Nuno R. C. Gomes & Maria João Lavoura
###



# packages ----------------------------------------------------------------
require(cowplot)
require(FactoMineR)
require(GGally)
require(latex2exp)
require(MASS)
require(tidyverse)

# theme_set(theme_cowplot())



# data set ----------------------------------------------------------------
data.orig= read.csv("./data/webbink-gc.csv")
dat.raw= as_tibble(data.orig)

summary(dat.raw)



# variables ---------------------------------------------------------------

# NAME      DESCRIPTION                                           TYPE
#
# name      Common name                                           character
# gal.long  Galactic longitude (degrees)                          numeric
# gal.lat   Galactic latitude (degrees)                           numeric
# r.sol     Distance from the Sun (kpc)                           numeric
# r.gc      Distance from the Galactic Centre (kpc)               numeric
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

# change names of variables
dat.raw= dat.raw %>%
  rename(
    name= Name,
    gal.long= Gal.long,
    gal.lat= Gal.lat,
    r.sol= R.sol.kpc,
    r.gc= R.GC.kpc,
    metal= m.H.metal,
    mv= Mv,
    r.core= r.core.pc,
    r.tidal= r.tidal.pc,
    log.t= log.t.rad,
    log.rho= log.rho.cen,
    s0= s0.km.s,
    v.esc= V.esc.km.s,
    vhb= VHB.magl,
    e.bv= E.B.V.mag,
    bv= B.V.mag,
    ellipt= Ellipt,
    v.t= V.t.mag,
    csb= Cent.surf.bright
  )

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



# univariate analysis -----------------------------------------------------
# overview of globular cluster properties
histos= function(data.set, variables= var.units) {
  par(
    mfrow= n2mfrow(dim(data.set)[2]),
    mai= c(0.4, 0.3, 0.2, 0.2)
  )
  for (j in 1:dim(data.set)[2]) {
    name= variables[j]
    hist(
      data.set[, j][[1]],
      main= '',
      breaks= 'FD',
      ylab= '',
      xlab= ''
    )
    title(xlab= name, line= 2.3)
  }
  par(mfrow= c(1, 1))
}

histos(dat.raw[, 2:20])

# univariate boxplots and normal quantile-quantile plots for the variables
boxes.qqs= function(data.set, variables= var.units) {
  dimcol= dim(data.set)[2]
  n1= n2mfrow(dimcol)[1]
  n2= n2mfrow(dimcol)[2]
  layout.matrix= matrix(1:(n1*n2), byrow= T, ncol= 4)
  layout(
    mat= layout.matrix,
    heights= 1,
    widths= c(1, 3, 1, 3)
  )
  par(
    oma= c(0, 0.3, 2, 0),
    mai= c(0.4, 0.3, 0.2, 0.2),
    cex= 0.7
  )
  for (j in 1:dimcol) {
    name= variables[j]
    boxplot(
      data.set[, j][[1]],
      pch= 20,
      yaxt= 'n',
      main= ''
    )
    title(xlab= name, line= 1)
    axis(2, las= 2)
    qqnorm(
      data.set[, j][[1]],
      pch= 20,
      xlab= '',
      yaxt= 'n',
      ylab= '',
      main= ''
    )
    axis(2, las= 2)
    qqline(
      data.set[, j][[1]],
      col= 2,
      lwd= 2
    )
  }
  par(
    mfrow= c(1, 1),
    mai= c(1, 1, 1, 1)
  )
}

boxes.qqs(dat.raw[, 2:20])



# data set preparation ----------------------------------------------------
# remove NAs
dat.na= na.omit(dat.raw) # 34 rows removed


# remove outliers
## r.tidal
r.tidal.histo= ggplot(dat.na[, 9]) +
  geom_histogram(
    aes(r.tidal),
    bins= 30,
    col= "white",
    alpha= 0.5
  ) +
  labs(
    x= "Tidal radius (pc)",
    y= "Counts",
    title= TeX("Histogram of $\\textit{r.tidal}$")
  ) +
  theme_bw()

r.tidal.box= ggplot(dat.na[, 9]) +
  geom_boxplot(aes(r.tidal)) +
  labs(
    x= "Tidal radius (pc)",
    title= TeX("Boxplot of $\\textit{r.tidal}$")
  ) +
  theme_bw()

r.tidal.qq= ggplot(
  dat.na[, 9],
  aes(sample= r.tidal)
) +
  stat_qq() +
  stat_qq_line(
    colour= "red",
    size= 1.0,
    alpha= 0.5
  ) +
  labs(
    title= TeX("QQ-norm of $\\textit{r.tidal}$")
  ) +
  theme_bw()

toprow.r.tidal= plot_grid(
  r.tidal.histo,
  r.tidal.box,
  rel_widths= c(3, 2),
  labels= "AUTO"
)
plot_grid(
  toprow.r.tidal,
  r.tidal.qq,
  ncol= 1,
  labels= c("", "C")
)

## outlier with tidal radius > 250 pc ==> to remove
out.row.r.tidal= which.max(dat.na[, 9][[1]]) # 12
dat.na[out.row.r.tidal, ]
out.r.tidal= dat.na[out.row.r.tidal, 9][[1]] # 284.8
r.tidal.mean= mean(dat.na$r.tidal) # 41.50177
r.tidal.std= sd(dat.na$r.tidal) # 35.39065
(out.r.tidal - r.tidal.mean) / r.tidal.std # 6.874646
## outlier deviates almost 7*sigma from the mean ==> to remove!

summary(dat.na[-out.row.r.tidal, 9][[1]])
## min = 6.5, max = 142.6, median = 32.4, mean= 39.33


## conc
conc.histo= ggplot(dat.na[, 10]) +
  geom_histogram(
    aes(conc),
    bins= 30,
    col= "white",
    alpha= 0.5
  ) +
  labs(
    x= "Core concentration parameter",
    y= "Counts",
    title= TeX("Histogram of $\\textit{conc}$")
  ) +
  theme_bw()

conc.box= ggplot(dat.na[, 10]) +
  geom_boxplot(aes(conc)) +
  labs(
    x= "Core concentration parameter",
    title= TeX("Boxplot of $\\textit{core}$")
  ) +
  theme_bw()

conc.qq= ggplot(
  dat.na[, 10],
  aes(sample= conc)
) +
  stat_qq() +
  stat_qq_line(
    colour= "red",
    size= 1.0,
    alpha= 0.5
  ) +
  labs(
    title= TeX("QQ-norm of $\\textit{conc}$")
  ) +
  theme_bw()

toprow.conc= plot_grid(
  conc.histo,
  conc.box,
  rel_widths= c(3, 2),
  labels= "AUTO"
)
plot_grid(
  toprow.conc,
  conc.qq,
  ncol= 1,
  labels= c("", "C")
)

## no outliers detected
## outlier previously detected disappeared on NAs removal

summary(dat.na[-out.row.r.tidal, 10][[1]])
## min = 0.7, max = 2.5, median = 1.5, mean= 1.546



## e.bv
e.bv.histo= ggplot(dat.na[, 16]) +
  geom_histogram(
    aes(e.bv),
    bins= 30,
    col= "white",
    alpha= 0.5
  ) +
  labs(
    x= "B-V colour excess (mag)",
    y= "Counts",
    title= TeX("Histogram of $\\textit{e.bv}$")
  ) +
  theme_bw()

e.bv.box= ggplot(dat.na[, 16]) +
  geom_boxplot(aes(e.bv)) +
  labs(
    x= "B-V colour excess (mag)",
    title= TeX("Boxplot of $\\textit{e.bv}$")
  ) +
  theme_bw()

e.bv.qq= ggplot(
  dat.na[, 16],
  aes(sample= e.bv)
) +
  stat_qq() +
  stat_qq_line(
    colour= "red",
    size= 1.0,
    alpha= 0.5
  ) +
  labs(
    title= TeX("QQ-norm of $\\textit{e.bv}$")
  ) +
  theme_bw()

toprow.e.bv= plot_grid(
  e.bv.histo,
  e.bv.box,
  rel_widths= c(3, 2),
  labels= "AUTO"
)
plot_grid(
  toprow.e.bv,
  e.bv.qq,
  ncol= 1,
  labels= c("", "C")
)

## outlier with extinction > 1.5
out.row.e.bv= which.max(dat.na[, 16][[1]]) # 63
dat.na[out.row.e.bv, ]
out.e.bv= dat.na[out.row.e.bv, 16][[1]] # 2.9
e.bv.mean= mean(dat.na$e.bv) # 0.3300885
e.bv.std= sd(dat.na$e.bv) # 0.4022004
(out.e.bv - e.bv.mean) / e.bv.std # 6.389629
## outlier deviates more than 6*sigma from the mean ==> to remove
## after NAs removal, no other outliers are detected in e.bv
## double check:
dat.raw %>%
  arrange(-e.bv) %>%
  slice_max(e.bv > 1.5)
dat.na %>%
  arrange(-e.bv) %>%
  slice_max(e.bv > 1.5)

summary(dat.na[-out.row.e.bv, 16][[1]])
## min = 0, max = 1.5, median = 0.2, mean = 0.3071

# remove identified outliers
dat.out= dat.na %>%
  slice(-c(out.row.r.tidal, out.row.e.bv))
## two outliers removed


# standardise variables
dat.num= as_tibble(scale(dat.out[, -1]))

# separate variables: location + dynamics
dat.loc= dat.num[,  c(1:4)]
dat.dyn= dat.num[, -c(1:4)]



# save data sets ----------------------------------------------------------
saveRDS(dat.loc, "./data/location.dat") # readRDS to load; eg:
saveRDS(dat.dyn, "./data/dynamics.dat") # a= readRDS("./data/dynamics.dat")
saveRDS(dat.num, "./data/gc.dat")
write.csv(dat.loc, "./data/location.csv")
write.csv(dat.dyn, "./data/dynamics.csv")
dat.loc= readRDS("./data/location.dat")
dat.dyn= readRDS("./data/dynamics.dat")
dat.num= readRDS("./data/gc.dat")


# bivariate relationships -------------------------------------------------
## pair plot without density plots
ggpairs(
  dat.dyn,
  lower= list(
    continuous= wrap(
      "points",
      alpha= 0.3,
      size= 0.1
    ),
    combo= wrap(
      "dot",
      alpha= 0.4,
      size= 0.2
    )
  )
)

## pair plots with density plots
ggpairs(
  dat.dyn,
  upper= list(
    continuous= wrap(
      "density",
      alpha= 0.5
    ),
    combo= "box"
  ),
  lower= list(
    continuous= wrap(
      "points",
      alpha= 0.3,
      size= 0.1
    ),
    combo= wrap(
      "dot",
      alpha= 0.4,
      size= 0.2
    )
  )
)

dat.dyn %>%
  ggcorr(
    geom= "blank",
    label= T,
    hjust= 0.75,
    fontface= "bold",
    label_round= 2
  ) +
  geom_point(
    size= 12,
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

cor(dat.dyn, method= "kendall")
var(dat.dyn)

pairs(
  dat.dyn[, 5:8],
  main= "", labels= names(dat.dyn[5:8]),
  panel= function(x, y) {
    abline(
      lsfit(x, y)$coef,
      lwd= 2,
      col= "deeppink2"
    )
    lines(
      lowess(x, y),
      lwd= 3,
      col= "blue3",
      lty= 2
    )
    points(
      x, y,
      pch= 21,
      bg= c("red", "green3", "blue")
    )
    rug(
      jitter(x, factor= 3),
      side= 1,
      col= "lightcoral",
      ticksize= -0.05
    )
    rug(
      jitter(y, factor= 3),
      side= 2,
      col= "cornflowerblue",
      ticksize= -0.05
    )
    contour(
      kde2d(x, y)$x, kde2d(x, y)$y, kde2d(x, y)$z,
      drawlabels= F,
      add= T,
      col= "darkblue",
      nlevel= 4
    )
  }
)
