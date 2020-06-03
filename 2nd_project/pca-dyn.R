## libraries
library("FactoMineR")

set.seed(123)

## Read data frame
df = readRDS("./data/dynamics.dat") 

# View(df)
# dim(df)
# str(df)
summary(df)

## PCA
pca = PCA(df)
pca


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
barplot(pca$eig[,1],
        main="Eigenvalues",
        names.arg=1:nrow(pca$eig))





# In general, for each column/dimension/PC see which rows have high values.
# The respective rows will allow us to know what does the current 
# PC explains better and what it opposes

## In regard of the variables
## Interpretation
pca$var

## correlations variables - dimensions/PC
pca$var$cor

## Contributions
# pca$var$contrib == pca$var 
# the variables which have the highest correlation with each PC
# will be those that contribute more


## In regard of the individuals
## Interpretation
pca$ind

## coordinates of the individuals
## which ones are more extreme in each PC
plot(pca, axes = c(1,2), choix = c("ind","var","varcor"), col.var="black")
pca$ind$coord

## Contributions
## the more extreme will have the higher contribution
pca$ind$contrib

## quality of representation - cos2
pca$ind$cos2



# repeated (to remember)
# In general, for each column/dimension/PC see which rows have high values.
# The respective rows will allow us to know what does the current 
# PC explains better and what it opposes

## Interpretation of the principal components
# look into the eigenvectors
# variance explained by PC and its relative importance accessed 
# by eigenvalues (pca$eig)

## Eigenvectors
pca$svd$V 

## Eigenvalues
pca$eig



