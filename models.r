# libraries
library (glmnet) # Ridge Regression and Lasso models


# Linear Regression - MJ
mod.lr= lm(cardio ~ ., data= data.set) 

# LDA - Nuno
lda.mod= lda(cardio ~ ., data= data.set)


# QDA - MJ
qda.mod= qda(cardio ~ ., data= data.set)


# 6.6 Lab 2: Ridge Regression and the Lasso
# Generic for both models:
# glmnet()
#   *  x matrix and y vector
#   * no Na
#   * only take numerical,quantitative inputs. -> if categorical, make use of dummy variables
#   * if alpha=0, fits ridge regression. If alpha=1, fits lasso model
#   * by default, the function standardizes the variables so that they are on the same scale (standardize=TRUE)

# use cross-validation to choose the tuning parameter ??.
# built-in cross-validation function, cv.glmnet() (default 10 fold cv)
cv.out = cv.glmnet (x[train,], y[train])
best.lambda = cv.out$lambda.min
out = glmnet(x, y) #, alpha=... )
pred = predict(out, s=best.lambda, newx=x[test,] )

#test MSE
mean((pred -y[test])^2)


# Ridge Regression - Nuno
#   * none of the coefficients will be zero since ridge regression does not perform variable selection
ridge.mod = glmnet(x, y, alpha=0)


# Lasso - MJ
#   * some coefficients might be zero
lasso.mod = glmnet(x, y, alpha=1)



# 10.5 Lab 2: Clustering

# K-Means - Nuno
#km.mod =kmeans(x,2, nstart=20)

# Hierarchical classification - MJ
dd = dist(X) # euclidean distance
hh = hclust(dd,method="average")





