################################################################################################
# Codes for Ridge regression and the LASSO
################################################################################################

# Clear the workspace
rm(list=ls())

# Best Subset Selection

library(ISLR)
library(leaps)

# Displaying the name of the variables
names(Hitters)

# Computing the dimensions of the dataset
dim(Hitters)

# Computing the number of players for which we do not have a salary
sum(is.na(Hitters$Salary))

# Omit for the data, the entries of players with no salary information
Hitters=na.omit(Hitters)

# Re-computing the dimensions of the dataset
dim(Hitters)

# Making sure that all players in the data have salaries
sum(is.na(Hitters))

# Creating the matrix of regresors and the vector of dependent variables
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary


# Ridge Regression

# Loading the package that estimates Ridge Regressions
library(glmnet)

# Determining the grid, we choose it to range between 10^10 and 10^(-2)
grid=10^seq(10,-2,length=100)

# Alpha=0 is for RIDGE, Alpha=1 is for LASSO
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
plot (ridge.mod, xvar="lambda")

# The procedure returns a vector of 20 coefficients for each value of the 
# penalty grid
dim(coef(ridge.mod))

# Reporting the value of lambda for a value in the middle of the grid
ridge.mod$lambda[50]

# Reporting the ridge regression coefficient estimates for a value in the middle of the grid
coef(ridge.mod)[,50]

# Computing the sum of the coefficients 
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Reporting the value of lambda for lower shrinkage value
ridge.mod$lambda[60]

# Reporting the ridge regression coefficient estimates for a lower shrinkage value
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]

# Setting the seed 
set.seed(1)

# Separating the data into training and test datasets
train=sample(1:nrow(x), nrow(x)/2)
test=seq(1:nrow(x))[-train]
y.test=y[test]

# Training the ridge regression model 
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)

# predicting the ridge regression model on the test data
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])

# Compute the average mean-squared error on test observations
mean((ridge.pred-y.test)^2)

# Compute the performance of a model that just uses the average 
# of the training sample
mean((y.test-mean(y[train]))^2)

# You can get the same result if you use an extremely large 
# value for the shrinkage parameter
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Now let's compute the mean-squared error of the traditional OLS
# coefficient
ridge.pred=predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Compare the coefficient of standard OLS with the coefficients
# of Ridge with zero shrinkage
lm(y~x, subset=train)
predict(ridge.mod,s=0,type="coefficients")[1:20,]


# Setting the seed
set.seed(1)

# Default is 10-fold cross-validation
cv.out=cv.glmnet(x[train,],y[train],alpha=0)

# Plotting the resuls 
plot(cv.out)

# Computing the optimal lambda and displaying in
bestlam=cv.out$lambda.min
bestlam

# Computing the ridge regression using the optimal lambda
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])

# test the out-of-sample performance of the cross-validated Ridge Regression
mean((ridge.pred-y.test)^2)

# Re-estimate the ridge regression on the full dataset 
# and report the shrunk coefficient estimates.
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# Compare the coefficients on OLS estimated on the full data
lm(y~x)


# Doing the analysis using LASSO rather than Ridge regression
# Notice that the alpha is 1 and not zero
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)

# plotting lasso as a functio of the shrinkage parameter
plot(lasso.mod)
plot(lasso.mod, xvar="lambda")

# Compute cross-validation to determine the optimal lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)


# Reporting the optimal lambda 
bestlam=cv.out$lambda.min

# Predicing the test sample observations using the optimal lambda
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

# Computing the mean-squared error out of sample 
mean((lasso.pred-y.test)^2)

# Computing the final model on the full sample 
out=glmnet(x,y,alpha=1,lambda=grid)

# Reporting the lasso coefficients 
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]

lasso.coef
lasso.coef[lasso.coef!=0]


