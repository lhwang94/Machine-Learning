##################################################################################################
# Clear the workspace 
##################################################################################################

rm(list=ls())

# Setting the working directory
# wd=paste('/Users/luhaowang/Download/',sep="")

wd=paste('/Users/luhaowang/Download/',sep="")

setwd(wd)

# Loading the library needed
library(ISLR)
library(boot)


##################################################################################################
# Codes on the Validation Set Approach
##################################################################################################

##################################################################################################
# Set the seed
##################################################################################################
attach(Auto)
set.seed(1)

rownames(Auto)=1:nrow(Auto)

# Choosing a subset of the data as training
train=sample(392,196)
#test=seq(1:392)[-train]

# Estimating the parameters on the training dataset for
lm.fit=lm(mpg ~ horsepower , data=Auto, subset=train)

# Computing the mean squared error on the test dataset for a linear model
mean((mpg-predict(lm.fit,Auto))[-train]^2)

# Computing the mean squared error on the test dataset for a model with squared terms
lm.fit2=lm(mpg ~ poly(horsepower ,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# Computing the mean squared error on the test dataset for a model with cubed terms
lm.fit3=lm(mpg ~ poly(horsepower ,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

##################################################################################################
# Set a different seed and repeat the exercise
##################################################################################################
set.seed (2)
train=sample(392,196)
lm.fit=lm(mpg ~ horsepower ,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2=lm(mpg ~ poly(horsepower ,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg ~ poly(horsepower ,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


##################################################################################################
# Codes for "Leave-One-Out Cross-Validation"
##################################################################################################
# Show that the function glm gives the same output as lm, but is more general
glm.fit=glm(mpg ~ horsepower , data=Auto)
coef(glm.fit)
lm.fit=lm(mpg ~ horsepower , data=Auto)
coef(lm.fit)

# Conduct leave out of cross-validation for the linear model
glm.fit=glm(mpg ~ horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta


# Conduct leave out of cross-validation for polynomial model up to 
# fifth order
cv.error=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1] 
  print(i)
}
# plot and report the cross-validation error
cv.error
plot(cv.error,type="l")


##################################################################################################
# Codes for "k-fold Cross-Validation"
##################################################################################################
# Do 10-fold cross-validation for polynomial model up to tenth order
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
cv.error.10
plot(cv.error.10, type="l")

##################################################################################################
# Codes for "Bootstrap"
##################################################################################################
##################################################################################################
# Part 1. Simple Example
##################################################################################################
# Create a function that takes the data and computes the optimal alpha
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
  }

# Deploy the function on the full dataset
alpha.fn(Portfolio,1:100)

# Deploy the function on a bootstrap sample of the data
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

# Use the boot function to estimate the alpha 1,000 times
boot(Portfolio, alpha.fn, R=1000)


##################################################################################################
# Part 2. Estimating the accuracy of a linear regression model
##################################################################################################
# Defining a function that estimates linear regression
boot.fn=function(data,index){
  return(coef(lm(mpg ~ horsepower ,data=data,subset=index)))
}

# Use the function to estimate the results
boot.fn(Auto ,1:392)

# Use the function to estimate the results on two bootstrap samples
set.seed (1)
boot.fn(Auto, sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))

# Compute bootstrap estimates
boot(Auto, boot.fn, 1000)

# Compare the standard error estimates to the one obtained in closed form
# under the various assumptions
summary(lm(mpg ~ horsepower ,data=Auto))$coef


# Re-do the exercise for a second-order polynomial linear regression
boot.fn=function(data,index){
  coefficients(lm(mpg ~ horsepower+I(horsepower^2), 
                  data=data, subset=index))
}

set.seed (1)
boot(Auto ,boot.fn ,1000)
summary(lm(mpg ~ horsepower+I(horsepower^2),data=Auto))$coef












