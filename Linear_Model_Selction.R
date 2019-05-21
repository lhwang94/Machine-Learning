# Clear the workspace
rm(list=ls())

# Loading the required packages
library(ISLR)
library(leaps)

# Loading the credit data
data=Credit

####################################################################################
# PART 1. Best Subset Selection using Mallow's CP, BIC,
# and Adjusted R-square
####################################################################################


# Running all the regression combinations
regfit.full=regsubsets(Balance ~. ,data,nvmax=12)

# Saving the summary of the subset regressions
reg.summary=summary(regfit.full)

# Looking at all the statistics reported by R
names(reg.summary)

# Reporting the R-square for the various models
reg.summary$rsq

# Plotting the subset regression results for the various metrics
quartz()
par(mfrow=c(2,2))

# Results for the residual-sum-of-saquares
plot(reg.summary$rss, xlab="Number of Variables ", 
     ylab="RSS", type="l")

# Results for the adjusted R-squared
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")

# Identifying which model has the highest adjusted R-square
max_adj_r2=which.max(reg.summary$adjr2)

# Putting a red dot on the model that maximizes the adjusted R-square
points(max_adj_r2,reg.summary$adjr2[max_adj_r2], col="red", cex=2,pch=20)

# Repeating the analysis before for CP
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
min_cp=which.min(reg.summary$cp)
points(min_cp,reg.summary$cp[min_cp],col="red",cex=2,pch=20)

# Repeating the analysis before for BIC
min_BIC=which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(min_BIC,reg.summary$bic[min_BIC],col="red",cex=2,pch=20)

# Plot the models with various R-squares
quartz()
plot(regfit.full,scale="r2")
dev.off()

# Plot the model with different adjusted R-squares
quartz()
plot(regfit.full,scale="adjr2")
dev.off()

# Plot models with different Mallow's CP
quartz()
plot(regfit.full,scale="Cp")
dev.off()

# Plot models with different BIC
quartz()
plot(regfit.full,scale="bic")
dev.off()

# Report the coefficients for the best model with 6 regressors
coef(regfit.full,6)


# Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(Balance~.,data=data,nvmax=12,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Balance~.,data=data,nvmax=12,method="backward")
summary(regfit.bwd)
coef(regfit.full,3)
coef(regfit.fwd,3)
coef(regfit.bwd,3)


####################################################################################
# PART 2. Best Subset Selection Using the validation-set approach 
# and Cross-validation
####################################################################################

# PART A: Validation-set Approach
set.seed(1)

# Randomly assign certain observations to the training 
# and the test sample
train=sample(c(TRUE,FALSE), nrow(data),rep=TRUE)
test=(!train)

# Obtaining the best models for training data
regfit.best=regsubsets(Balance~.,data=data[train,],nvmax=12)

# Identifying the test data
test.mat=model.matrix(Balance~.,data=data[test,])

# Constructing validation errors for the best models of 
# each size
val.errors=rep(NA,12)
for(i in 1:12){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((data$Balance[test]-pred)^2)
}

# Printing validation errors
val.errors

# Identifying the minimum validation errors and reporting 
# the coefficients
min_val_errors=which.min(val.errors)
coef(regfit.best,min_val_errors)

# Obtaining the best model with as many regressors 
# as determined by the validation set approach
regfit.best=regsubsets(Balance~.,data=data,nvmax=12)
coef(regfit.best,min_val_errors)

# PART B: Cross-Validation Approach

# Constructing the function to automatically
predict.regsubsets=function(object,newdata,id){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Performing ten-fold cross-validation approach
k=10
set.seed(1)

# Split the sample if five groups at random
folds=sample(1:k,nrow(data),replace=TRUE)

# Prepare the matrix to store the results
cv.errors=matrix(NA,k,12, dimnames=list(NULL, paste(1:12)))

# Do the analyis across cross-validation blocks
for(j in 1:k){
  # Compute the best model of each dimension on the training data
  best.fit=regsubsets(Balance~.,data=data[folds!=j,],nvmax=12)
  # Test the best model of each size on test data
  for(i in 1:12){
    pred=predict.regsubsets(best.fit,data[folds==j,],id=i)
    # Compute mean-squared errors
    cv.errors[j,i]=mean( (data$Balance[folds==j]-pred)^2)
  }
}

# Compute the average errors across cross-validation rounds
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
best_model_cv=which.min(mean.cv.errors)

# Plot the performance for the models of various dimensions
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

# Plot the final model and assoicated coefficients
reg.best=regsubsets(Balance~.,data=data, nvmax=12)
coef(reg.best,best_model_cv)





