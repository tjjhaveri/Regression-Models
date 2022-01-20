library(leaps)
library(readxl)
library(ggplot2)
mydata <- read_excel("Snails.xlsx")
attach(mydata)
View(mydata)
summary(mydata)

set.seed(15)

#Perform best fit
regfit.full = regsubsets(Rings~.,mydata,nvmax = 10)

#Generate summary of the fit
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq

#Plot the RSS vs no. of variables
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of variables",ylab = "RSS", type = "l",main="Best fit RSS vs No of Variables")

#Plot the Adjusted RSS vs no. of variables
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted Rsq", type = "l")

#Get the index of the minimum value
which.min(reg.summary$rss)
#Mark this in red
points(9,reg.summary$rss[9],col = "red",cex =2, pch = 20)

#Display the selected variables for the best model 

plot(regfit.full,scale = "r2",main = "R2 vs Variables")

#Print the 9 coefficient estimates
coef(regfit.full,9)

###MODEL 1

##Forward and Backward selection with cross validation 
set.seed(15)
nvmax<-8
cvError <- NULL
# Loop through each features
for(i in 1:nvmax){
  # Set no of folds
  noFolds=10
  # Create the rows which fall into different folds from 1..noFolds
  folds = sample(1:noFolds, nrow(mydata), replace=TRUE) 
  cv<-0
  # Loop through the folds
  for(j in 1:noFolds){
    # The training is all rows for which the row is != j (k-1 folds -> training)
    train <- mydata[folds!=j,]
    # The rows which have j as the index become the test set
    test <- mydata[folds==j,]
    # Create a forward fitting model for this
    fitFwd=regsubsets(Rings~.^2,data=train,nvmax=8,method=c("forward","backward"))
    # Select the number of features and get the feature coefficients
    coefi=coef(fitFwd,id=i)
    #Get the value of the test data
    test.mat=model.matrix(Rings~.^2,data=test)
    # Multiply the test data with the fitted coefficients to get the predicted value
    # pred = b0 + b1x1+b2x2...
    pred=test.mat[,names(coefi)]%*%coefi
    # Compute mean squared error
    rss=mean((test$Rings - pred)^2)
    # Add all the Cross Validation errors
    cv=cv+rss
  }
  # Compute the average of MSE for K folds for number of features 'i'
  cvError[i]=cv/noFolds
}
rss
pred
a <- seq(1,8)
d <- as.data.frame(t(rbind(a,cvError)))
names(d) <- c("Features","CVError")
#Plot the CV Error vs No of Variables
ggplot(d,aes(x=Features,y=CVError),color="blue") + geom_point() + geom_line(color="blue") +
  xlab("No of features") + ylab("Cross Validation Error") +
  ggtitle("Subset Selection - Cross Valdation Error vs No of Variables")

#Finding the index of the minimum value
b=which.min(cvError)
print(b)
#Print the 8 coefficients of these features
coefi = coef(fitFwd, id = b)
coefi

###MODEL 2: Ridge Regression

install.packages("glmnet")
library(glmnet)
set.seed(15)
# Set X and y as matrices
ncol(mydata)
x=model.matrix(Rings~.^2,mydata)[,-1]
y=mydata$Rings

#Splitting the data
trainRidge = sample(1:nrow(x),nrow(x)/2)
testRidge = (-trainRidge)
y.test=y[testRidge]

# Fit a Ridge model
grid = 10^seq(10,-2,length=100)
fitRidge <-glmnet(x,y,alpha=0,lambda = grid, standardize = TRUE)

# Compute the cross validation error
cvRidge=cv.glmnet(x[trainRidge,],y[trainRidge],alpha=0)

#Plot the cross validation error
par(mfrow = c(1,1))
plot(cvRidge, main="Ridge regression Cross Validation Error (10 fold)")

#Finding the best lambda
bestlam = cvRidge$lambda.min
bestlam

#Finding the test MSE
# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  MSE = SSE/nrow(df)
  
  # Model performance metrics
  data.frame(
    MSE = MSE,
    Rsquare = R_square
  )
  
}
ridge.pred = predict(fitRidge, s= bestlam,newx = x[testRidge,])
TestMSE = mean((ridge.pred-y.test)^2)
TestMSE
eval_results(y.test,ridge.pred,x[testRidge,])

#Predicting the coefficient estimates
predict(fitRidge, type = "coefficients",s= bestlam)[1:10,]


###Model 3: Lasso Regularization
library(glmnet)
set.seed(15)

# Set X and y as matrices
x=model.matrix(Rings~.^2,mydata)[,-1]
y=mydata$Rings

# Fit the lasso model
fitLasso <- glmnet(x,y,alpha = 1,standardize = TRUE)

# Plot the coefficient shrinkage as a function of log(lambda)
par(mfrow=c(1,1))
plot(fitLasso,xvar="lambda",label=TRUE,main="Lasso regularization - Coefficient shrinkage vs log lambda")

# Compute the cross validation error (10 fold)
cvLasso=cv.glmnet(x,y,alpha=1)

# Plot the cross validation error
plot(cvLasso)

#Finding the best lambda
bestlambda = cvLasso$lambda.min
bestlambda

#Finding the test MSE and Rsquared
lasso.pred = predict(fitLasso, s= bestlambda,newx = x[testRidge,])
TestMSE2 = mean((lasso.pred-y.test)^2)
TestMSE2
# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  MSE = SSE/nrow(df)
  
  # Model performance metrics
  data.frame(
    MSE = MSE,
    Rsquare = R_square
  )
  
}
eval_results(y.test,lasso.pred,x[testRidge,])

#Predicting the coefficient estimates
predict(fitLasso, type = "coefficients",s= bestlambda)[1:10,]
plot(cvLasso$lambda,rsq)

