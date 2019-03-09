# This removes anything from a previous session
rm(list=ls())

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_1910_meeting")
getwd()

# Libraries
library(ISLR)
library (glmnet)
library(caret)
library(Metrics)

# Lets look at Hitters data
data("Hitters")
str(Hitters)

# We had some missing values, to do regression we need to fill those.
# This would not be my choice, but I think for the example its ok
?na.omit
Hitters =na.omit(Hitters)

# Lets Visualise it - seems like we have skew - a lot...
par(mfrow = c(4,5), mar = c(2,2,2,2))
for(var in colnames(Hitters)) if(class(Hitters[,var])!="factor")   hist(Hitters[,var], main = var)
for(var in colnames(Hitters)) if(class(Hitters[,var])=="factor")   barplot(table(Hitters[,var]), main = var)

###################
# Ridge regression
###################

# Lets create our training and test data
x=model.matrix (Salary~.,Hitters )[,-1]
y=Hitters$Salary
x = scale(x)
colnames(x)

# Let runs Ridge regression with glmnet first.
# Remember, we need ot choose a Lmbda value. 
grid =10^ seq(10,-2, length =100)
grid
ridge.mod =glmnet(x,y,alpha =0, lambda =grid)

# By default glmnet standardizes variables. 

# Lets check the coefficients at a particular lambda
grid[50]
ridge.mod$lambda [50]
coef(ridge.mod)[,50]

ridge.mod$lambda [60]
coef(ridge.mod)[,60]

# Notice with higher lambda's the regularising term, the coefficients go down. 
par(mfrow=c(1,1), mar = c(2,2,2,2))
plot(ridge.mod)

# Lets split our data into two folds and predict
set.seed(1)
ind = createFolds(y, 2, list=F)
X_train = x[ind==1, ]
X_test  = x[ind!=1, ]
y_train = y[ind==1 ]
y_test  = y[ind!=1 ]

# Make a prediction using ridge regression.
ridge.mod =glmnet(X_train,y_train,alpha =0, lambda=grid, thresh =1e-12)
y.ridge.pred=predict (ridge.mod, s=4, newx=X_test)
head(y.ridge.pred)
rmse(y_test, y.ridge.pred)
# [1] 316.5635

# Lets compare against linear regression. 
lm.mod = lm(y_train~., data=data.frame(X_train))
rmse(y_test, predict(lm.mod, data.frame(X_test)))
# [1] 312.5006
mae(y_test, predict(lm.mod, data.frame(X_test)))
# [1] 239.4047

# Now how do we chose our lambda value
set.seed (1)
cv.out =cv.glmnet(X_train, y_train,alpha =0, nfold=3)

plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

log(292)

# Make a prediction using ridge regression.
ridge.mod =glmnet (X_train,y_train,alpha =0, lambda = bestlam)
y.ridge.pred=predict (ridge.mod ,s=4, newx=X_test)
rmse(y_test, y.ridge.pred)
# [1] 400.1381
mae(y_test, y.ridge.pred)
# [1] 241.3449

###################
# Lasso
###################
lasso.mod =glmnet(X_train, y_train,alpha = 1, lambda =grid)
plot(lasso.mod)

# Now lets compare the Lasso to the other model results
set.seed(1)
cv.out =cv.glmnet(X_train, y_train,alpha =1, nfold=3)
plot(cv.out)

# this plot is interesting - it shows us the minimum lambda, but also a more conservative 
# 1SE error lambda, which will not be overfit on the data. 
bestlam =cv.out$lambda.min
log(bestlam)
best1se =cv.out$lambda.1se
log(best1se)

# Now lets check if we predict with each
lasso.pred=predict(lasso.mod ,s=bestlam,newx= X_test)
rmse(y_test, lasso.pred)
# [1] 418.5818
mae(y_test, lasso.pred)
# [1] 243.0988

# One of the big advantages of Lassoo is it pushes certain variable to zero
out=glmnet(x,y,alpha =1, lambda = bestlam)
lasso.coef=predict(out ,type ="coefficients",s=bestlam )
lasso.coef



