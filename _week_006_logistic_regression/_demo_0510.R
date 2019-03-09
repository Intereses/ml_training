# Today we look at Linear Regression in both R and TI.
# Please use the below book as a reference - its an  excellent resource for getting started in many ML tasks.
#    https://ibm.box.com/s/9qa5l146in39u2lmfb1ln29lfif8yr6v

# This removes anything from a previous session
rm(list=ls())

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_2109_meeting")
getwd()

# Libraries
library(Metrics)
library(caret)
library(ISLR)

# Before we start, what is a logistic function ??
e = 2.71828   # natural log

log(0.5)
e^-0.6931472

e^(log(0.5)) 

log(1)
log(10)
log(100000000000000000)
log(1000000000000000000000000000000000000000000000000000000000)

# Let look at how it behaves
sequence = 0:100
plot(sequence, log(sequence), type="l", col="red") 
log(0)

# Function for logistics regression 
myfunc = function(x) (e^x/(1+e^x))
sequence = (-100:100)/10
sequence
plot(sequence, myfunc(sequence), type="l", col="red") 

# shift the function to a different intercept
myfunc = function(x) (e^(x-3)/(1+e^(x-3)))
plot(sequence, myfunc(sequence), type="l", col="red") 

# flip the function
myfunc = function(x) (e^(-3*x)/(1+e^(-3*x)))
plot(sequence, myfunc(sequence), type="l", col="red") 


# We are going to use the US cereals data set
data(Caravan)
dim(Caravan)
str(Caravan)

# Lets look at the target variable
table(Caravan$Purchase)
plot(table(Caravan$Purchase))

# As before, lets split the data into predictor and response
y = Caravan$Purchase
X = Caravan[,-86]

# Lets look at the predictors
par(mfrow = c(4, 5))
for(i in 1:20) hist(X[,i], main = colnames(X)[i])

# They are all using different scaling - lets align this
X = data.frame(scale(X))
for(i in 1:20) hist(X[,i], main = colnames(X)[i])

# Other transormations possible
Xlog = data.frame(apply(Caravan[,-86], 2, function(x) log(x+0.875966)))
for(i in 1:20) hist(Xlog[,i], main = colnames(Xlog)[i])

# Split into train and test using straified split based on target
set.seed(1)
idx = createFolds(y, k = 4, list = FALSE)
table(idx, y)   # this show a good stratified split into 3 folds
plot(table(idx, y) )

ytrain = y[idx!=1]
ytest =  y[idx==1]
Xtrain = X[idx!=1,]
Xtest =  X[idx==1,]

# Now we fit our logistic regression and predict it
glm.fit=glm(ytrain ~ ., data=Xtrain, family = binomial)
summary(glm.fit)
glm.fit$coefficients

# Lets predict who bough caravans 
glm.probs = predict(glm.fit, Xtest,  type="response")

# Lets check the first few predicted and actual results
head(glm.probs, 10)
tail(ytest, 10)

# Lets check the results
par(mfrow = c(1, 1))
plot(ytest, glm.probs)
abline(h=0.5, col="red")

# Lets try and classify them by making a cut in the probabilities
glm.pred = ifelse(glm.probs>.5, "Predicted Yes", "Predicted No")
table(ytest, glm.pred)

# Not to good, lets try again
glm.pred = ifelse(glm.probs>.05, "Predicted Yes", "Predicted No")
table(ytest, glm.pred)
abline(h=0.1, col="red")



# Lets try and classify them by making a cut in the probabilities
Xlogtrain = Xlog[idx!=1,]
Xlogtest =  Xlog[idx==1,]
glm.fitlog=glm(ytrain ~ .,data=Xlogtrain, family = binomial)
glm.probslog = predict(glm.fitlog, Xlogtest,  type="response")
# Lets check the results
par(mfrow = c(1, 1))
plot(ytest, glm.probslog)

glm.predlog = ifelse(glm.probslog>.5, "Predicted Yes", "Predicted No")
table(ytest, glm.predlog)
abline(h=0.1, col="red")

# Not to good, lets try again
glm.predlog = ifelse(glm.probslog>.1, "Predicted Yes", "Predicted No")
table(ytest, glm.predlog)
abline(h=0.1, col="red")

