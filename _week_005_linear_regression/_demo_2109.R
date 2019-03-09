# Today we look at Linear Regression in both R and TI.
# Please use the below book as a reference - its an  excellent resource for getting started in many ML tasks.
#    https://ibm.box.com/s/9qa5l146in39u2lmfb1ln29lfif8yr6v


# This removes anything from a previous session
rm(list=ls())

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
# setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_2109_meeting")
setwd("/Users/dhanley2/Documents/ml_training_r/_005_meeting")
getwd()

# Libraries
library(ggplot2)
library(GGally)
library(Metrics)
library(caret)

# We are going to use the US cereals data set
cereals = read.csv("data/US_Cereals.csv")
str(cereals)

# Lets looks at these features visually
par(mfrow = c(4, 5))
for(i in 4:16) hist(cereals[,i], main = names(cereals)[i], col = "red")
for(i in c(1:3, 17)) barplot(table(cereals[,i]), main = names(cereals)[i])

# Name is just an ID, lets lake it out
# we also take out the categorial columns but we can try putting them back later
regCereals = cereals[,4:16]
str(regCereals)
# Lets start with Simple Linear Regression
?lm
lm.fit =lm(rating ~ sugars ,data= regCereals )
lm.fit
summary(lm.fit)
## NOTE : Explore the object lm.fit in your right top pane
lm.fit$coefficients

lm.fit$df.residual

# Lets plot the line
par(mfrow = c(1, 1))
plot( regCereals$sugars, regCereals$rating, pch=19, xlab="Sugars", ylab = "Ratings" )
abline(lm.fit, col = "red")
abline(h=50, col="blue")


# Now just to show how predict works, let predict the same points.
preds.lm = predict(lm.fit, regCereals)
points(regCereals$sugars, preds.lm, col="blue", pch=19, cex=1.2)

# Lets add jitter to the plot to see them better - this helps if you have overlapping points
plot( jitter(regCereals$sugars), jitter(regCereals$rating), pch=19, xlab="Sugars", ylab = "Ratings" )
abline(lm.fit, col = "red")
points(jitter(regCereals$sugars), jitter(preds.lm), col="blue")

# what does a non linear model look like - adding non linear terms
lm.fit2 =lm(rating ~ regCereals$sugars + I(sugars^2) , data = regCereals )
preds.lm2 = as.numeric(predict(lm.fit2, regCereals))
plot( regCereals$sugars, regCereals$rating, pch=19, xlab="Sugars", ylab = "Ratings" )
points(regCereals$sugars,preds.lm2 ,col="red", pch=19, cex = 2)


# Now lets look at multiple linear regression
# There are two ways of doing this but they both gove the same result
lmm.fit.1 =lm(rating ~ calories + protein + fat + sodium + fiber + carbo + sugars + potass + vitamins + shelf + weight + cups, data = regCereals )
lmm.fit =lm(rating ~ . ,data = regCereals )
all.equal(lmm.fit.1$coefficients, lmm.fit$coefficients)


# Not lets look at the model and make a perdiction
summary(lmm.fit)
plot(lmm.fit)

# Lets make a prediction using both models and see which performs the best.
preds.lmm = predict(lmm.fit, regCereals)
?rmse
sqrt(mean((regCereals$rating - preds.lm)^2)) # [1] 9.075487
mean(abs(regCereals$rating - preds.lm))

rmse(regCereals$rating, preds.lm) # [1] 9.075487
rmse(regCereals$rating, preds.lm2) # [1] 8.730459
rmse(regCereals$rating, preds.lmm) # [1] 2.775257e-07 = 0.0000002775257

# this is an amazing improvement 
#       A bit too good to be true. 
#             this is most likely because we trained and tested on the same data. 

########################################################################################
########    Test and train partition is very important for testing models     ########## 
########################################################################################

# Now lets try making a test partition and training and predicting again.
# Caret is very good for straified sampling of data
idx = createFolds(regCereals$rating, k = 3, list = FALSE)
head(idx, 10)
head(regCereals$rating, 10)
par(mfrow = c(2, 2))
for(i in 1:3) hist(regCereals$rating[idx==i], main=paste0("Fold: ", i), xlim = c(0,100), ylim=c(0,10))

X_train = regCereals[idx!=1,]              # Predictors for training
X_test  = regCereals[idx==1,]              # Predictors for testing
y_train = regCereals$rating[idx!=1]        # Response for training
y_test  = regCereals$rating[idx==1]        # Response for testing
X_train$rating = NULL                      # Remove reponse from Predictors data set
X_test $rating = NULL

lm.fit  =lm(y_train ~ sugars , data= X_train )
lmm.fit =lm(y_train ~ .      , data= X_train )

preds.lm  = predict(lm.fit, X_test)
preds.lmm = predict(lmm.fit, X_test)

rmse(y_test, preds.lm) # [1] 8.495122
rmse(y_test, preds.lmm) # [1] 3.355702e-07 = 0.000000335570

# I'm amazed that the error is so low. I can only conclude one of two reaons,
#    1) I have a bug in the script which is allowing lm overfit
#    2) The rating was derived originally from a linear function. 

# You can read up more on adding interaction terms or polynomials in linear terms on page 115 forward of the book. 
#    https://ibm.box.com/s/9qa5l146in39u2lmfb1ln29lfif8yr6v

# Let say we wanted to bring back our categorical data. 
# How can we represent categorical data in a linear model. One common way is to "One hot encode".

?stats::model.matrix
str(cereals)
par(mfrow = c(1, 1))
plot(table(cereals$mfr))


mfr_ohe = model.matrix(~ cereals$mfr + 0, sparse = T)
# lets looks at the OHE table and compare it to the categorical values
dim(mfr_ohe)
head(cereals$mfr)
# Now we have represented a categorical in numerical format


regCereals = cbind(regCereals, mfr_ohe)
str(regCereals)
lmm1.fit =lm(rating ~ . ,data = regCereals )
preds1.lmm = predict(lmm1.fit, regCereals)
rmse(regCereals$rating, preds1.lmm)  # [1] 2.661632e-07    ... before was [1] 2.775257e-07

