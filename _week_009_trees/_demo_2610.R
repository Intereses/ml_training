# This removes anything from a previous session
rm(list=ls())

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_2610_meeting")
getwd()

# Libraries
library(tree)
library(MASS)
library(Metrics)
library(caret)
set.seed(1)

# Lets look at Hitters data
data("Boston")
str(Boston)

# Lets create our training and test data
# Median house value is the target
x=Boston
x$medv = NULL
y=Boston$medv

# Lets split our data into two folds and predict
set.seed(1)
ind = createFolds(y, 2, list=F)
X_train = x[ind==1, ]
X_test  = x[ind!=1, ]
y_train = y[ind==1 ]
y_test  = y[ind!=1 ]

# Now lets model 
tree.boston = tree(y_train ~ . , X_train)
summary (tree.boston )
plot(tree.boston)
text(tree.boston ,pretty =0)

?tree
cv.boston =cv.tree(tree.boston )
plot(cv.boston$size ,cv.boston$dev ,type="b")


# Now we prune the tree
prune.boston =prune.tree(tree.boston ,best =7.5)
plot(prune.boston )
text(prune.boston ,pretty =0)

# Let us predict on the tree
ypred.tree = predict(prune.boston , newdata = X_test)
plot(y_test, ypred.tree, col="blue", pch=19)
abline (0,1)
rmse(y_test, ypred.tree)
# 5.059668

# Lets compare to a linear model type approach
lm.mod = lm(y_train~., data=data.frame(X_train))
ypred.lm = predict(lm.mod, data.frame(X_test))
points(ypred.lm, col="red", pch=19)
rmse(y_test, ypred.lm)
# 4.679406

# Lets check if we combine the two
# These two models work a lot differently, so a combination of the two can often do better than one on its own. 
rmse(y_test, (ypred.lm+ypred.tree)/2)
# [1] 4.22






