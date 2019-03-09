# This removes anything from a previous session
rm(list=ls())

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_0911_meeting")
getwd()

# Libraries
library(tree)
library(MASS)
library(Metrics)
library(caret)
library (randomForest)
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
prune.boston =prune.tree(tree.boston ,best =7)
plot(prune.boston )
text(prune.boston ,pretty =0)

# Let us predict on the tree
ypred.tree = predict(tree.boston , newdata = X_test)
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


# Random Forest 
?randomForest
set.seed (1)
rf.mod = randomForest(y_train~.,data=X_train, mtry=6, importance =TRUE) # ranger

?randomForest
#rf.mod = randomForest(as.factor(loan_status)~.,data=X_train, mtry=6, importance =TRUE) # ranger


ypred.rf = predict(rf.mod ,newdata = X_test)
rmse(y_test, ypred.rf)
# [1] 3.907974

# Now lets look at the importance
importance(rf.mod)

# Lets sort it
imp = importance(rf.mod)
imp[order(imp[,1], decreasing = T),]

# Now let's tune to see if we can do better
# First we tune over a different number of variables per tree
set.seed(7)
mtry = 2:13
error = rep(0, length(2:13))

for(m in 1:length(mtry)){
  rf.mod = randomForest(y_train~.,data=X_train, mtry=mtry[m], ntree = 500)
  ypred.rf = predict(rf.mod ,newdata = X_test)
  error[m] = rmse(y_test, ypred.rf)
  plot(mtry, error, pch = 19, col="blue", ylim = c(3.5, 4.5), main = "Random Forest : Error for Number of Random Features per Tree")
}

# Now we keep number of variables constant and try changing the number of trees
set.seed(100)
m = 5
trees = seq(100, 3000, 100)
error = rep(0, length(trees))

for(t in 1:length(error)){
  rf.mod = randomForest(y_train~.,data=X_train, mtry=m, ntree = trees[t])
  ypred.rf = predict(rf.mod ,newdata = X_test)
  error[t] = rmse(y_test, ypred.rf)
  plot(trees, error, pch = 19, col="blue", ylim = c(3.5, 4.5), main = "Random Forest : Error for Different Number of Trees")
}
rmse(y_test, ypred.rf)
# [1] 3.82486

# Finally, lets check blending the randomforest and linear model. 
rmse(y_test, (ypred.rf+ypred.lm)/2)
# [1] 3.951797

# Lets try a weighted blend
rmse(y_test, ((ypred.rf*0.8)+(ypred.lm*0.2)))
# [1] 3.793317
# .... A little better, but hard to tell... would be better to run with different seeds to verify. 


