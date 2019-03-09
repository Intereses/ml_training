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
library(randomForest)
library(xgboost)
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
rf.mod = randomForest(y_train~.,data=X_train, ntree=1000, mtry=4, importance =TRUE) # ranger
# rf.mod = randomForest(as.factor(loan_status)~.,data=X_train, mtry=6, importance =TRUE) # ranger


ypred.rf = predict(rf.mod ,newdata = X_test)
rmse(y_test, ypred.rf)
# [1] 3.907974

# Finally, lets check blending the randomforest and linear model. 
rmse(y_test, (ypred.rf+ypred.lm)/2)
# [1] 3.951797

# Lets try a weighted blend
rmse(y_test, ((ypred.rf*0.8)+(ypred.lm*0.2)))
# [1] 3.793317
# .... A little better, but hard to tell... would be better to run with different seeds to verify. 


# Gradient boosting
# http://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
# Xgb parameters : https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
?xgb.train

# For xgb we need to set the data in a special format
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)

# Lets train a model and output the training error as we go
xg.mod <- xgboost(data = dtrain, 
                  max.depth = 2,                   # depth of all the trees
                  eta = 0.1,                       # Learning rate of trees
                  nthread = 8,                     # Number of threads or parallel processes
                  nround = 100,                    # number of trees to create 
                  objective = "reg:linear",        # We want to optimise on a regression porblem
                  print.every.n = 10,
                  verbose = 1)

# Xgb also lets us to cross validate
xg.mod <- xgb.cv(data = dtrain, 
                 prediction = T,
                  max.depth = 4,                   # depth of all the trees
                  eta = 0.01,                      # Learning rate of trees
                  nthread = 8,                     # Number of threads or parallel processes
                  nround = 2000,                   # number of trees to create 
                  nfold = 5,                       # Use 5 fold cross validate
                  objective = "reg:linear",        # We want to optimise on a regression porblem
                  print.every.n = 50,              # We only print teh 
                  verbose = 1)

# We can ask xgb to stop it when it gets to the best round
xg.mod <- xgb.cv(data = dtrain, 
                 max.depth = 2,                   # depth of all the trees
                 eta = 0.01,                      # Learning rate of trees
                 nthread = 8,                     # Number of threads or parallel processes
                 nround = 200000,                   # number of trees to create 
                 nfold = 5,                       # Use 5 fold cross validate
                 early.stop.round = 50,           # stop it when it does not improve after 50 trees
                 maximize = F,                    # linked to watchlist; definition of improve is to minimise
                 objective = "reg:linear",        # We want to optimise on a regression porblem
                 print.every.n = 50,              # We only print teh 
                 verbose = 1)

# So now lets train our data using the best number of trees 
xg.mod <- xgboost(data = dtrain, 
                  max.depth = 6,                   # depth of all the trees
                  eta = 0.01,                      # Learning rate of trees
                  nthread = 8,                     # Number of threads or parallel processes
                  nround = 1154,                   # number of trees to create 
                  objective = "reg:linear",        # We want to optimise on a regression porblem
                  print.every.n = 10,
                  verbose = 0)                     # This time dont print out the log.

# Get the feature real names
names <- colnames(X_train)
# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xg.mod)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

# Now lets predict
ypred.xg = predict(xg.mod, as.matrix(X_test))
rmse(y_test, ypred.xg)
# [1] 4.005849


rmse(y_test, ypred.lm)
# 4.679406

rmse(y_test, ypred.rf)
# [1] 3.907974
rmse(y_test, ypred.xg)
# [1] 3.907974

# Lets try a weighted blend
rmse(y_test, ((ypred.xg*0.4)+(ypred.rf*0.4)+(ypred.lm*0.2)))
# [1] 3.793317

