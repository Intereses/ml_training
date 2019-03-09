# https://github.com/Rdatatable/data.table/wiki

## install libraries
# install.packages("dplyr")
library(data.table)
library(fasttime)
library(RcppRoll)
library(datasets)
library(xgboost) 


# This removes anything from a previous session
rm(list=ls())
gc()

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
setwd("/Users/dhanley2/Documents/ml_training_r/_004_meeting")
getwd()
#install.packages("nycflights13", repos='http://cran.us.r-project.org')

list.files('../_002_meeting')
dt    = fread('../_002_meeting/train.csv')
mydt1 = fread('../_002_meeting/train.csv', nrows = 1000)
mydt2 = fread('head -1000 ../_002_meeting/train.csv')
mydt3 = fread('head -1000 ../_002_meeting/train.csv', select = paste0('VAR_', 1204:1245))
#mydt4 = fread("sed  –n 4001,5000p ../_002_meeting/train.csv", select = paste0('VAR_', 1204:1245))

dim(dt)
dim(mydt1)
dim(mydt2)
dim(mydt3)

# Quick Preview
dt

####################################
#### Count encode string columns ###
####################################
dttxt = dt[,(sapply(dt, class) == "character"), with = FALSE]
str(dttxt)

# The magic .N
dttxt[ , .N, by = VAR_0237]
dttxt[ , .N, by = .(VAR_0237, VAR_0274)]
dttxt[ , .N, by = .(VAR_0237, VAR_0274)][order(N)]
tail(dttxt[ , .N, by = .(VAR_0237, VAR_0274)][order(N)], 50)
View(dttxt[ , .N, by = .(VAR_0237, VAR_0274)])

# Count encode single column
dttxt[ , Count_VAR_0237 := .N , by = VAR_0237]
dttxt[, .(VAR_0237, Count_VAR_0237)]

# Count encode interaction of columns
dttxt[ , Count_VAR_0001_0005 := .N , by = .(VAR_0001, VAR_0005) ]
dttxt[, .(Count_VAR_0001_0005, VAR_0001, VAR_0005)]

for (col in colnames(dttxt)) {
  dttxt[, paste0("Count_", col) := .N, by = col] 
} 

str(dttxt)

#####################################
#### Work with dates, leads, lags ###
#####################################
dtdt = dt[,c("VAR_0075", 'VAR_0002'), with = FALSE]

dtdt[, date:= fastPOSIXct(VAR_0075) ]
dtdt[, date_lag := difftime(date, shift(date, 1, type = 'lag'), units = "days") ]
dtdt[, date_lead:= difftime(date, shift(date, 1, type = 'lead'), units = "days") ]
dtdt

dtdt[, roll_lead := roll_mean(date_lead, 5000)]

# Quarterly Time Series of the Number of Australian Residents
data(austres	, "austres")
dtts = data.table(austres)
dtts
plot(dtts$austres)
dtts[, lead := austres - shift(austres, 1, type = 'lead')]
plot(dtts$lead)
dtts[, roll_lead := roll_mean(lead, 20)]
plot(dtts$roll_lead)

dim(X)

###########################################
# Modelling
#############################################
# put our testing & training data into two seperates Dmatrixs objects
X =  data.frame(dttxt[,grep('Count_', colnames(dttxt)), with=F])
y = as.numeric(dt$target)
X = data.frame(sapply(X, as.numeric))
Xtrain = as.matrix(X[1:100000,])
ytrain = y[1:100000]
Xtest = as.matrix(X[100001: 145231,])
ytest = y[100001: 145231]


bst <- xgboost(data = Xtrain, label = ytrain, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 50,objective = "binary:logistic")
y_pred = predict(bst, Xtest)
hist(y_pred)
plot(table(y_pred>0.4, ytest))





# Xnum = as.matrix(dt[,(sapply(dt, class) == "numeric"), with = FALSE])
# Xint = as.matrix(dt[,(sapply(dt, class) == "integer"), with = FALSE])
# 
# Xtrain = cbind(Xint[1:100000], Xnum[1:100000], as.matrix(X[1:100000,]))
# Xtest = cbind(Xint[100001: 145231,], Xnum[100001: 145231,], as.matrix(X[100001: 145231,]))
# 
# bst <- xgboost(data = Xtrain, label = ytrain, max_depth = 4,
#                eta = 1, nthread = 2, nrounds = 50,objective = "binary:logistic")
# y_pred = predict(bst, Xtest)
# hist(y_pred)
# plot(table(y_pred>0.9, ytest))


