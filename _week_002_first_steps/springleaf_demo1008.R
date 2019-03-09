# Data set from - Springleaf Financial Personal loans for bill consolidation, home improvements or unexpected expenses. 
# Easy to apply for online or at one of our 700+ branches.

# In this competition, Springleaf is asking you to predict which customers will respond to a direct mail offer.
# For Data Description see : https://www.kaggle.com/c/springleaf-marketing-response
# For Data Source see : https://www.kaggle.com/c/springleaf-marketing-response/data

# This removes anything from a previous session
rm(list=ls())

# Lets check current time
time.1 <- Sys.time()
time.1
format(time.1, "%m-%d-%Y-------------------------------%H:%M:%S")
format(time.1, "%d-%m-%Y")

# First we set our working directory
# Please change this to your working directory where the data is
# setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_1008_meeting")
setwd("/Users/dhanley2/Documents/ml_training_r/_002_meeting")
getwd()

# # Now lets install some packages
# # this is a one time effort - after this you have the package on your PC
# install.packages("caret")
# install.packages("readr")
# install.packages("data.table")
# install.packages("R.utils")

# Now we load the packages for the session 
# this is needed each time you want to use the package
library(caret)
# library(readr)
library(data.table)
library(R.utils)

# Now we read in the file, its big so lets take the first 20K rows only
getwd() # Check our working directory
# train <- read_csv("train.csv", n_max=30000)
train <- fread("train.csv", nrows=30000)
class(train)
train = data.frame(train)
#?read_csv
?fread

# lets look at the file file dimensions quickly
dim(train)
nrow(train)
ncol(train)
colnames(train)
head(train, 10)

# now we look at indexing a data frame (same applies to a matrix)
# this is a comment - because of the "#"
train[1,1]  # first row, first columns
train[nrow(train), ncol(train)] # last row, last column
train[1:4, 1:4] # first four rows, first four columns
train[,grep("VAR_002", colnames(train))]
?grep
train$ID # first column
train[,1] # first column

# We take our response variable and save it to a vector
y = train$target
hist(y)
hist(y, main = "Direct Mail offer response", col="red", xlab = "Responded")
table(y, train$VAR_0001)
plot(table(train$VAR_0001, y))


# remove the id and target, so we are left with predictor variables
train = subset(train, select=-c(ID, target))
dim(train)

# get the rowcount of the full file
#library(R.utils)
row_count = countLines("train.csv") 
cat("Row count : ", row_count[1], "; Predictor column count : ", ncol(train))


# The proportion of NA values.
notMissing = 1
missing = NaN
is.na(notMissing)

length(train[   is.na(train)     ]) # number of missing values
length(train$VAR_0074[   is.na(train$VAR_0074)     ]) # number of missing values

length(train[is.na(train)])   /   (ncol(train)*nrow(train)) # proportion of missing values

# Check for duplicate rows.
var = c(1,1,1,2,3,4,5)
length(var)
length(unique(var))

nrow(train) - nrow(unique(train))


# Lets look at the columns with only one unique value. (Sapply is a bit more complex, dont worry if it needs more time)
?sapply
col_ct = sapply(train, function(x) length(unique(x))) 

col_ct # how many unique variables does each columns have

cat("Constant feature count:", length(col_ct[col_ct==1]))
cat("Constant feature names:", names(col_ct[col_ct==1]))


# Identify and separate the numeric and non numeric rows.
train_numr = train[, sapply(train, function(x) is.numeric(x))]
train_char = train[, sapply(train, function(x) is.character(x))]
cat("Numerical column count : ", dim(train_numr)[2], 
    "; Character column count : ", dim(train_char)[2])

# Lets digs into the character features.
?str
str(train_char)
str(lapply(train_char, unique), vec.len = 4) # this one is a bit more complext but useful

# It looks like NA is represented in character columns by -1 or [] or blank values, lets convert these to explicit NAs. 
train_char[train_char==-1] = NA
train_char[train_char==""] = NA
train_char[train_char=="[]"] = NA

# We place the date columns in a new dataframe and parse the dates
?grep
stringvar = c("I", "like", "milk")
grep("like", stringvar)
grep("JAN1|FEB1|MAR1", train_char)
names(train_char)
names(train_char)[grep("JAN1|FEB1|MAR1", train_char)]

# Now lets separate out the dates from the character columns and look at them further.
train_date = train_char[,grep("JAN1|FEB1|MAR1", train_char),]
str(train_date)
class(train_date)
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
str(train_date)
class(train_date)
train_date = do.call(cbind.data.frame, train_date)
str(train_date)
class(train_date)


# Now lets plot out some dates
hist(train_date[,1], "weeks", format = "%d %b %y", main = "first column", xlab="", ylab="")

?par
par(mar=c(2,2,2,2),mfrow=c(4,4))
for(i in 1:16) {
  hist(train_date[,i], "weeks", format = "%d %b %y", main = colnames(train_date)[i], xlab="", ylab="")
}
install.packages('caret')
library(caret)

