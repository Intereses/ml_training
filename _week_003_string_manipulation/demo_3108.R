# Today we look at string operations in R. 
# You will need to load the library stringr. 
# In TI data preprocessing, we often want to,
#     Abbreviate strings (such as reducing the length of survey questions)
#     Stem strings - eg. we may have multiple job types which are close (System Admin & system Administrator) and we want to make them the same
#     Find or subset rows containing a certain string
#     Giving names to oridinal categories
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
# https://www.kaggle.com/crischir/word2vec-hashing-and-some-xgboost-models/code

# Now lets say we want to look for specific letters in a strings
library(stringr)
library(data.table) 
library(FeatureHashing)
library(Matrix)
library(tm)

# This removes anything from a previous session
rm(list=ls())

# Lets check current time
Sys.time()

# First we set our working directory
# Please change this to your working directory where the data is
# setwd("C:/Users/IBM_ADMIN/Box Sync/DScTraining/_3108_meeting")
setwd("/Users/dhanley2/Documents/ml_training_r/_003_meeting")
getwd()

# Lets load our data set, and take a quick look at it 
data("USArrests")
head(USArrests)
dim(USArrests)

# We see the rows are called after states of the US, and the column is the number of arrests per crime
states = rownames(USArrests)
states
substr(states, 1, 4)

# We can also use the abbreviate function which reduces to unique characters
states2 = abbreviate(states)
states2
abbreviate(states, minlength = 5)

# Lets look at the longest state name
states[1]
nchar(states[1:10])
states[which(nchar(states) > max(nchar(states))-5)]

# Get states with the string "w"
grep("w", states, value = T)

# Notice we miss Washington
grep("[wW]", states, value = T)  # ... this searches for both lower and upper case

# Alternate approach is make states lower case
tolower(states)
grep("w", tolower(states), value = T)
states[grep("w", tolower(states))]
# We can also do uppercase
toupper(states)

# Or we can check in the help
?grep
grep("w", states, ignore.case = T, value = T)



states[1]
str_count(states[1], "a")

states[7]
str_count(states[7], "a")

str_count(states, "a")

# Now we will look at String Manipulations
pi
PI = paste("Proj/Act", pi)
PI

IloveR = paste("I", "love", "R", sep = "-")
IloveR

paste("Column", 1:5000, sep = ".")

# Create dynamic columns
letters
LETTERS
paste(LETTERS, 1:26, sep="-")
paste(LETTERS, 1:26, sep="-", collapse = "")

# Lets print out strings
my_string = "The year was 1979."
print(my_string)
noquote(my_string)
cat(my_string, "GRRRRRR")

# Substituting strings
chartr("a", "A", my_string)
gsub("a", "A", my_string)


# You can also check out regular expressions
?gsub
# replace digit with '_'
sub("\\d", "_", my_string)
gsub("\\d", "_", my_string)
sub("\\D", "_", my_string)
gsub("\\D", "_", my_string)
gsub("[[:digit:]]", "@", my_string)

# special characters
s = "$ It can be annoying, when people @@ put in many special characters!"
s
gsub("[[:punct:]]", "", s)

# Other regular expressions for character substitutions 
# http://www.pnotepad.org/docs/search/regular_expressions/
# or look at 
# **************************************
# http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
# **************************************

# Taking portion of a string
s = "This is a boring string"
s
substr(s, 1, 8)
substr(s, nchar(s)-10, nchar(s))

# splitting strings
sentence = c("R is a collaborative project with many contributors")
str_split(sentence, " ")

tels = c("510-548-2238", "707-231-2440", "650-752-1300", "510-548-2239", "510-548-2403", "510-548-1111")
str_split(tels, "-")[[3]]
1:3

lapply(str_split(tels, "-"), function(x) x[1])
unlist(lapply(str_split(tels, "-"), function(x) x[1]))
table(unlist(lapply(str_split(tels, "-"), function(x) x[1])))

df = data.frame(phone=tels)
df
df$First = unlist(lapply(str_split(tels, "-"), function(x) x[1]))
df$Second = unlist(lapply(str_split(tels, "-"), function(x) x[2]))
df

# Matching characters
LETTERS
match("U", LETTERS)
match("A", LETTERS)
match(650, df[,2])
df[510 == df[,2],]

# Use Case

## function for converting some symbols
conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")
####Loading Classified texts
#id,Comment_text,toxic,severe_toxic,obscene,threat,insult,identity_hate
#Submission is in the following format
#id,toxic,severe_toxic,obscene,threat,insult,identity_hate

t1 <- fread("data/train.csv") 
t1$comment_text[1:10]
t1$comment_text <-  conv_fun(t1$comment_text)

t1$comment_text <- tolower(gsub("[^[:alnum:] ]", " ", t1$comment_text))
gc()
#cleaning function
cleaning_texts <- function(text){
  ## need to do tolower and remove stopwords bevor removing punctuation!!
  # all letters to lower
  text <- tolower(text)
  
  # remove links
  text <- gsub("(f|ht)tp(s?)://\\S+", " ", text)
  text <- gsub("http\\S+", "", text)
  text <- gsub("xml\\S+", "", text)
  
  # transform short forms
  text <- gsub("'ll", " will", text)
  text <- gsub("i'm", "i am", text)
  text <- gsub("'re", " are", text)
  text <- gsub("'s", " is", text)
  text <- gsub("'ve", " have", text)
  text <- gsub("'d", " would", text)
  
  # remove "shittext"
  text <- gsub("(a|e)w+\\b", "", text)
  text <- gsub("(y)a+\\b", "", text)
  text <- gsub("(w)w+\\b", "", text)
  text <- gsub("a?(ha)+\\b", "", text)
  #gsub("((a+)|(h+))(a+)((h+)?)\\b", "", "explanation hardcore aaaaaaahhhhhhhhh hhhaaaaaah haaaaaaah aaaaaaaa haaaa")
  text <- gsub("((a+)|(h+))(a+)((h+)?)\\b", "", text)
  text <- gsub("((lol)(o?))+\\b", "", text)
  #gsub("(?<=\\w(ck))\\s(?=(ing)\\b)", "", "fuck ing suck ing lick ing", perl = T)
  text <- gsub("(?<=\\w(ck))\\s(?=(ing)\\b)", "", text, perl = T)
  
  # remove nicknames
  #gsub("@\\w+", " ", "@nana, das @ ist ein @spieler")
  text <- gsub("@\\w+", " ", text)
  
  # remove linebreaks
  text <- gsub("\n", " ", text)
  # remove graphics
  text <- gsub("[^[:graph:]]", " ", text)
  # remove punctuation
  text <- gsub("[[:punct:]]", " ", text)
  # remove digits
  text <- gsub("[[:digit:]]", " ", text)
  # strip multiple whitspace to one
  text <- gsub("\\s{2}", " ", text)
  text <- gsub("(?<=\\b\\w)\\s(?=\\w\\b)", "", text, perl = T)
  
  # again clean shittext
  text <- gsub("((lol)(o?))+", "", text)
  text <- gsub("(?<=\\b(fu|su|di|co|li))\\s(?=(ck)\\b)", "", text, perl = T)
  text <- gsub("(?<=\\w(ck))\\s(?=(ing)\\b)", "", text, perl = T)
  text <- gsub("(?<=\\w(uc))\\s(?=(ing)\\b)", "", text, perl = T)
  #gsub("(?<=\\b(fu|su|di|co|li))\\s(?=(ck)\\w)", "", tolower(train_data[79644,]$comment_text), perl = T)
  text <- gsub("(?<=\\b(fu|su|di|co|li))\\s(?=(ck)\\w)", "", text, perl = T)
  text <- gsub("cocksu cking", "cock sucking", text)
  text <- gsub("du mbfu ck", "dumbfuck", text)
  text <- gsub("cu nt", "cunt", text)
  text <- gsub("(?<=\\b(fu|su|di|co|li))\\s(?=(k)\\w)", "c", text, perl = T)
  
  # strip multiple whitspace to one
  text <- gsub("\\s+", " ", text)
  # remove tailing whitespaces
  text <- gsub("\\s*$", "", text)
  # remove leading whitespaces
  text <- gsub("^\\s+", "", text)
  
  # remove single letter words but save "I"
  text <- gsub("\\W*\\b\\w\\b\\W*", " ", text)
  
  # again strip multiple whitspace to one
  text <- gsub("\\s+", " ", text)
  
  unname(text)
}
t1[, comment_text:= cleaning_texts(comment_text)]
t1$comment_text[1:10]
#little arangement
t1$comment_text[333]
strwrap(t1$comment_text[457], width = 80)

#hashing FeatureHashing
d1 <- hashed.model.matrix(~ split(comment_text, delim = " ", type = "tf-idf"),
                          data = t1, hash.size = 2^22, signed.hash = FALSE)
d116 <- hashed.model.matrix(~ split(comment_text, delim = " ", type = "tf-idf"),
                            data = t1, hash.size = 2^16, signed.hash = FALSE)
dim(d1)
dim(d16)

int("offensive")

d1[457, ]
as.integer(which(d1[457, ] != 0))
