rm(list=ls())

# Create some variables
6 + 6
a = 3
b = 4
a
b

# Lets add them
a + b

# lets try a character vector
char = c("The brown cow", "said", "moo")
char[3]

# Boolean
bool = c(TRUE, TRUE, FALSE, T, F, T, F, F)
bool

# now lets create variables
v1 = c(1, 5, 9, 9, 5, 5, 5, 5)
v2 = c(a, b, 7, 5, 5, 5, 5, 5)
v1 * v2

# now lets put them in a matrix
m1 = matrix(c(v1, v2), nrow = length(v1))
m1

# rename the colums
colnames(m1) = c("First", "Second")
m1
rownames(m1) = 1:nrow(m1)
m1

# lets get a description of our matrix
summary(m1)
?summary
?grep

# lets get a compact description of our matrix
str(m1)
?str

# If we want to add a different type of variable to a matrix we need to use a data frame
df1 = data.frame(col1 = v1, col2 = v2, col4 = bool)
df1
str(df1)

