rm(list=ls())
theURL <- "http://vincentarelbundock.github.io/Rdatasets/csv/ggplot2/movies.csv";
mov <- read.table(file = theURL, header = TRUE, sep = ",")
mov = mov[,c("title", "votes", "rating")]

global_count = sum(mov$votes, na.rm = T)
global_mean = sum(mov$rating*mov$votes, na.rm = T)/global_count
global_count
global_mean
prior = 10000


mov$bayesMean = ( (mov$rating * mov$votes) + (prior * global_mean) )/(prior + mov$votes)

head(mov[order(-mov$bayesMean),], 20)
head(mov[order(-mov$rating),], 20)
head(mov[order(-mov$votes),], 20)
