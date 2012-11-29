somedata <- R$"counts";

hist(somedata)
shapiro.test(somedata);


# normalize by range and shift by 10 to get something shapiro can work with

datarange <- range(somedata)
normdata <- 5 + (somedata-mean(somedata))/(datarange[2] - datarange[1]) 

# use another normality test
library(nortest)

help(package=nortest)