
# use another normality test
library(nortest)


somedata <- R$"counts";

hist(somedata)
shapiro.test(somedata);
