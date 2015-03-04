
# use another normality test
library(nortest)


somedata <- R$"counts";

hist(somedata)
shapiro.test(somedata);


somedata <- R$"Nuclei DAPI - Number of Objects.boxcox";

hist(somedata);
hist(2);
shapiro.test<-test(somedata);

aa <- function(tt="sdfsdf") {
return(a)
}

