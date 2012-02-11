"/Volumes/"

# 1. Parameter selection


# 1) select the readouts that span the phenotypic space
featSelection = match(c("par1","par1 - StdDev","par2 - Median"), names(R));


featSelection<-332

#load the necessary libraries
library(car)
library(alr3);
library(ggplot2);
iris
########
### dfdf

teprint <- function(x){
    print("test")
}

ggplot()

rename()
teprint("blab")
teprint(sdf)
subframe <- R[, featSelection];
subframe <- R[, featSelection];
subframe <- R[, featSelection];
subfr33ame <- R[, featSelection];

subfr
# caclulate the optimal lambda for the trafo
powers <- box.cox.powers(as.matrix(subframe));
normValues <- powtran(subframe, powers$lambda, family="box.cox")

# #debug plots
# par(mfrow=c(length(subframe),1))
# sapply(1:length(subframe), FUN = function(param) { hist(normValues[, param], n=100);} )


# replace normalized columns to the table

# add a suffix to the name of hte bxcx normalized columns 
bxCxNames <- paste(names(subframe), '.boxcox', sep = "");
names(normValues) <- bxCxNames;

# bind both dataframes together
R <- cbind(R, normValues);
