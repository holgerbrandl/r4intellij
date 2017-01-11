#format of args <cran mirror> <number of default repos> <indexies of default repos> <users repos>
args <- commandArgs(TRUE)
chooseCRANmirror(ind = args[1])
number.defaults = as.numeric(args[2])
defaults = if(number.defaults>0){
  args[3:(2+number.defaults)]
}
additional = if((length(args)-2-number.defaults)>0){
                args[(3+number.defaults):length(args)]}
setRepositories(FALSE,defaults,additional)
p = available.packages()[,c("Package","Version","Repository")]
for( i in seq( length(p)/3)) {
  print(paste(p[i,1],p[i,2],p[i,3],sep=" "))
}