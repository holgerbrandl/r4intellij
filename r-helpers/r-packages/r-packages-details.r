args <- commandArgs(TRUE)
chooseCRANmirror(ind = args[2])
number.defaults = as.numeric(args[3])
defauls = if(number.defaults>0){
  args[4:(3+number.defaults)]
}
additional = if((length(args)-3-number.defaults)>0){
                args[(4+number.defaults):length(args)]}
setRepositories(FALSE,defauls,additional)
p = available.packages()
cat(p[args[1], "Version"])
cat("\t")
cat(p[args[1], "Depends"])
cat("\t")
cat(p[args[1], "Repository"])