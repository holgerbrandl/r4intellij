args <- commandArgs(TRUE)
fName = args[1]
# fName = "scale_fill_manual"
# fName = "filter"
# fName = "mutate"
# fName = "library"

# hResults = help(fName)
options(help_type = "text")
hResults = help(fName,  try.all.packages=T)
# str(hResults)
# hResults[1]
# hResults[2]

if(length(hResults)>0){
    pName = basename(dirname(dirname(hResults[1])))
    help(fName, package=(pName)) ## use round bracket wrapper to avoid deparsing; see ?help
}




