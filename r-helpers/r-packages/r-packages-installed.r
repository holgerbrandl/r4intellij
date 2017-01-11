p = installed.packages()[,c("Package", "Version","LibPath")]
for( i in seq( length(p)/3)) {
  print(paste(p[i,1],p[i,2],p[i,3],sep=" "))
}