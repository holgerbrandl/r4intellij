## Create a R setup that contains all packages required for running the r4ij unit test suite

## clone R
cd /Library/Frameworks/R.framework/Versions/3.4/Resources
#rm -rf library
mv library library_work
mv library library_r4intellij
mkdir library

## re-establish base packages
cd library_work
cp -r stats  stats4 graphics grDevices utils datasets grid methods base tcltk tools parallel splines ../library
cp -r compiler ../library

Rscript - <<"EOF"
chooseCRANmirror(graphics=FALSE, ind=10)
install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/holgerbrandl/datautils/v1.38/R/core_commons.R")

load_pack(R.utils)
load_pack(lubridate)
load_pack(caret)
load_pack(tidyverse)
EOF

#cd /Library/Frameworks/R.framework/Versions/3.4_devel/Resources/library
#cd /Library/Frameworks/R.framework/Versions/3.4_devel
#./R

## backup this library for later
cp -rf library library_r4intellij

## zip them up for later
cd /Library/Frameworks/R.framework/Versions/3.4/Resources
tar -zcvf minimal_r_library.tar.gz library_r4intellij


## switch to a library
cd /Library/Frameworks/R.framework/Versions/3.4/Resources
rm -rf library
cp -rf library_work library
cp -rf library_r4intellij library
