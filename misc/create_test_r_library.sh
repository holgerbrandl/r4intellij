## Create and maintain R setup that contains all packages required for running the r4ij unit test suite


########################################################################################################################
## re-establish base installation with test packages

cd /Library/Frameworks/R.framework/Versions/3.4/Resources

cd library_work
rm -rf library
mkdir library
cd library_work
cp -r stats  stats4 graphics grDevices utils datasets grid methods base tcltk tools parallel splines compiler ../library


Rscript - <<"EOF"
chooseCRANmirror(graphics=FALSE, ind=10)
install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/holgerbrandl/datautils/v1.38/R/core_commons.R")

load_pack(R.utils)
load_pack(lubridate)
load_pack(caret)
load_pack(tidyverse)
load_pack(mvtnorm)
EOF


## backup this library for later
cp -rf library library_r4intellij

## zip them up for later
cd /Library/Frameworks/R.framework/Versions/3.4/Resources
tar -zcvf minimal_r_library.tar.gz library_r4intellij

########################################################################################################################
## switch between libraries

cd /Library/Frameworks/R.framework/Versions/3.4/Resources

sudo rm -rf library
sudo cp -rf library_work library
sudo cp -rf library_r4intellij library


# TODO automate skeleton creation here and update test-resources accordingly