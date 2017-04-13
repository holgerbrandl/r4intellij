## Create a R setup that contains all packages required for running the r4ij unit test suite

## clone R
cd /Library/Frameworks/R.framework/Versions/3.3/Resources
mv library library_all_new
mkdir library

## re-establish base packages
cd library_all
cp -r stats  stats4 graphics grDevices utils datasets grid methods base tcltk tools ../library
cp -r parallel ../library

Rscript - <<"EOF"
install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/holgerbrandl/datautils/v1.38/R/core_commons.R")
load_pack(R.Utils)
load_pack(lubridate)
EOF

#cd /Library/Frameworks/R.framework/Versions/3.3_devel/Resources/library
#cd /Library/Frameworks/R.framework/Versions/3.3_devel
#./R

## backup this library for later
cp -rf library r4intellij_

## zip them up for later
cd /Library/Frameworks/R.framework/Versions/3.3/Resources
tar -zcvf minimal_r_library.tar.gz library
