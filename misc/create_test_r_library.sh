## Create a R setup that contains all packages required for running the r4ij unit test suite

## clone R
cd /Library/Frameworks/R.framework/Versions/3.3/Resources
mv library library_all
mkdir library

## re-establish base packages
cd library_all
cp -r stats graphics grDevices utils datasets grid methods base tcltk tools ../library

Rscript - <<"EOF"
install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/holgerbrandl/datautils/v1.38/R/core_commons.R")
load_pack(R.Utils)
load_pack(lubridate)
EOF

#cd /Library/Frameworks/R.framework/Versions/3.3_devel/Resources/library
#cd /Library/Frameworks/R.framework/Versions/3.3_devel
#./R


## zip them up for later
cd /Library/Frameworks/R.framework/Versions/3.3/Resources
tar -zcvf minimal_r_library.tar.gz library
