
cd ~/Library/Caches/IntelliJIdea2017.1/plugins-sandbox/test/system/
rm -rf index
rm -rf r_skeletons

find . | grep index

#cat /Users/brandl/Library/Caches/IntelliJIdea2017.1/plugins-sandbox/system/r_skeletons/1842261700/dichromat.R
#cat /Users/brandl/Library/Caches/IntelliJIdea2017.1/plugins-sandbox/system/r_skeletons/1842261700/BH.R
#cat /Users/brandl/Library/Caches/IntelliJIdea2017.1/plugins-sandbox/system/r_skeletons/1842261700/MASS.R

## templates
cd /Users/brandl/Library/Caches/IntelliJIdea2017.1/plugins-sandbox
cat "./config/fileTemplates/internal/R Script.java"
find . | grep "R Script"
rm "./config/fileTemplates/internal/R Script.ft"
rm "./plugins/R4Intellij/classes/fileTemplates/internal/R Script.ft"






find ~/Library/Preferences/IntelliJIdea2017.1 | grep "R Script"
find ~/Library/Caches/IntelliJIdea2017.1 | grep "R Script"
find "~/Library/Application Support/IntelliJIdea2017.1" | grep "R Script"
