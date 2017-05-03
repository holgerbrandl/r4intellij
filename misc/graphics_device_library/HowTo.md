# Device Libraries for Graphics Integration


R allows provide custom bindings for graphics output. See https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/Devices.html for a general description.


## Prerequisites

### macOS

[Download](https://cmake.org/download/) and install cmake (which is a Makefile generator):
```bash
## add cmake to PATH
sudo /Applications/CMake.app/Contents/bin/cmake-gui --install=/usr/local/bin
```

Add required dependencies
```bash
## http://stackoverflow.com/questions/26024878/cmake-cannot-find-boost-on-os-x-with-brew
brew install boost-python
```

And finally build it
```bash
cd /Users/brandl/projects/rplugin/r4intellij/misc/graphics_device_library
cmake .
make

# to redo cmake clear files first
rm -rf CMakeCache.txt cmake_install.cmake Makefile

#find /Library/Frameworks/R.framework/ | grep Rinternals.h
```


## R Usage

when starting a new instance the following code will set our output device
```r
# dyn.load("~/Library/Caches/IntelliJIdea2017.1/plugins-sandbox/plugins/R4Intellij/classes/libtherplugin_device64.so")
dyn.load("/Users/brandl/projects/rplugin/r4intellij/misc/graphics_device_library/libtherplugin_device64.dylib")

jetbrains_ther_device_init <- function() { 
    .Call("jetbrains_ther_device_init", "/Users/brandl/projects/r4i__tests/r4i_test_project/.idea/snapshots") 
}
    
options(device=jetbrains_ther_device_init)
plot(1:3)
```

### Debug Usage

1. create a new R console in intellij
2. run (with adjusted paths)
```r
dyn.load("~/Library/Caches/IntelliJIdea2017.1/plugins-sandbox/plugins/R4Intellij/classes/libtherplugin_device64.dylib")
jetbrains_ther_device_init <- function() { .Call("jetbrains_ther_device_init", "/Users/brandl/IdeaProjects/untitled/.idea/snapshots") }
options(device=jetbrains_ther_device_init)
```

```
require(ggplot2); ggplot(iris,aes(Species)) + geom_bar() 

```


## Useful links

[How to check the operating system is MacOSX?](http://public.kitware.com/pipermail/cmake/2012-September/052049.html)

[Original Author comments on build issues](https://github.com/ktisha/TheRPlugin/issues/33#issuecomment-296237071)

[rstudio plot device](https://github.com/rstudio/rstudio/tree/master/src/cpp/r/session/graphics)

[knime r plot canvas](https://github.com/knime-mpicbg/knime-scripting/blob/develop/de.mpicbg.knime.scripting.r/src/de/mpicbg/knime/scripting/r/node/plot/RPlotCanvas.java#L113)

http://stackoverflow.com/questions/7171523/in-r-how-to-plot-into-a-memory-buffer-instead-of-a-file

[RCaller](https://github.com/jbytecode/rcaller/blob/master/RCaller/src/main/java/examples/SimplePlot.java)

[jvm only devices](https://github.com/bedatadriven/renjin)


[javagd as device](http://stackoverflow.com/questions/43656227/how-to-use-javagd-as-r-graphics-device)

### JRI
http://stackoverflow.com/questions/19100875/java-r-interface-jri-setup

https://comsysto.com/blog-post/java-r-integration-with-jri-for-on-demand-predictions


http://algorithm-forge.com/techblog/2010/07/using-javagd-in-java-applications/

http://stackoverflow.com/questions/26643852/ggplot-plots-in-scripts-do-not-display-in-rstudio

describes print is needed for ggplot

[use capture.out to force print in rEngine](http://stackoverflow.com/questions/28315947/get-same-output-as-r-console-in-java-using-jri)

[nice javagd scale example](https://pastebin.com/b2FAV6X4)

## ToDo / ToTest

* Resize should replot and just not just rescale pic (reinitialiation of device may be needed)

## Concept

MasterDevice corresponds to https://github.com/rstudio/rstudio/blob/cfce730f051fbab56a13ff218132f4802dcefdab/src/cpp/r/session/graphics/RGraphicsDevice.cpp


knime r integration how is it done? https://tech.knime.org/forum/r-statistics-nodes-and-integration/strange-error-from-r-interactive-nodes

## Acknowledgements

Original code was developed in https://github.com/sproshev/TheRPlugin_Device and imported with the author's [permission](https://github.com/ktisha/TheRPlugin/issues/33#issuecomment-296237071)