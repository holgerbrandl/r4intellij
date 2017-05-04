# Device Libraries for Graphics Integration


R allows provide custom bindings for graphics output. See https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/Devices.html for a general description.

---
https://books.google.de/books?id=n5rNBQAAQBAJ&pg=PA57&lpg=PA57&dq=%22options(device%22+R&source=bl&ots=yl_hfJMc7Y&sig=wn63wrTjDx87wAhvdGynL5tOlxA&hl=de&sa=X&ved=0ahUKEwiN8__Y0NTTAhUlDJoKHZmjBZw4ChDoAQgtMAI#v=onepage&q=%22options(device%22%20R&f=false

r-only custom device

```r
optoins
```

---
[Making my own graphics device](http://r.789695.n4.nabble.com/Making-my-own-graphics-device-td4695629.html#a4695636)

The "canonical" approach is to look at the source code for one of the  
built-in devices in the R source code and change it to do what you want  
(e.g., src/modules/X11/devX11.c). The file  
src/include/R_ext/GraphicsDevice.h has comments describing how the  
graphics device should be set up. See the RSvgDevice package for an  
example of a device implemented in a package.

---
best ref
https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Graphics-devices



## rJava

https://github.com/s-u/rJava

* seems activly developed


see setup of https://github.com/s-u/rJava, which answers both **cannot find JRI n** and " **cannot find system Renviron** "

Related links
* [nice javagd scale example](https://pastebin.com/b2FAV6X4)
* http://stackoverflow.com/questions/19100875/java-r-interface-jri-setup
* https://comsysto.com/blog-post/java-r-integration-with-jri-for-on-demand-predictions


## javagd


An actual device implemented in java with some native bindings
http://algorithm-forge.com/techblog/2010/07/using-javagd-in-java-applications/


See `/Users/brandl/projects/rplugin/related_projects/java_gd_example` inspired from
very gooddL http://stackoverflow.com/questions/30210495/how-to-visualize-my-r-plot-in-my-jframe-with-javagd
https://techtravelo.wordpress.com/2014/05/15/getting-jri-works-for-r-and-java/


Has issues
* non-native font-rendering, see http://stackoverflow.com/questions/43655984/how-to-match-font-rendering-between-r-quartz-and-javagd-devices
* unclear about how to use it as default device, see http://stackoverflow.com/questions/43656227/how-to-use-javagd-as-r-graphics-device
* print is needed for ggplot, so not really suited for interactive use, see [use capture.out to force print in rEngine](http://stackoverflow.com/questions/28315947/get-same-output-as-r-console-in-java-using-jri)


Related links
* [nice javagd scale example](https://pastebin.com/b2FAV6X4)

## RStudio graphics device

MasterDevice: https://github.com/rstudio/rstudio/blob/cfce730f051fbab56a13ff218132f4802dcefdab/src/cpp/r/session/graphics/RGraphicsDevice.cpp

[rstudio plot device](https://github.com/rstudio/rstudio/tree/master/src/cpp/r/session/graphics)


http://stackoverflow.com/questions/26643852/ggplot-plots-in-scripts-do-not-display-in-rstudio

---
https://tech.knime.org/forum/r-statistics-nodes-and-integration/strange-error-from-r-interactive-nodes

knime r integration how is it done?
* no graphics panel for provided nodes
* rserver impl for [tds-nodes](https://github.com/knime-mpicbg/knime-scripting/blob/develop/de.mpicbg.knime.scripting.r/src/de/mpicbg/knime/scripting/r/node/plot/RPlotCanvas.java#L113)


---
http://thecoatlessprofessor.com/programming/detecting-if-r-is-in-rstudio-and-changing-rstudios-default-graphing-device/

override rstudio custom graphics device

```
external_graphs = function(ext = TRUE){
  if( is.rstudio() ){
    if(isTRUE(ext)){
      o = tolower(Sys.info()["sysname"])
      a = switch(o,
                 "darwin"  = "quartz",
                 "linux"   = "x11",
                 "windows" = "windows")
      options("device" = a)
    } else{
      options("device"="RStudioGD")
    }
    
    # Kill open graphic devices
    graphics.off()
  }
    }
```

## ToDo / ToTest

* Resize should replot and just not just rescale pic (reinitialiation of device may be needed)


## Other

[RCaller](https://github.com/jbytecode/rcaller/blob/master/RCaller/src/main/java/examples/SimplePlot.java) does not incluude a graphics device

http://stackoverflow.com/questions/7171523/in-r-how-to-plot-into-a-memory-buffer-instead-of-a-file

[jvm only devices](https://github.com/bedatadriven/renjin) not applicable because we want to use native R
