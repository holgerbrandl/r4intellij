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

cmake CMakeLists.txt
make

find /Library/Frameworks/R.framework/ | grep Rinternals.h

```



Useful links

[How to check the operating system is MacOSX?](http://public.kitware.com/pipermail/cmake/2012-September/052049.html)

[Original Author comments on build issues](https://github.com/ktisha/TheRPlugin/issues/33#issuecomment-296237071)





## Acknowledgements

Original code was developed in https://github.com/sproshev/TheRPlugin_Device and imported with the author's [permission](https://github.com/ktisha/TheRPlugin/issues/33#issuecomment-296237071)