# R_INCLUDE_DIRS
# R_64_LIBRARIES
# R_32_LIBRARIES

if (UNIX)
    set(R_BIT 64) # Architecture of installed R

    set(R_INCLUDE_DIRS "/usr/share/R/include") # Default include directory

    set(R_64_LIBRARIES R_64_LIBRARIES-NOTFOUND)
    set(R_32_LIBRARIES R_32_LIBRARIES-NOTFOUND)

    if (R_BIT EQUAL 64)
        find_library(R_64_LIBRARIES NAMES R HINTS "/usr/lib/R/lib") # Default lib directory
    elseif (R_BIT EQUAL 32)
        find_library(R_32_LIBRARIES NAMES R HINTS "/usr/lib/R/lib") # Default lib directory
    endif ()
elseif (WIN32)
    set(R_HOME "C:/Program Files/R/R-3.2.2") # Directory of installed R

    set(R_INCLUDE_DIRS "${R_HOME}/include") # Default include directory

    find_library(R_64_LIBRARIES NAMES R HINTS "${R_HOME}/bin/x64") # Default lib directory for 64-bit R
    find_library(R_32_LIBRARIES NAMES R HINTS "${R_HOME}/bin/i386") # Default lib directory for 32-bit R
endif ()