cmake_minimum_required(VERSION 3.3)

project(TheRPlugin_Device)
string(TOLOWER ${PROJECT_NAME} TARGET_NAME)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Werror")

if (WIN32)
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -static-libgcc -static -lpthread")
endif ()

find_package(R)
find_package(Boost 1.31.0 REQUIRED)

include_directories(${R_INCLUDE_DIRS})
include_directories(${Boost_INCLUDE_DIRS})

set(
        SOURCE_FILES
        Evaluator.cpp
        Interface.cpp
        MasterDevice.cpp
        ScopeProtector.cpp
        SlaveDevice.cpp
)

function(build_for_architecture BIT BIT_LIBRARIES)
    add_library(${TARGET_NAME}${BIT} SHARED ${SOURCE_FILES})

    set_target_properties(${TARGET_NAME}${BIT} PROPERTIES COMPILE_FLAGS "-m${BIT}" LINK_FLAGS "-m${BIT}")

    if (BIT_LIBRARIES)
        target_link_libraries(${TARGET_NAME}${BIT} ${BIT_LIBRARIES})
    endif ()
endfunction()

build_for_architecture(32 ${R_32_LIBRARIES})
build_for_architecture(64 ${R_64_LIBRARIES})
