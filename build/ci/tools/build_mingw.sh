#!/bin/sh

export PATH=/mingw64/bin:/usr/local/bin:/usr/bin:/bin:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl

VERSION=$(cmake -P config.cmake | sed -n -e "s/^.*VERSION  *//p")
BUILD_NUMBER=""
#CPUS=1

export VERBOSE=1

mkdir build.debug 2> /dev/null
cd build.debug || exit 1

echo "*** PATH: $PATH"
echo "*** Running cmake ***"
command -v cmake
cmake -G "MinGW Makefiles" \
      -DCMAKE_INSTALL_PREFIX=../win32install \
      -DCMAKE_BUILD_TYPE=DEBUG \
      -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
      -DCMAKE_MAKE_PROGRAM=mingw32-make.exe \
      -DBUILD_FOR_WINSTORE=OFF \
      .. || exit 1

echo "*** Running make ***"
ls -ltr /mingw64/bin/make
/mingw64/bin/make V=1 || exit 1

