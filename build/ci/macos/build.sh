#!/usr/bin/env bash

echo "Build MacOS MuseScore"

echo "---- opt:"
ls /usr/local/opt

echo "---- Applications:"
ls /Applications
export PATH=$PATH:/usr/local/opt/qt5/bin

command -v qmake

export DEVELOPER_DIR=/Applications/Xcode_12.4.app/Contents/Developer
unset DEVELOPER_DIR

ARTIFACTS_DIR=build.artifacts
BUILD_AUTOUPDATE=OFF

mkdir build.debug 2> /dev/null
cd build.debug || exit 1

cmake -G Xcode \
      -DCMAKE_INSTALL_PREFIX=install \
      -DCMAKE_BUILD_TYPE=DEBUG \
      -DVERBOSE=1 \
      -DBUILD_SHARED=ON \
      -DGUILE=1 \
      -DCMAKE_VERBOSE_MAKEFILE:BOOL=ON \
      -DBUILD_CRASH_REPORTER=OFF \
      -DBUILD_PULSEAUDIO=OFF \
      -DDOWNLOAD_SOUNDFONT=ON \
      -DLOGLEVEL=4 \
      ..
