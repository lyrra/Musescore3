#!/usr/bin/env bash

echo "Setup MacOS build environment"

#trap 'echo Setup failed; exit 1' ERR
#SKIP_ERR_FLAG=true

#export MACOSX_DEPLOYMENT_TARGET=10.10

# install dependencies
#wget -c --no-check-certificate -nv -O bottles.zip https://musescore.org/sites/musescore.org/files/2020-02/bottles-MuseScore-3.0-yosemite.zip
#unzip bottles.zip

# we don't use freetype
#rm bottles/freetype* | $SKIP_ERR_FLAG

echo "-- run brew update --"
brew update
#echo "-- run brew upgrade--"
#brew upgrade

# fixing install python 3.11 error (it is a dependency for JACK)
rm '/usr/local/bin/2to3'
rm '/usr/local/bin/2to3-3.11'
rm '/usr/local/bin/idle3'
rm '/usr/local/bin/idle3.11'
rm '/usr/local/bin/pydoc3'
rm '/usr/local/bin/pydoc3.11'
rm '/usr/local/bin/python3'
rm '/usr/local/bin/python3.11'
rm '/usr/local/bin/python3-config'
rm '/usr/local/bin/python3.11-config'

echo "-- install packages --"
# additional dependencies
brew install cmake
brew install jack
brew install lame
brew install libogg
brew install libvorbis
brew install flac
brew install libsndfile
brew install portaudio
brew install qt5
brew link qt5 --force

brew install gcc
echo "-- done install packages --"
which qmake
command -v qmake

#BREW_CELLAR=$(brew --cellar)
#BREW_PREFIX=$(brew --prefix)

#function fixBrewPath {
#  DYLIB_FILE=$1
#  BREW_CELLAR=$(brew --cellar)
#  BREW_PREFIX=$(brew --prefix)
#  chmod 644 $DYLIB_FILE
#  # change ID
#  DYLIB_ID=$(otool -D  $DYLIB_FILE | tail -n 1)
#  if [[ "$DYLIB_ID" == *@@HOMEBREW_CELLAR@@* ]]
#  then
#      PSLASH=$(echo $DYLIB_ID | sed "s,@@HOMEBREW_CELLAR@@,$BREW_CELLAR,g")
#      install_name_tool -id $PSLASH $DYLIB_FILE
#  fi
#  if [[ "$DYLIB_ID" == *@@HOMEBREW_PREFIX@@* ]]
#  then
#      PSLASH=$(echo $DYLIB_ID | sed "s,@@HOMEBREW_PREFIX@@,$BREW_PREFIX,g")
#      install_name_tool -id $PSLASH $DYLIB_FILE
#  fi
#  # Change dependencies
#  for P in `otool -L $DYLIB_FILE | awk '{print $1}'`
#  do
#    if [[ "$P" == *@@HOMEBREW_CELLAR@@* ]]
#    then
#        PSLASH=$(echo $P | sed "s,@@HOMEBREW_CELLAR@@,$BREW_CELLAR,g")
#        install_name_tool -change $P $PSLASH $DYLIB_FILE
#    fi
#    if [[ "$P" == *@@HOMEBREW_PREFIX@@* ]]
#    then
#        PSLASH=$(echo $P | sed "s,@@HOMEBREW_PREFIX@@,$BREW_PREFIX,g")
#        install_name_tool -change $P $PSLASH $DYLIB_FILE
#    fi
#  done
#  chmod 444 $DYLIB_FILE
#}
#export -f fixBrewPath

#function installBottleManually {
#  brew unlink $1
#  rm -rf /usr/local/Cellar/$1
#  tar xzvf bottles/$1*.tar.gz -C $BREW_CELLAR
#  find $BREW_CELLAR/$1 -type f -name '*.pc' -exec sed -i '' "s:@@HOMEBREW_CELLAR@@:$BREW_CELLAR:g" {} +
#  find $BREW_CELLAR/$1 -type f -name '*.dylib' -exec bash -c 'fixBrewPath "$1"' _ {} \;
#  brew link $1
#}



#export QT_SHORT_VERSION=5.15.2
#export QT_PATH=$HOME/Qt
#export QT_MACOS=$QT_PATH/$QT_SHORT_VERSION/clang_64
#export PATH=$PATH:$QT_MACOS/bin
#echo "PATH=$PATH" >> $GITHUB_ENV
#wget -nv -O qt5.zip https://s3.amazonaws.com/utils.musescore.org/Qt5152_mac.zip
#mkdir -p $QT_MACOS
#unzip -qq qt5.zip -d $QT_MACOS
#rm qt5.zip


#install sparkle
#export SPARKLE_VERSION=1.20.0
#mkdir Sparkle-${SPARKLE_VERSION}
#cd Sparkle-${SPARKLE_VERSION}
#wget -nv https://github.com/sparkle-project/Sparkle/releases/download/${SPARKLE_VERSION}/Sparkle-${SPARKLE_VERSION}.tar.bz2
#tar jxf Sparkle-${SPARKLE_VERSION}.tar.bz2
#cd ..
#mkdir -p ~/Library/Frameworks
#mv Sparkle-${SPARKLE_VERSION}/Sparkle.framework ~/Library/Frameworks/
#rm -rf Sparkle-${SPARKLE_VERSION}

echo "Setup script done"
