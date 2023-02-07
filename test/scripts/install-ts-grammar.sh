#!/bin/bash

set -u
set -e

if [ "$(uname)" == "Darwin" ]; then
    soext="dylib"
elif uname | grep -q "MINGW" > /dev/null; then
    soext="dll"
else
    soext="so"
fi

echo "Building tree-sitter VHDL grammar"
URL="https://github.com/alemuller/tree-sitter-vhdl.git"
echo "Cloning $URL"
git clone $URL --depth 1 --quiet

### Build
echo "Building VHDL grammar..."
cd tree-sitter-vhdl/src
cc -fPIC -c -I. parser.c
cc -fPIC -shared *.o -o "libtree-sitter-vhdl.${soext}"

### Copy out
DESTDIR=$HOME/.emacs.d/tree-sitter
echo "Copying libtree-sitter-vhdl.${soext} to $DESTDIR"
mkdir -p $DESTDIR
cp -v "libtree-sitter-vhdl.${soext}" $DESTDIR
ls -al $DESTDIR | grep libtree-sitter-vhdl
cd ../..
rm -rf tree-sitter-vhdl

