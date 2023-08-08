#!/bin/bash

# Copyright (c) 2022-2023 Gonzalo Larumbe
# All rights reserved.

PKGS_TO_INSTALL=(global universal-ctags python3-pygments silversearcher-ag ripgrep libgnat-10)
EXPECTED_INSTALLED_BINARIES=(python global gtags ctags ag rg)

sudo apt-get update

for pkg in "${PKGS_TO_INSTALL[@]}"; do
    echo ""
    echo "Installing $pkg"
    sudo apt-get install "$pkg"
done

echo ""
echo "Checking binaries PATHs and versions..."

for bin in "${EXPECTED_INSTALLED_BINARIES[@]}"; do
    echo ""
    echo "$bin path: $(which $bin)"
    echo "$bin version: $($bin --version)"
done

# Setup GHDL (get latest release)
GHDL_GITHUB_URL=https://github.com/ghdl/ghdl
LATEST_RELEASE_URL=releases/download/v3.0.0
LATEST_RELEASE_FILE=ghdl-gha-ubuntu-22.04-llvm.tgz
echo ""
echo "Setting up GHDL..."
curl -L -o $LATEST_RELEASE_FILE $GHDL_GITHUB_URL/$LATEST_RELEASE_URL/$LATEST_RELEASE_FILE
sudo tar xvzf $LATEST_RELEASE_FILE --directory /usr
echo ""
echo "$(which ghdl)"
echo "ghdl version: $(ghdl --version)"


echo ""
echo "Setting up tree-sitter"
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
echo ""
echo "Building tree-sitter..."
make all
echo ""
echo "Installing tree-sitter..."
sudo make install
sudo ldconfig # Update ldconfig cache to find libtree-sitter
echo ""
echo "tree-sitter lib path: "
echo "$(sudo ldconfig -p | grep libtree-sitter)"
cd ..


