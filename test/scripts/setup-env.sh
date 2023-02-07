#!/bin/bash

# Copyright (c) 2022-2023 Gonzalo Larumbe
# All rights reserved.

PKGS_TO_INSTALL=(global universal-ctags python3-pygments silversearcher-ag ripgrep ghdl)
EXPECTED_INSTALLED_BINARIES=(python global gtags ctags ag rg ghdl)

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

