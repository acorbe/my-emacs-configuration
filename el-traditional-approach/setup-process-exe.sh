#!/bin/bash


## texlab
wget https://github.com/latex-lsp/texlab/releases/download/v2.2.2/texlab-x86_64-linux.tar.gz
tar -xf texlab*
sudo cp texlab /usr/bin

## language tool
wget https://languagetool.org/download/LanguageTool-stable.zip
unzip LanguageTool-stable.zip
