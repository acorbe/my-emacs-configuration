#!/bin/bash

mv ~/.emacs ~/.emacs.bak
myPWD=`pwd`
cd 
ln -s $myPWD/emacs.el  .emacs
cd $myPWD
