# my-emacs-configuration

This is my current emacs configuration, updated as it goes. Essential in my daily work as reasearcher/developer :). 

Tested on emacs 26.2 (how to install latest emacs: [linux](http://ubuntuhandbook.org/index.php/2019/02/install-gnu-emacs-26-1-ubuntu-18-04-16-04-18-10/),  [macos](https://emacs.stackexchange.com/questions/37240/how-install-emacs-26-or-whatever-latest-ver-on-mac)).

## Installation

+ Just replace your `~/.emacs` file with the file in this repo. Make sure your `~/.emacs.d` is empty. 
+ All the packages will install by themselves. :)
+ Finalize by:
    + <kbd>M-x all-the-icons-install-fonts</kbd>
    + <kbd>M-x company-tabnine-install-binary</kbd>
+ For elpy to work properly:
  + `pip install jedi rope autopep8 yapf black flake8`
`


## Comes with
+ IDE: treemacs, icons, centaur tabs (all enabled by default in display-graphic)
+ default themes: gui/display-graphic: `doom-opera-light`, terminal: `zenburn`
+ modeline: doom 
+ company, company-tabnine
+ helm, with fuzzy matching for `helm-M-x` 
+ Editing: autopair, undo-tree, highlight-parentheses
+ Python: elpy, ein, company enabled
+ Latex: auctex, latex-preview-pane, gscholar-bibtex
+ Other modes: gnuplot-mode, yaml-mode, markdown-mode

## Some keybindings
+ <kbd>M-x</kbd>: `helm-m-x`
+ <kbd>C-o</kbd>: `occur`
+ <kbd>f5</kbd>: `compile` (looking for a `Makefile` in the current folder or layers up)
+ <kbd>f7</kbd>/<kbd>C-f7</kbd>: `winner-undo`/`winner-redo`
+ <kbd>f8</kbd>: `treemacs`
+ <kbd>S-f8</kbd>: `centaur-tabs`
+ <kbd>C-=</kbd>/<kbd>C-M-=</kbd>: `previous-multiframe-window` (cycles between both frames and windows)


## Screenshots

![gui_mode](img/img_example_gui.png)
