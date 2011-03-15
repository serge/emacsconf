#!/bin/bash
echo "WARNING: Must be launched from the same directory of emacs-main.el" 
echo ";; call .emacs from source control" > ~/.emacs
echo "(add-to-list 'load-path \"`echo $PWD | sed -E -e 's@'$HOME'(.*)@~\1@g'`\")" >> ~/.emacs
echo "(load \"emacs-main\")" >> ~/.emacs


