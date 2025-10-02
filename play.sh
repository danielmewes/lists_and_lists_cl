#!/bin/sh
# Launcher script for Lists and Lists

if command -v sbcl &> /dev/null; then
    sbcl --noinform --load lists-and-lists.lisp --eval "(lists-and-lists:play-game)"
elif command -v ccl &> /dev/null; then
    ccl --load lists-and-lists.lisp --eval "(lists-and-lists:play-game)"
elif command -v clisp &> /dev/null; then
    clisp -q -i lists-and-lists.lisp -x "(lists-and-lists:play-game)"
else
    echo "No Common Lisp implementation found."
    echo "Please install SBCL, CCL, or CLISP."
    exit 1
fi
