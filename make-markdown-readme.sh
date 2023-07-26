#! /usr/bin/bash
cat README.tex | pandoc -r latex -w markdown_strict | dos2unix | sed -re '/\<Parchment\>/s/Parchment/rgb(231,211,178)/' | cat README-prelude.md - > README.md

