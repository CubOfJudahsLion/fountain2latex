#! /usr/bin/bash
sed -re '/^\\newcommand\{/d; /Click the back arrow/d; /\<link\>/s/\\link\>/\\href/g' README.tex | pandoc -r latex -w markdown_strict | dos2unix | cat README-prelude.md - > README.md

