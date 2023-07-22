sed -re '/^\\newcommand\{/d; s/\\link\>/\\href/g' README.tex | pandoc -r latex -w markdown | sed -re 's/\s*\{#[^}]*\}//g; s/\\$//' > README.md
