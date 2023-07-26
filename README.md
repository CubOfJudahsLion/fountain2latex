_(**Note**: this version was automatically converted through [Pandoc](https://pandoc.org/). [Click here](https://docs.google.com/viewer?url=https://raw.githubusercontent.com/CubOfJudahsLion/fountain2latex/master/README.pdf) for the original.)_



`fountain2latex` is a simple utility to convert from the
<span style="color: Blue">[`.fountain`](https://fountain.io/)</span>
screenplay format to the
<span style="color: Blue">[LaTeX](https://www.latex-project.org/)</span>
screenplay subformat. LaTeX provides superior typesetting for
professional-looking documents.

This utility relies on the <span style="color: Blue">[screenplay LaTeX
class](https://www.ctan.org/pkg/screenplay)</span> and the
<span style="color: Blue">[`screenplay-pkg`
package](https://www.ctan.org/pkg/screenplay-pkg)</span>, both by
<span style="color: Blue">[John
Pate](http://dvc.org.uk/johnny.html)</span>. The former implements a
`document class` with Academy-recommended rules and macros; the latter
creates an `enviroment` which allows inclusion of screenplay-formatted
sections in larger documents. They’re both available in the
<span style="color: Blue">[TeXLive](https://tug.org/texlive/)</span> and
<span style="color: Blue">[MiKTeX](https://miktex.org/)</span>
distributions, both of which offer packages for Linux, Windows and Mac.

`fountain2latex` is written in
<span style="color: Blue">[**Haskell**](https://haskell.org)</span> and
compiled with <span class="sans-serif">GHC</span> version 9.2.8.

# Using `fountain2latex`

When it’s installed (and in your path), `fountain2latex` can be invoked
in several ways. *Legend*: braces (`{}`) mean you must select one of the
alternatives separated by vertical bars (`|`). Brackets (`[]`) mean the
alternatives are optional.

-   To display the version:  
      
    <span style="background-color: Black"><span style="color: White">`fountain2latex {-v|--version}`</span></span>  

-   To see basic usage instructions:  
      
    <span style="background-color: Black"><span style="color: White">`fountain2latex {-u|--usage}`</span></span>  

-   To get a (somewhat) more comprehensive help text:  
      
    <span style="background-color: Black"><span style="color: White">`fountain2latex {-h|--help}`</span></span>  

-   To convert standard input into standard output:  
      
    <span style="background-color: Black"><span style="color: White">`fountain2latex [-p|--as-part]`</span></span>  
      
    The argument `-p` (or its equivalent longer variant, `--as-part`)
    instructs `fountain2latex` not to generate a standalone document,
    but to emit a document fragment that can be included in another. The
    master document must use `screenplay-pkg`:  
      
    <span style="background-color: rgb(231,211,178)"><span style="color: Black">`\usepackage{screenplay-pkg}`</span></span>  

-   To convert a file to standard output:  
      
    <span style="background-color: Black"><span style="color: White">`fountain2latex [-p|--as-part] `*`<input-file>`*`[.fountain]`</span></span>  
      
    `-p` (or `--as-part`) works exactly the same as in the previous
    case. If the `.fountain` extension is omitted, `fountain2latex` will
    try to find a file with the same name as the argument; failing that,
    it will append `.fountain` to it and then try again.

-   To convert a file into another:  
      
    <span style="background-color: Black"><span style="color: White">`fountain2latex [-p|--as-part] `*`<input-file>`*`[.fountain] {.|`*`<output-file>`*`[.tex]}`</span></span>  
      
    `-p` and the optional `.fountain` extension work as stated above.
    For the output file, if the `.tex` extension is omitted, it will be
    added. If you use a period instead of an output filename, the output
    file will be named the same as the input one, with the extension
    changed to `.tex`.

More accurately, `fountain2latex` processes all options from left to
right, each option overriding previous ones.

# Why?

I find LaTeX to be more obtrusive to my writing flow, but its
typesetting is without par. Fountain barely requires an extra character
here or there, so it’s more amenable to creative flow and it’s plain
text so I can use even my favorite <span style="color: Blue">[code
editor](https://www.vim.org/)</span> (with a
<span style="color: Blue">[helper
plugin](https://github.com/kblin/vim-fountain)</span> for some extra
niceties) but no formatting magic.

That’s where `fountain2latex` comes in: a simple console application.
Just a few keystrokes in your terminal and that’s it. Zero leak risks.

# Installing from release

You know this one. Download the latest release, extract all the files to
any directory, optionally add the directory to your `$PATH` for
convenience.

# Installing from sources

Needless to say, you need GHC 9.2.8+ to do this. If you don’t have it,
you can always install from release, as shown in the previous section.

Just `cd` to the `fountain2latex` directory and run:  
  
<span style="background-color: Black"><span style="color: White">`make install`</span></span>  
  
Which should take care of everything, including overwriting older
versions.

# Contact

I can be reached at
<span style="color: Blue">[`10951848+CubOfJudahsLion@users.noreply.github.com`](mailto:10951848+CubOfJudahsLion@users.noreply.github.com)</span>.  
