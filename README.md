# fountain2latex

`fountain2latex` is a simple utility to convert from the
[_`.fountain`_ screenplay format](https://fountain.io/) to [LaTeX](https://www.latex-project.org/).

More precisely, it relies on the
[_screenplay_ LaTeX class](https://www.ctan.org/pkg/screenplay) by John Pate,
which implements Academy-recommended rules. It can be found in the
[TeXLive](https://tug.org/texlive/) (Linux/Windows/Mac) and
[MiKTeX](https://miktex.org/) (Windows) distributions.

`fountain2latex` is written in [Haskell](https://haskell.org) and compiled
with GHC version 9.2.8.


## Installing from release

Download the latest release and extract all the files to any directory.
Run by typing

    fountain2latex <input>[.fountain] [<output>[.tex]]

Extensions can be omitted, and the output name will be the same as the
input if missing. When rendered through LaTeX, the result will be a
professional-quality screenplay.


## Installing from sources

Needless to say, you need GHC 9.2.8+ to do this. If you don't have it,
you can always install from release, as shown in the previous section.

Just `cd` to the `fountain2latex` directory and run:

    make install

Which should take care of everything, overwriting older versions if
present.

## Contact

I can be reached at  [10951848+CubOfJudahsLion@users.noreply.github.com](mailto:10951848+CubOfJudahsLion@users.noreply.github.com).

