(**Note**: this version was automatically converted through
[Pandoc](https://pandoc.org/).
[Click here](https://docs.google.com/viewer?url=https://raw.githubusercontent.com/CubOfJudahsLion/fountain2latex/master/README.pdf) for the original.)



`fountain2latex` is a simple utility to convert from the
[`.fountain`](https://fountain.io/) screenplay format to the
[LaTeX](https://www.latex-project.org/) screenplay subformat. LaTeX
provides world-class typesetting quality for that extra bit of class and
professionalism in your script.

To be more precise, this utility specifically relies on the *[screenplay
LaTeX class](https://www.ctan.org/pkg/screenplay)* by [John
Pate](http://dvc.org.uk/johnny.html), which implements
Academy-recommended rules. It can be found in the
[TeXLive](https://tug.org/texlive/) and [MiKTeX](https://miktex.org/)
distributions, both of which offer packages for Linux, Windows and Mac.

`fountain2latex` is written in [Haskell](https://haskell.org) and
compiled with GHC version 9.2.8.

# Why?

I find LaTeX to be more obtrusive to my writing flow. Fountain barely
requires an extra character here or there. Having to think about the
`\command{}` I need to properly format something is disrupting. Then
again, I want the sweet, sweet typesetting…

There’s gotta be a way to have my cake and eat it too.

“*But Cub, you beautiful, daunting force of nature*”, I hear you say,
“*there are already ways to convert fountain to other formats*”.

I know, but I’m old school (no duh. I’m actually using *make* here.)
Something this simple should be just a few keystrokes in your terminal
and that’s it. Zero leak risk.

Now where did I leave that gosh-darned Bengay.

# Installing from release

Download the latest release and extract all the files to any directory.
Run by typing  
  
`    `<span style="background-color: Black"><span style="color: White">`fountain2latex `*`<input>`*`[.fountain] [`*`<output>`*`[.tex]]`</span></span>  
  
Extensions can be omitted, and the result will be sent to standard
output if the second filename is not provided.

# Installing from sources

Needless to say, you need GHC 9.2.8+ to do this. If you don’t have it,
you can always install from release, as shown in the previous section.

Just `cd` to the `fountain2latex` directory and run:  
  
`    `<span style="background-color: Black"><span style="color: White">`make install`</span></span>  
  
Which should take care of everything, including overwriting older
versions.

# Contact

I can be reached at
[`10951848+CubOfJudahsLion@users.noreply.github.com`](mailto:10951848+CubOfJudahsLion@users.noreply.github.com).  
  
