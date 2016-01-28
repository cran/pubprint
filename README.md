pubprint
===========

Pubprint is an extension for the R programming language. This package takes
the output of several statistical tests, collects the characteristic values
and transforms it in a publish-friendly pattern. Currently only the APA style
is supported with output to HTML, LaTeX, Markdown and plain text. The
pubprint package is easily extendable and can be used well with
[knitr](http://yihui.name/knitr/).

The package is still in alpha stage. This means that it should work reliable
but that the interface of the functions may be subject to change.

Installation
------------

### CRAN, stable versions

Using [CRAN](https://cran.r-project.org/) installation is possible with a
simple
```
install.packages("pubprint")
```
in a R session.

### Bitbucket, stable versions

Download the package from
[here](https://bitbucket.org/mutluyum/pubprint/downloads)
and install it in a Linux/Unix shell:
```
R CMD INSTALL pubprint_0.1.1.tar.gz
```

### Bitbucket, development versions

Just download the current repository or get a copy with
[Mercurial](https://www.mercurial-scm.org/) from
[here](https://bitbucket.org/mutluyum/pubprint) and execute the makefile in
the source directory:
```
hg clone https://bitbucket.org/mutluyum/pubprint
cd pubprint/
make
```

Documentation
-------------

You can find some documentation in the package documentation, later there will
be an more in detail documentation with example files as well.

Requirements
------------

### R
 - check DESCRIPTION file.

### LaTeX
 - The `amsmath` package may be required if text is set above or below another
   text (see `out.above()` and `out.below()`).

Bugs and missing features
-------------------------

Of course, there aren't any bugs in the pubprint package. But you may
encounter unexpected behaviour, missing features or cryptic error messages. If
so, please report it to the [issue
tracker](https://bitbucket.org/mutluyum/pubprint/issues). Thank you! 

Contribution
------------

Any contribution to pubprint is appreciated! Have a look at [the
development page](https://bitbucket.org/mutluyum/pubprint). Please notice
that the package is published under the BSD 2-clause licence and you confirm
it by contributing.
