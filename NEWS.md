Changelog
===========

This file lists only changes that affect user behaviour and bugs reported to
the [issue tracker][https://bitbucket.org/mutluyum/pubprint/issues/].
See [commit list][https://bitbucket.org/mutluyum/pubprint/commits/] for more
details.

Version 0.2.1 (release date: 24.05.2016)
----------------------------------------

- Fixed wrong output in html brackets.
- Corrected date and version in vignette.

Version 0.2 (release date: 11.05.2016)
--------------------------------------

- Added documentation for all internal style functions (style.apa.).
- Added style.apa.bartlett
- Fixed style.apa.summary.aovlist
- Improved style.apa.character/numeric functions
- Removed global options `leading0`, `replace0` and `drop0trailing`. This is
  now set by publication style.
- Added support for `anova()`
- Added support for `summary.aov()`
- Fixed warning in pprint (only first argument was checked in if clause)
- Renamed `style.apa.ks.test` to `style.apa.ks`
- Renamed `style.apa.chisquared` to `style.apa.chisq` and added argument
  `print.n` (logical) and renamed argument `chi.n.name` to `n.name`
- Added support for Kendalls and Spearmans correlation coefficient (and
  renamed `style.apa.pearson` to `style.apa.cor.test`)
- Fixing mistake in out.value: a smaller sign was printed even without the
  option `replace0`
- Fixing printing of beta values in regression: greek letter beta is the
  population coefficient, standardized parameter is only "beta"
- Detect automatically output format if pubprint is used with knitr
- Changed global option `delimiter` to `","`
- Moved to pandoc Markdown flavour and MathML for HTML output
- Add `toClipboard` function
- Support HTML above and below notation
- Remove `print.d` option in `style.apa.t.test` (d is printed when second
  argument is given ...)
- Splitted style.apa.summary.lm into three functions (.model, .coeff,
  .equation)
- Fixing [issue #2][#2]
- Fixing [issue #8][#8]
- Fixing [issue #10][#10]

[#2]: https://bitbucket.org/mutluyum/pubprint/issues/2
[#8]: https://bitbucket.org/mutluyum/pubprint/issues/8
[#10]: https://bitbucket.org/mutluyum/pubprint/issues/10

Version 0.1.1 (release date: 28.01.2016)
----------------------------------------

- bug fix release to comply CRAN policy

Version 0.1 (release date: 26.01.2016)
--------------------------------------

This release is intended to stabilize the function calls and general program
flow. It is not thought to provide as many styles and outputs as possible.

- Initial release
- Fixing [issue #1][#1]
- Fixing [issue #5][#5]
- Fixing [issue #6][#6]
- Fixing [issue #7][#7]

[#1]: https://bitbucket.org/mutluyum/pubprint/issues/1
[#5]: https://bitbucket.org/mutluyum/pubprint/issues/5
[#6]: https://bitbucket.org/mutluyum/pubprint/issues/6
[#7]: https://bitbucket.org/mutluyum/pubprint/issues/7
