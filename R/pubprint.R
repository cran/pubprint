#############################################################################
# pubprint.R
#############################################################################

#' pubprint: Printing Results of Statistical Computing In A Publishable Way
#'
#' Pubprint is an extension for the R programming language. This package takes
#' the output of several statistical tests, collects the characteristic values
#' and transforms it in a publish-friendly pattern. Currently only the APA
#' (American Psychological Association) style is supported with output to
#' HTML, LaTeX, Markdown and plain text.  The pubprint package is easily
#' customizable, extendable and can be used well with
#' \code{\link[knitr]{knitr}}. Additionally pubprint offers a memory system
#' that allows to save and retrieve results of computations.
#'
#' The Markdown output is in \href{http://pandoc.org/}{pandoc} flavour and
#' HTML output complies to \href{https://www.w3.org/TR/MathML/}{MathML}. If
#' pubprint is used in a document that is processed by knitr, output format
#' will be automatically determined.
#' 
#' @docType package
#' @name pubprint-package
#' @import stats utils
#' @author Rudolf Siegel <\url{rs.os@t-online.de}>
#' @seealso See \code{\link{pprint}} in this package for the core function.
NULL
