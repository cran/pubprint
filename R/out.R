#############################################################################
# out.R
#############################################################################

#' @include out_latex.R
#' @include out_html.R
#' @include out_markdown.R
#' @include out_plain.R
#' @include defaults.R
NULL

#########
# initialise function
#########

#' Setting the output format functions
#'
#' A list is returned which contains all the default functions for the
#' specified output format.
#'
#' @param x a character indicating the output format.
#'
#' @seealso See \code{\link{pp_opts_out}} to get and set all currently used
#' output format functions.
#'
#' @examples
#' pp_opts_out$set(pp_init_out())
#' pp_opts_out$set(pp_init_out("html"))
#' 
#' @export
pp_init_out <- function(x = c("latex", "markdown", "html", "plain"))
{
    x <- match.arg(x)

    ret <- switch(EXPR = x,
                  "latex" = out.latex.init(),
                  "markdown" = out.markdown.init(),
                  "html" = out.html.init(),
                  "plain" = out.plain.init())

    return(ret)
}

#########
# calling functions
#########
out.names <- function(x)
{
    pp_opts_out$get("names")(x = x)
}

out.specialchar <- function(x)
{
    pp_opts_out$get("specialchar")(x = x)
}

out.math <- function(..., mmode = TRUE)
{
    pp_opts_out$get("math")(..., mmode = mmode)
}

out.operator <- function(x)
{
    pp_opts_out$get("operator")(x=x)
}

out.number <- function(x, 
                       nsmall=pp_opts$get("nsmall"),
                       leading0=TRUE,
                       ...)
{
    pp_opts_out$get("number")(x=x,
                              nsmall=nsmall,
                              leading0=leading0,
                              ...)
}

out.identifier <- function(x)
{
    pp_opts_out$get("identifier")(x=x)
}

out.term <- function(x)
{
    pp_opts_out$get("term")(x=x)
}

out.concat <- function(..., sep = pp_opts$get("delimiter"))
{
    pp_opts_out$get("concat")(..., sep = sep)
}

# x_y
out.subscript <- function(x, y)
{
    pp_opts_out$get("subscript")(x = x,
                                 y = y)
}

# x^y
out.superscript <- function(x, y)
{
    pp_opts_out$get("superscript")(x = x,
                                   y = y)
}

out.bracket <- function(x, 
                        brackets = pp_opts$get("brackets"),
                        inmmode = TRUE)
{
    pp_opts_out$get("bracket")(x=x, 
                               brackets=brackets,
                               inmmode=inmmode)
}

out.above <- function(x, y)
{
    pp_opts_out$get("above")(x = x, y = y)
}

out.below <- function(x, y)
{
    pp_opts_out$get("below")(x = x, y = y)
}
