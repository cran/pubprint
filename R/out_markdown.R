#############################################################################
# out_markdown.R
#############################################################################

#' @include out_default.R
#' @include out_latex.R
NULL

out.markdown.init <- function()
{
    return(list(specialchar = out.latex.specialchar,
                math = out.markdown.math,
                operator = out.default.operator,
                number = out.default.number,
                identifier = out.default.identifier,
                term = out.default.term,
                concat = out.default.concat,
                subscript = out.latex.subscript,
                superscript = out.latex.superscript,
                bracket = out.default.bracket,
                above = out.latex.above,
                below = out.latex.below))
}

out.markdown.math <- function(..., mmode)
{
    if (mmode)
        return(paste0("$", ..., "$"))
    else
        return(paste0(...))
}
