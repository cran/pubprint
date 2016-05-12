#############################################################################
# out_default.R
#############################################################################

#' @include utils.R
NULL

out.default.specialchar <- function(x)
{
    return(x)
}

out.default.math <- function(..., mmode)
{
    if (missing(mmode))
        stop("argument \"mmode\" is missing, with no default")

    paste0(...)
}

out.default.operator <- function(x)
{
    paste0(out.specialchar(x))
}

out.default.number <- function(x,
                               nsmall,
                               leading0,
                               ...)
{
    ret <- format(round(x, nsmall), 
                  nsmall = nsmall, 
                  trim = TRUE,
                  ...)

    if (!leading0) 
        ret <- sub("^(-?)0\\.", "\\1\\.", ret)

    return(ret)
}

out.default.identifier <- function(x)
{
    return(x)
}

out.default.term <- function(x)
{
    return(x)
}

out.default.concat <- function(..., sep)
{
    return(paste(..., sep = sep, collapse = sep))
}

# x_y
out.default.subscript <- function(x, y)
{
    return(paste0(x, "_", y))
}

# x^y
out.default.superscript <- function(x, y)
{
    return(paste0(x, "^", y))
}

# really ugly, but it works ...
out.default.bracket <- function(x, brackets, inmmode)
{
    if (length(brackets) %% 2 != 0 && length(brackets) != 1)
        stop("Argument brackets must be length one or a multiple of two.")

    if (1 == length(brackets))
        return(stringr::str_c(brackets, x, brackets))

    x <- strsplit(x, split="")
    ret <- c()

    for (i in x)
    {
        reti <- c()
        depth <- 1L

        for (c in i)
        {
            if (c == brackets[depth %% length(brackets)])
            {
                depth <- depth + 2L
                reti <- stringr::str_c(reti, brackets[depth %% length(brackets)])
            }
            else if (depth != 1 && c == brackets[depth - 1L])
            {
                reti <- stringr::str_c(reti, brackets[depth %% length(brackets) + 1L])
                depth <- depth - 2L
            }
            else
                reti <- stringr::str_c(reti, c)
        }

        ret <- c(ret, stringr::str_c(brackets[1], reti, brackets[2]))
    }

    return(ret)
}

out.default.above <- function(x, y)
{
    paste0(x, y)
}

out.default.below <- function(x, y)
{
    paste0(x, y)
}
