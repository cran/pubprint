#############################################################################
# out_html.R
#############################################################################

#' @include out_default.R
NULL

out.html.init <- function()
{
    return(list(specialchar = out.html.specialchar,
                math = out.html.math,
                operator = out.html.operator,
                number = out.html.number,
                identifier = out.html.identifier,
                term = out.html.term,
                concat = out.html.concat,
                subscript = out.html.subscript,
                superscript = out.html.superscript,
                bracket = out.html.bracket,
                above = out.html.above,
                below = out.html.below))
}

out.html.math <- function(..., mmode)
{
    if (mmode)
        return(paste0("<math xmlns=\"&mathml;\">", ..., "</math>"))
    else
        return(paste0(...))
}

out.html.specialchar <- function(x)
{
    myspec <- c("<" = "&lt;",
                ">" = "&gt;",
                "alpha" = "&alpha;",
                "beta" = "&beta;",
                "gamma" = "&gamma;",
                "delta" = "&delta;",
                "epsilon" = "&epsilon;",
                "zeta" = "&zeta;",
                "eta" = "&eta;",
                "theta" = "&theta;",
                "iota" = "&iota;",
                "kappa" = "&kappa;",
                "lambda" = "&lambda;",
                "mu" = "&mu;",
                "nu" = "&nu;",
                "xi" = "&xi;",
                "omikron" = "&omikron;",
                "pi" = "&pi;",
                "rho" = "&rho;",
                "sigma" = "&sigma;",
                "tau" = "&tau;",
                "upsilon" = "&upsilon;",
                "phi" = "&phi;",
                "chi" = "&chi;",
                "psi" = "&psi;",
                "omega" = "&omega;",
                "Alpha" = "&Alpha;",
                "Beta" = "&Beta;",
                "Gamma" = "&Gamma;",
                "Delta" = "&Delta;",
                "Epsilon" = "&Epsilon;",
                "Zeta" = "&Zeta;",
                "Eta" = "&Eta;",
                "Theta" = "&Theta;",
                "Iota" = "&Iota;",
                "Kappa" = "&Kappa;",
                "Lambda" = "&Lambda;",
                "Mu" = "&Mu;",
                "Nu" = "&Nu;",
                "Xi" = "&Xi;",
                "Omikron" = "&Omikron;",
                "Pi" = "&Pi;",
                "Rho" = "&Rho;",
                "Sigma" = "&Sigma;",
                "Tau" = "&Tau;",
                "Upsilon" = "&Upsilon;",
                "Phi" = "&Phi;",
                "Chi" = "&Chi;",
                "Psi" = "&Psi;",
                "Omega" = "&Omega;")
    
    return(utils.symbols.replace(x, replacements = myspec))
}

out.html.operator <- function(x)
{
    paste0("<mo>",
           out.default.operator(x),
           "</mo>")
}

out.html.number <- function(x,
                            nsmall,
                            leading0,
                            ...)
{
    ret <- out.default.number(x=x,
                              nsmall=nsmall,
                              leading0=leading0,
                              ...)

    ret <- paste0("<mn>",
                  ret,
                  "</mn>")

    return(ret)
}

out.html.identifier <- function(x)
{
    paste0("<mi>",
           out.default.identifier(x),
           "</mi>")
}

out.html.term <- function(x)
{
    paste0("<mrow>",
           out.default.term(x),
           "</mrow>")
}

out.html.concat <- function(..., sep)
{
    return(paste0("<mfenced open=\"\" close=\"\" separators=\"",
                  sep,
                  "\">",
                  paste0(..., collapse = ""),
                  "</mfenced>"))
}

# x_y
out.html.subscript <- function(x, y)
{
    return(paste0("<msub>", 
                  x,
                  y,
                  "</msub>"))
}

# x^y
out.html.superscript <- function(x, y)
{
    return(paste0("<msup>", 
                  x,
                  y,
                  "</msup>"))
}

# really ugly, but it works ...
out.html.bracket <- function(x, brackets, inmmode = TRUE)
{
    if (length(brackets) %% 2 != 0 && length(brackets) != 1)
        stop("Argument brackets must be length one or a multiple of two.")

    if (!inmmode)
        return(stringr::str_c(brackets, x, brackets))

    if (1 == length(brackets))
        return(stringr::str_c("<mfenced open=\"",
                              brackets, 
                              "\" close=\"",
                              brackets,
                              "\">",
                              x, 
                              "</mfenced>"))

    x <- stringr::str_split(x, stringr::fixed("<mfenced"))
    ret <- c()

    for (item in x)
    {
        reti <- c()
        depth <- -1L

        for (string in item)
        {
            # no replacement in first string (input x must not start with a
            # bracket)
            if (-1L != depth)
            {
                string <- gsub(brackets[depth], 
                               brackets[(depth + 2) %% length(brackets)], 
                               string, 
                               fixed=TRUE)
                string <- gsub(brackets[depth + 1], 
                               brackets[(depth + 2) %% length(brackets) + 1], 
                               string, 
                               fixed=TRUE)

                reti <- stringr::str_c(reti, "<mfenced", string)
            }
            else
                reti <- stringr::str_c(reti, string)

            depth <- depth + 2L - (2 * stringr::str_count(string, "</mfenced>"))
        }

        ret <- c(ret, 
                 stringr::str_c("<mfenced open=\"", 
                                brackets[1], "\" close=\"", 
                                brackets[2], 
                                "\">",
                                reti,
                                "</mfenced>"))
    }

    return(ret)
}

# y over x
out.html.above <- function(x, y)
{
    return(paste0("<mover>",
                  x, y,
                  "</mover>"))
}

# y below x
out.html.below <- function(x, y)
{
    return(paste0("<munder>",
                  x, y,
                  "</munder>"))
}
