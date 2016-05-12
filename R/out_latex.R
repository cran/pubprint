#############################################################################
# out_latex.R
#############################################################################

#' @include out_default.R
#' @include utils.R
NULL

out.latex.init <- function()
{
    return(list(specialchar = out.latex.specialchar,
                math = out.latex.math,
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

out.latex.specialchar <- function(x)
{
    myspec <- c("<" = "\\ifmmode<\\else\\textless\\fi",
                ">" = "\\ifmmode<\\else\\textgreater\\fi",
                "alpha" = "\\ifmmode\\alpha\\else\\textalpha\\fi",
                "beta" = "\\ifmmode\\beta\\else\\textbeta\\fi",
                "gamma" = "\\ifmmode\\gamma\\else\\textgamma\\fi",
                "delta" = "\\ifmmode\\delta\\else\\textdelta\\fi",
                "epsilon" = "\\ifmmode\\epsilon\\else\\straightepsilon\\fi",
                "zeta" = "\\ifmmode\\zeta\\else\\textzeta\\fi",
                "eta" = "\\ifmmode\\eta\\else\\texteta\\fi",
                "theta" = "\\ifmmode\\theta\\else\\straighttheta\\fi",
                "iota" = "\\ifmmode\\iota\\else\\textiota\\fi",
                "kappa" = "\\ifmmode\\kappa\\else\\textkappa\\fi",
                "lambda" = "\\ifmmode\\lambda\\else\\textlambda\\fi",
                "mu" = "\\ifmmode\\mu\\else\\textmugreek\\fi",
                "nu" = "\\ifmmode\\nu\\else\\textnu\\fi",
                "xi" = "\\ifmmode\\xi\\else\\textxi\\fi",
                "omikron" = "\\ifmmode\\o\\else\\textomikron\\fi",
                "pi" = "\\ifmmode\\pi\\else\\textpi\\fi",
                "rho" = "\\ifmmode\\rho\\else\\textrho\\fi",
                "sigma" = "\\ifmmode\\sigma\\else\\textsigma\\fi",
                "tau" = "\\ifmmode\\tau\\else\\texttau\\fi",
                "upsilon" = "\\ifmmode\\upsilon\\else\\textupsilon\\fi",
                "phi" = "\\ifmmode\\phi\\else\\textphi\\fi",
                "chi" = "\\ifmmode\\chi\\else\\textchi\\fi",
                "psi" = "\\ifmmode\\psi\\else\\textpsi\\fi",
                "omega" = "\\ifmmode\\omega\\else\\textomega\\fi",
                "Alpha" = "\\ifmmode\\Alpha\\else\\textAlpha\\fi",
                "Beta" = "\\ifmmode\\Beta\\else\\textBeta\\fi",
                "Gamma" = "\\ifmmode\\Gamma\\else\\textGamma\\fi",
                "Delta" = "\\ifmmode\\Delta\\else\\textDelta\\fi",
                "Epsilon" = "\\ifmmode\\Epsilon\\else\\textEpsilon\\fi",
                "Zeta" = "\\ifmmode\\Zeta\\else\\textZeta\\fi",
                "Eta" = "\\ifmmode\\Eta\\else\\texTeta\\fi",
                "Theta" = "\\ifmmode\\Theta\\else\\textTheta\\fi",
                "Iota" = "\\ifmmode\\Iota\\else\\textIota\\fi",
                "Kappa" = "\\ifmmode\\Kappa\\else\\textKappa\\fi",
                "Lambda" = "\\ifmmode\\Lambda\\else\\textLambda\\fi",
                "Mu" = "\\ifmmode\\Mu\\else\\textMugreek\\fi",
                "Nu" = "\\ifmmode\\Nu\\else\\textNu\\fi",
                "Xi" = "\\ifmmode\\Xi\\else\\textXi\\fi",
                "Omikron" = "\\ifmmode\\O\\else\\textOmikron\\fi",
                "Pi" = "\\ifmmode\\Pi\\else\\textPi\\fi",
                "Rho" = "\\ifmmode\\Rho\\else\\textRho\\fi",
                "Sigma" = "\\ifmmode\\Sigma\\else\\textSigma\\fi",
                "Tau" = "\\ifmmode\\Tau\\else\\textTau\\fi",
                "Upsilon" = "\\ifmmode\\Upsilon\\else\\textUpsilon\\fi",
                "Phi" = "\\ifmmode\\Phi\\else\\textPhi\\fi",
                "Chi" = "\\ifmmode\\Chi\\else\\textChi\\fi",
                "Psi" = "\\ifmmode\\Psi\\else\\textPsi\\fi",
                "Omega" = "\\ifmmode\\Omega\\else\\textOmega\\fi")
                #"varepsilon" = "\\ifmmode\\varepsilon\\else\\textepsilon\\fi",
                #"vartheta" = "\\ifmmode\\vartheta\\else\\texttheta\\fi",
                #"varrho" = "\\ifmmode\\varrho\\else\\???\\fi",
                #"varphi" = "\\ifmmode\\varphi\\else\\???\\fi")

    return(utils.symbols.replace(x, replacements = myspec))
}

out.latex.math <- function(..., mmode)
{
    if (mmode)
        return(paste0("\\ensuremath{", ..., "}"))
    else
        return(paste0(...))
}

# x_y
out.latex.subscript <- function(x, y)
{
    return(paste0(x,
                  "\\ifmmode_{", 
                  y, 
                  "}\\else\\textsubscript{", 
                  y,
                  "}\\fi"))
}

out.latex.superscript <- function(x, y)
{
    return(paste0(x,
                  "\\ifmmode^{", 
                  y, 
                  "}\\else\\textsuperscript{", 
                  y,
                  "}\\fi"))
}

# requires amsmath package
# y over x
out.latex.above <- function(x, y)
{
    if ("^" == y)
        return(paste0("\\ifmmode\\hat{",
                      x, 
                      "}\\else ",
                      x,
                      "\\textsuperscript{\\textasciicircum}\\fi"))
    else 
        return(paste0("\\ifmmode\\overset{",
                      y, 
                      "}{", 
                      x,
                      "}\\else ",
                      x,
                      "\\textsuperscript{",
                      y,
                      "}\\fi"))
}

# requires amsmath package
# y below x
out.latex.below <- function(x, y)
{
    return(paste0("\\ifmmode\\underset{",
                  y, 
                  "}{", 
                  x,
                  "}\\else ",
                  x,
                  "\\textsubscript{",
                  y,
                  "}\\fi"))
}
