#############################################################################
# style_apa.R
#############################################################################

#' @include style_default.R
#' @include utils.R
NULL

#########
# initialise function
#########
style.apa.init <- function()
{
    return(list(character = style.apa.character,
                numeric = style.apa.numeric,
                p.value = style.apa.p.value,
                df = style.apa.df,
                t.test = style.apa.t.test,
                fisher = style.apa.fisher,
                cor.test = style.apa.cor.test,
                chisq = style.apa.chisq,
                shapiro = style.apa.shapiro,
                bartlett = style.apa.bartlett,
                ks = style.apa.ks,
                anova = style.apa.anova,
                summary.aov = style.apa.summary.aovlist,
                summary.aovlist = style.apa.summary.aovlist,
                summary.lm.model = style.apa.summary.lm.model,
                summary.lm.equation = style.apa.summary.lm.equation,
                summary.lm.coeff = style.apa.summary.lm.coeff,
                summary.lm.beta.coeff = style.apa.summary.lm.beta.coeff))
}

#########
# working functions
#########

#' Formats a character vector
#'
#' This style functions takes character vectors in a list and returns it
#' unlisted. This is mainly for putting math mode and brackets around the
#' character vector using \code{\link{pprint}}, but may be used to replace
#' identifier names in character vectors as well.
#'
#' @template internal_style
#'
#' @param x a list. All list items have to be character vectors.
#' 
#' @param as.identifier a logical. If \code{TRUE} character vector is handled
#' as identifiers (this is mainly for special treatment in HTML output mode).
#'
#' @param replace a logical, specifying whether identifiers are replaced with
#' more proper names (e.g. \code{R^2} will be written in proper output format
#' or \code{cor} is replaced with \code{r}).
#'
#' @family APA style functions
#'
#' @return character vector.
#'
#' @examples
#' pprint(c("test 1", "test 2"), format="character")
style.apa.character <- function(x,
                                as.identifier=FALSE,
                                replace=TRUE)
{
    myspec <- c("cor" = out.identifier("r"), 
                "D^-" = out.superscript(out.identifier("D"),
                                        out.operator("-")),
                "X-squared" =
                    out.superscript(out.identifier(out.specialchar("chi")),
                                    out.number(2L, nsmall=0)), 
                "Bartlett's K-squared" =
                    out.superscript(out.identifier(out.specialchar("chi")),
                                    out.number(2L, nsmall=0)), 
                "Friedman chi-squared" =
                    out.superscript(out.identifier(out.specialchar("chi")),
                                    out.number(2L, nsmall=0)), 
                "mean of x" = out.subscript(out.identifier("M"),
                                            out.identifier("x")), 
                "mean of y" = out.subscript(out.identifier("M"),
                                            out.identifier("y")), 
                "tau" = out.subscript(out.identifier("r"),
                                      out.identifier(out.specialchar("tau"))),
                "rho" = out.subscript(out.identifier("r"), out.identifier("S")), 
                "R^2" = out.superscript(out.identifier("R"), out.number(2L, nsmall=0)),
                "F value" = out.identifier("F"))

    x <- x[[1]]

    if (!as.identifier)
        return(x)

    if (replace)
    {
        replaced <- x %in% names(myspec)
        x <- utils.symbols.replace(x, replacements=myspec)
        x[!replaced] <- out.identifier(x[!replaced])
    }
    else
        out.identifier(x)

    if (!length(x))
        x <- NULL

    return(x)
}


#' Formats a numeric vector
#'
#' This style functions takes a numeric vector in a list and returns a
#' formatted numeric vector. Additional arguments can be given specifying how
#' to format the numeric vector.
#'
#' @template internal_style
#'
#' @param x a list. First list item has to be a numeric vector. All other
#' items are ignored.
#' 
#' @param name a character. Giving the identifier name of the numeric (like in
#' \code{x = 12}).
#'
#' @param inbracket a character. Giving additional information after the
#' identifier, like a degree of freedom (e.g. \code{t(12) = 3.42}).
#'
#' @param nsmall a integer. The minimum number of digits to the right of the
#' decimal point in formatting real/complex numbers in non-scientific formats.
#'
#' @param replace0 a boolean. Controls whether numbers that are absolute
#' smaller than \code{1/(10^nsmall)} are replaced with this term.  For example
#' \eqn{p = .0001} with \eqn{p < .001}. Pay attention that zero is replaced as
#' well.
#'
#' @param leading0 a boolean, specifying whether a zero before the decimal
#' point is printed.
#'
#' @param ... further arguments passed to \code{\link{format}}.
#'
#' @family APA style functions
#'
#' @return numeric vector.
#' 
#' @examples
#' pprint(c(.74895, 13.1234567), format = "numeric")
style.apa.numeric <- function(x, 
                              name,
                              inbracket,
                              nsmall = pp_opts$get("nsmall"),
                              replace0 = FALSE,
                              leading0 = TRUE,
                              ...)
{
    num <- x[[1]]
    operator <- out.operator("=")

    # replace zero with .001 ?
    if (replace0)
    {
        operator <- ifelse(abs(num) < 1/(10^nsmall), 
                           out.operator("<"), 
                           out.operator("="))
        num <- ifelse(abs(num) < 1/(10^nsmall), 
                      1/(10^nsmall) * sign(x[[1]]), 
                      num)
        num <- ifelse(num, num, 1/(10^nsmall))
    }

    num <- out.number(num,
                      nsmall,
                      leading0,
                      ...)

    if (!missing(inbracket) && !is.null(inbracket))
        name <- paste0(name, 
                       out.bracket(inbracket))

    if (!missing(name) && !is.null(name))
        num <- paste0(name, operator, num)

    num <- out.term(num)

    return(num)
}


#' Formats a p value
#'
#' This style functions takes a numeric vector in a list and transforms the
#' items into a character vector with formatted p values.  \code{replace0} is
#' set to \code{TRUE} and \code{nsmall = 3}. Leading zeros will not be
#' printed.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a numeric, that will be formatted as p
#' value. All other list items will be ignored.
#'
#' @family APA style functions
#'
#' @return character vector with formatted p values.
#' 
#' @examples
#' pprint(c(.74895, .00001), format = "p.value")
style.apa.p.value <- function(x)
{
    ret <- style.apa.numeric(list(x[[1]]),
                             name = style.apa.character(list("p"),
                                                        as.identifier=TRUE),
                             nsmall = 3L,
                             replace0 = TRUE,
                             leading0 = FALSE)

    return(ret)
}


#' Formats a degree of freedom value
#'
#' This style functions takes a numeric vector in a list and transforms the
#' items into a character vector with formatted df values. Values will not be
#' printed with trailing zeros.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a numeric, that will be formatted as df
#' value. All other list items will be ignored.
#'
#' @family APA style functions
#'
#' @return character vector with formatted df values.
#' 
#' @examples
#' pprint(c(15, 15.23456), format = "df")
style.apa.df <- function(x)
{
    ret <- style.apa.numeric(list(x[[1]]),
                             drop0trailing = TRUE)

    return(ret)
}


#' Formats a t-test
#'
#' This style functions takes a t-test in a list and transforms it into a
#' formatted character vector. Estimates are printed as well.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a t-test (htest class). Second item is
#' optional and may contain an effect size (numeric). All other list items
#' will be ignored.
#'
#' @param print.estimate a logical, indicating whether estimates are printed
#' as well.
#'
#' @param estimate.names a character vector, giving the names of the
#' estimates.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{t.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' pprint(t.test(1:30, 2:31), format = "t.test")
#' pprint(t.test(1:30, 2:31), format = "t.test", print.estimate = FALSE)
#' pprint(t.test(1:30, 2:31), 
#'        format = "t.test", 
#'        estimate.names = c("control", "treatment"))
style.apa.t.test <- function(x,
                             print.estimate = TRUE,
                             estimate.names = names(x[[1]]$estimate))
{
    ret <- c()

    # estimates
    if (print.estimate)
        ret <- style.apa.numeric(list(unname(x[[1]]$estimate)), 
                                 name = style.apa.character(list(estimate.names),
                                                            as.identifier=TRUE))

    # t value
    ret <- c(ret,
             style.apa.numeric(list(x[[1]]$statistic[[1]]),
                               name = style.apa.character(list(names(x[[1]]$statistic)),
                                                          as.identifier=TRUE),
                               inbracket = style.apa.df(list(x[[1]]$parameter))))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))

    # optional d value
    if (1 < length(x))
        ret <- c(ret,
                 style.apa.numeric(list(x[[2]]),
                                   name = style.apa.character(list("d"),
                                                              as.identifier=TRUE)))

    return(ret)
}


#' Formats a Fisher's Exact Test
#'
#' This style functions takes a Fisher's exact test in a list and transforms
#' it into a formatted character vector. Odds ratio is printed optionally.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a Fisher's exact test (htest class).
#' All other list items will be ignored.
#'
#' @param print.estimate a logical, indicating whether odds ratio is printed
#' as well.
#'
#' @param estimate.names a character vector, giving a alternative name for
#' "odds ratio".
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{fisher.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' pprint(t.test(1:30, 2:31), format = "t.test")
#' pprint(t.test(1:30, 2:31), format = "t.test", print.estimate = FALSE)
#' pprint(t.test(1:30, 2:31), 
#'        format = "t.test", 
#'        estimate.names = c("control", "treatment"))
#' TeaTasting <-
#' matrix(c(3, 1, 1, 3),
#'        nrow = 2,
#'        dimnames = list(Guess = c("Milk", "Tea"),
#'                        Truth = c("Milk", "Tea")))
#' pprint(fisher.test(TeaTasting, alternative = "greater"),
#'        format = "fisher")
#' pprint(fisher.test(TeaTasting, alternative = "greater"),
#'        format = "fisher",
#'        print.estimate = FALSE)
style.apa.fisher <- function(x,
                             print.estimate = TRUE,
                             estimate.names = names(x[[1]]$estimate))
{
    ret <- c()

    # estimates
    if (print.estimate && !is.null(x[[1]]$estimate))
        ret <- style.apa.numeric(list(unname(x[[1]]$estimate)), 
                                 name = style.apa.character(list(estimate.names),
                                                            as.identifier=TRUE))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))

    return(ret)
}


#' Formats a Correlation Between Paired Samples
#'
#' This style functions takes a correlation test (htest class) from
#' \code{\link[stats]{cor.test}} and transforms it into a formatted character
#' vector, including p value.  Statistics are printed optionally (not by
#' default).
#'
#' @template internal_style
#'
#' @param x a list. First item must be a correlation test (htest class) from
#' \code{\link[stats]{cor.test}}. All other list items will be ignored.
#'
#' @param print.statistic a logical, indicating whether test statistic is
#' printed as well. Corresponding p value is always printed.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{cor.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' a <- rnorm(100)
#' b <- rnorm(100) * .75 + a * .25
#'
#' pprint(cor.test(a, b), format = "cor.test")
#' pprint(cor.test(a, b, method = "kendall"), format = "cor.test")
#' pprint(cor.test(a, b, method = "spearman"), format = "cor.test")
#' pprint(cor.test(a, b), 
#'        format = "cor.test", 
#'        print.statistic = TRUE)
#' pprint(cor.test(a, b, method = "kendall"), 
#'        format = "cor.test", 
#'        print.statistic = TRUE)
#' pprint(cor.test(a, b, method = "spearman"), 
#'        format = "cor.test", 
#'        print.statistic = TRUE)
style.apa.cor.test <- function(x,
                               print.statistic = FALSE)
{
    inbracket <- NULL

    # estimates
    ret <- style.apa.numeric(list(unname(x[[1]]$estimate)), 
                             name = style.apa.character(list(names(x[[1]]$estimate)),
                                                        as.identifier=TRUE),
                             leading0 = FALSE)

    # t, S or z value
    if (!is.null(x[[1]]$parameter))
        inbracket = style.apa.df(list(x[[1]]$parameter))

    if (print.statistic)
        ret <- c(ret,
                 style.apa.numeric(list(x[[1]]$statistic[[1]]),
                                   name = style.apa.character(list(names(x[[1]]$statistic)),
                                                              as.identifier=TRUE),
                                   inbracket = inbracket))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))

    return(ret)
}


#' Formats a Pearson's Chi-squared Test for Count Data
#'
#' This style functions takes a chi-squared test (htest class) from
#' \code{\link[stats]{chisq.test}} and transforms it into a formatted
#' character vector. Statistics are printed optionally (not by default).
#'
#' @template internal_style
#'
#' @param x a list. First item must be a chi-squared test (htest class) from
#' \code{\link[stats]{chisq.test}}. All other list items will be ignored.
#'
#' @param print.n a logical, indicating whether number of participants is
#' printed.
#'
#' @param n.name a character, giving the name or letter for the number of
#' participants
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{chisq.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' x <- matrix(c(141,29,43,26,5,10,26,12,10), nc=3)
#' 
#' pprint(chisq.test(x), format = "chisq")
#' pprint(chisq.test(x), format = "chisq", n.name = "n")
#' pprint(chisq.test(x), format = "chisq", print.n = FALSE)
style.apa.chisq <- function(x,
                            print.n = TRUE,
                            n.name = "N")
{
    myinb <- style.apa.df(x[[1]]$parameter)

    if (print.n)
        myinb <- out.concat(myinb, 
                            style.apa.numeric(list(sum(x[[1]]$observed)),
                                              name = style.apa.character(list(n.name),
                                                                         as.identifier=TRUE),
                                              drop0trailing = TRUE))

    # chi value
    ret <- style.apa.numeric(list(x[[1]]$statistic[[1]]),
                             name = style.apa.character(list(names(x[[1]]$statistic)),
                                                        as.identifier=TRUE),
                             inbracket = myinb)

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))


    return(ret)
}


#' Formats a Shapiro-Wilk Normality Test
#'
#' This style functions takes a Shapiro-Wilk test of normality (htest class)
#' from \code{\link[stats]{shapiro.test}} and transforms it into a formatted
#' character vector.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a Shapiro-Wilk test (htest class) from
#' \code{\link[stats]{shapiro.test}}. All other list items will be ignored.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{shapiro.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' pprint(shapiro.test(rnorm(100, mean = 5, sd = 3)), 
#'        format = "shapiro")
style.apa.shapiro <- function(x)
{
    # W value
    ret <- c(style.apa.numeric(list(x[[1]]$statistic[[1]]),
                               name = style.apa.character(list(names(x[[1]]$statistic)),
                                                          as.identifier=TRUE)))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))

    return(ret)
}


#' Formats a Bartlett Test of Homogeneity of Variances
#'
#' This style functions takes a Bartlett test of homogeneity (htest class)
#' from \code{\link[stats]{bartlett.test}} and transforms it into a formatted
#' character vector.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a Bartlett test (htest class) from
#' \code{\link[stats]{bartlett.test}}. All other list items will be ignored.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{bartlett.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' pprint(bartlett.test(count ~ spray, data = InsectSprays), 
#'        format = "bartlett")
style.apa.bartlett <- function(x)
{
    # chi squared value
    ret <- style.apa.numeric(list(x[[1]]$statistic[[1]]),
                             name = style.apa.character(list(names(x[[1]]$statistic)),
                                                        as.identifier=TRUE),
                             inbracket = style.apa.df(x[[1]]$parameter))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))

    return(ret)
}


#' Formats a Kolmogorov-Smirnov-Test
#'
#' This style functions takes a Kolmogorov-Smirnow test (htest class) from
#' \code{\link[stats]{ks.test}} and transforms it into a formatted character
#' vector.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a Kolmogorv-Smirnov test (htest class)
#' from \code{\link[stats]{ks.test}}. All other list items will be ignored.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{ks.test}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' x <- rnorm(50)
#' y <- runif(30)
#' 
#' pprint(ks.test(x, y),
#'        format = "ks")
style.apa.ks <- function(x)
{
    # D^- value
    ret <- c(style.apa.numeric(list(x[[1]]$statistic[[1]]),
                               name = style.apa.character(list(names(x[[1]]$statistic)),
                                                          as.identifier=TRUE)))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x[[1]]$p.value)))

    return(ret)
}


#' Formats a Analysis of Variance Result
#'
#' This style functions takes a Anova table from \code{\link[stats]{anova}}
#' and transforms it into a formatted character vector. This function checks
#' for model comparisons.
#'
#' @template internal_style
#'
#' @param x a list. First item must be a Anova table from
#' \code{\link[stats]{anova}}. All other list items will be ignored.
#'
#' @param effect a numeric, specifying the effect or model comparison that
#' should be printed.  Please note that the default value of 1 is not
#' appropriate for model comparisons.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{anova}}
#'
#' @return character vector with a formatted character vector.
#' 
#' @examples
#' pprint(anova(lm(weight ~ Diet, data = ChickWeight)),
#'        format = "anova")
#' pprint(anova(lm(weight ~ Diet, data = ChickWeight),
#'              lm(weight ~ Diet + Time, data = ChickWeight)),
#'        format = "anova",
#'        effect = 2)
style.apa.anova <- function(x,
                            effect = 1)
{
    x <- x[[1]]
    # is there a better solution? for next two lines?
    myp <- names(x)[names(x) %in% c("Pr(>F)", "Pr(>Chi)")]
    mytest <- names(x)[names(x) %in% c("F", "F value", "Cp")]

    # test value
    if (length(mytest))
    {
        inbracket <- style.apa.df(list(x[["Df"]][effect]))

        if (is.null(x[["Res.Df"]]))
            inbracket <- out.concat(inbracket,
                                    style.apa.df(tail(x[["Df"]], 1)))
        else
            inbracket <- out.concat(inbracket,
                                    style.apa.df(x[["Res.Df"]][effect]))

        # Test statistic
        ret <- style.apa.numeric(list(x[[mytest]][effect]),
                                 name = style.apa.character(list(mytest),
                                                            as.identifier=TRUE),
                                 inbracket = inbracket)
    }

    # p value
    if (length(myp))
        ret <- c(ret,
                 style.apa.p.value(list(x[[myp]][effect])))

    return(ret)
}


#' Formats the summary of a Analysis of Variance Model
#'
#' This style functions takes the summary of a Analysis of Variance Model from
#' \code{\link[stats]{aov}} or \code{\link[stats]{manova}} and transforms it
#' into a formatted character vector.
#'
#' @template internal_style
#'
#' @param x a list. First item must be the summary of a Analysis of Variance
#' Model from \code{\link[stats]{aov}} or \code{\link[stats]{manova}}. All other list
#' items will be ignored.
#'
#' @param response a integer. This argument selects the desired response
#' variable in a \code{aovlist}. Default is appropriate for \code{aov} class.
#'
#' @param effect a integer, specifying the effect that should be printed.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{aov}}, \code{\link[stats]{manova}}
#'
#' @return character vector with a formatted character vector.
#'
#' @aliases style.apa.summary.aov
#' 
#' @examples
#' op <- options(contrasts = c("contr.helmert", "contr.poly"))
#'
#' # AOV
#' ( npk.aov <- aov(yield ~ block + N*P*K, npk) )
#' summary(npk.aov)
#' pprint(summary(npk.aov),
#'        format = "summary.aov")
#'
#' # MANOVA
#' npk2 <- within(npk, foo <- rnorm(24))
#' ( npk2.aov <- manova(cbind(yield, foo) ~ block + N*P*K, npk2) )
#' summary.aov(npk2.aov)
#' pprint(summary.aov(npk2.aov),
#'        format = "summary.aovlist")
#'
#'  options(op) # reset to previous
style.apa.summary.aovlist <- function(x,
                                      response = 1L,
                                      effect = 1L)
{
    x <- x[[1]][[response]]

    ret <- style.apa.anova(list(x), 
                           effect=effect)

    return(ret)
}


#' Formats the summary of a Linear Model Fit (model statistics)
#'
#' This style functions takes the summary of a linear model fit from
#' \code{\link[stats]{lm}} and transforms it into a formatted character
#' vector. It prints the model statistics.
#'
#' @template internal_style
#'
#' @param x a list. First item must be the summary of a linear model fit from
#' \code{\link[stats]{lm}}. All other list items will be ignored.
#'
#' @param r.squared a character. This argument selects whether the normal or
#' the adjusted R squared is printed.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{lm}}
#'
#' @return character vector with a formatted character vector.
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' pprint(summary(lm.D9),
#'        format = "summary.lm.model")
style.apa.summary.lm.model <- function(x,
                                       r.squared = c("normal", "adjusted"))
{
    r.squared = match.arg(r.squared)
    x <- x[[1L]]

    # R squared
    ret <- style.apa.numeric(list(switch(EXPR = r.squared, 
                                         "normal" = x$r.squared, 
                                         "adjusted" = x$adj.r.squared)), 
                             name = style.apa.character(list("R^2"),
                                                        as.identifier=TRUE))
    
    # F value
    ret <- c(ret,
             style.apa.numeric(list(x$fstatistic[[1L]]),
                               name = style.apa.character(list("F"),
                                                          as.identifier=TRUE),
                               # use style.apa.df function for correct
                               # df formatting
                               inbracket = out.concat(style.apa.df(x$fstatistic[[2L]]),
                                                      style.apa.df(x$fstatistic[[3L]]))))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(pf(x$fstatistic[[1L]],
                                       x$fstatistic[[2L]],
                                       x$fstatistic[[3L]],
                                       lower.tail = FALSE))))

    return(ret)
}


#' Formats the summary of a Linear Model Fit (regression equation)
#'
#' This style functions takes the summary of a linear model fit from
#' \code{\link[stats]{lm}} and transforms it into a formatted character
#' vector. It prints the regression equation.
#'
#' @template internal_style
#'
#' @param x a list. First item must be the summary of a linear model fit from
#' \code{\link[stats]{lm}}. All other list items will be ignored.
#'
#' @param respname a character. This argument specifies how the response is
#' called.
#'
#' @param coeff.name a character. This argument specifies how the coefficients
#' are named.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{lm}}
#'
#' @return character vector with a formatted character vector.
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' pprint(summary(lm.D9),
#'        format = "summary.lm.equation")
style.apa.summary.lm.equation <- function(x,
                                          respname = out.above(out.identifier("y"), 
                                                               out.operator("^")),
                                          coeff.name = names(x$coefficients[, 1]))
{
    x <- x[[1]]

    ret <- style.apa.numeric(list(x$coefficients[1, 1]))

    for (i in 2:length(x$coefficients[, 1]))
        ret <- c(ret,
                 out.concat(style.apa.numeric(list(x$coefficients[i, 1])),
                                              name = style.apa.character(list(coeff.name[i]),
                                                                         as.identifier=TRUE),
                            sep = "*"))

    ret <- paste(ifelse(ret < 0,
                        ret,
                        paste0("+", ret)),
                 collapse = "")

    ret <- paste(respname,
                 "=",
                 ret,
                 collapse = "")

    return(ret)
}


#' Formats the summary of a Linear Model Fit (unstandardized coefficients)
#'
#' This style functions takes the summary of a linear model fit from
#' \code{\link[stats]{lm}} and transforms it into a formatted character
#' vector. It prints coefficient statistics.
#'
#' @template internal_style
#'
#' @param x a list. First item must be the summary of a linear model fit from
#' \code{\link[stats]{lm}}. All other list items will be ignored.
#'
#' @param coeff a integer, specifying the desired coefficient.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[stats]{lm}}. See
#' \code{\link{style.apa.summary.lm.beta.coeff}} for
#' \code{\link[lm.beta]{lm.beta}} models.
#'
#' @return character vector with a formatted character vector.
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.D9 <- lm(weight ~ group)
#' pprint(summary(lm.D9),
#'        format = "summary.lm.coeff",
#'        coeff = 2L)
style.apa.summary.lm.coeff <- function(x,
                                       coeff = 1L)
{
    x <- x[[1]]

    if (1L == coeff)
        mycname <- "intercept"
    else
        mycname <- "b"

    ret <- style.apa.numeric(list(x$coefficients[coeff, 1]),
                               name = style.apa.character(list(mycname),
                                                          as.identifier=TRUE))

    # t value
    ret <- c(ret,
             style.apa.numeric(list(x$coefficients[coeff, 3]),
                               name = style.apa.character(list("t"),
                                                          as.identifier=TRUE),
                               inbracket = style.apa.df(list(x$df[2]))))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x$coefficients[coeff, 4])))

    return(ret)
}


#' Formats the summary of a Linear Model Fit (standardized coefficients)
#'
#' This style functions takes the summary of a linear model fit from
#' \code{\link[lm.beta]{lm.beta}} and transforms it into a formatted character
#' vector. It prints coefficient statistics.
#'
#' @template internal_style
#'
#' @param x a list. First item must be the summary of a linear model fit from
#' \code{\link[lm.beta]{lm.beta}}. All other list items will be ignored.
#'
#' @param coeff a integer, specifying the desired coefficient.
#'
#' @param standardized a logical, indicating whether the standardized or
#' unstandardized coefficients are printed. Intercept will always be
#' unstandardized.
#'
#' @family APA style functions
#'
#' @seealso \code{\link[lm.beta]{lm.beta}}. See
#' \code{\link{style.apa.summary.lm.coeff}} for models of
#' \code{\link[stats]{lm}} as well.
#'
#' @return character vector with a formatted character vector.
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' # requires lm.beta
#' if (requireNamespace("lm.beta", quietly=TRUE))
#' {
#'     lm.D9 <- lm.beta::lm.beta(lm(weight ~ group))
#'     pprint(summary(lm.D9),
#'            format = "summary.lm.beta.coeff",
#'            coeff = 2L)
#' }
style.apa.summary.lm.beta.coeff <- function(x,
                                            coeff = 1L,
                                            standardized = TRUE)
{
    x <- x[[1]]

    myindex <- 1

    if (1L == coeff)
        mycname <- "intercept"
    else if (standardized)
    {
        mycname <- "beta"
        myindex <- 2
    }
    else
        mycname <- "b"

    ret <- style.apa.numeric(list(x$coefficients[coeff, myindex]),
                               name = style.apa.character(list(mycname),
                                                          as.identifier=TRUE))

    # t value
    ret <- c(ret,
             style.apa.numeric(list(x$coefficients[coeff, 4]),
                               name = style.apa.character(list("t"),
                                                          as.identifier=TRUE),
                               inbracket = style.apa.df(list(x$df[2]))))

    # p value
    ret <- c(ret,
             style.apa.p.value(list(x$coefficients[coeff, 5])))

    return(ret)
}
