% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style_apa.R
\name{style.apa.summary.lm.beta.coeff}
\alias{style.apa.summary.lm.beta.coeff}
\title{Formats the summary of a Linear Model Fit (standardized coefficients)}
\usage{
style.apa.summary.lm.beta.coeff(x, coeff = 1L, standardized = TRUE)
}
\arguments{
\item{x}{a list. First item must be the summary of a linear model fit from
\code{\link[lm.beta]{lm.beta}}. All other list items will be ignored.}

\item{coeff}{a integer, specifying the desired coefficient.}

\item{standardized}{a logical, indicating whether the standardized or
unstandardized coefficients are printed. Intercept will always be
unstandardized.}
}
\value{
character vector with a formatted character vector.
}
\description{
This style functions takes the summary of a linear model fit from
\code{\link[lm.beta]{lm.beta}} and transforms it into a formatted character
vector. It prints coefficient statistics.
}
\details{
Please note that this is a internal style function. It is called
from \code{\link{pprint}} and not exported to user namespace. Usually
\code{\link{pprint}} determines the correct style function automatically,
but you can define the style function by using the \code{format} argument
of \code{\link{pprint}} (pass the name of this function without
\code{style.apa.} prefix). Additionally you can pass the arguments listed
in this documentation to \code{\link{pprint}}.

Argument \code{x} of this function expects a list. Be aware that you do not
have to pass a list to \code{\link{pprint}} or \code{\link{pull.pubprint}}
-- these functions will convert your arguments. This is only necessary if
you want to pass additionally information to the internal style functions
(see vignette for examples).
}
\examples{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
# requires lm.beta
if (requireNamespace("lm.beta", quietly=TRUE))
{
    lm.D9 <- lm.beta::lm.beta(lm(weight ~ group))
    pprint(summary(lm.D9),
           format = "summary.lm.beta.coeff",
           coeff = 2L)
}
}
\seealso{
\code{\link[lm.beta]{lm.beta}}. See
\code{\link{style.apa.summary.lm.coeff}} for models of
\code{\link[stats]{lm}} as well.

Other APA.style.functions: \code{\link{style.apa.anova}},
  \code{\link{style.apa.bartlett}},
  \code{\link{style.apa.character}},
  \code{\link{style.apa.chisq}},
  \code{\link{style.apa.cor.test}},
  \code{\link{style.apa.df}},
  \code{\link{style.apa.fisher}},
  \code{\link{style.apa.ks}},
  \code{\link{style.apa.numeric}},
  \code{\link{style.apa.p.value}},
  \code{\link{style.apa.shapiro}},
  \code{\link{style.apa.summary.aovlist}},
  \code{\link{style.apa.summary.lm.coeff}},
  \code{\link{style.apa.summary.lm.equation}},
  \code{\link{style.apa.summary.lm.model}},
  \code{\link{style.apa.t.test}}
}

