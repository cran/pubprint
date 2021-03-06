% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defaults.R
\docType{data}
\name{pp_opts_style}
\alias{pp_opts_style}
\title{Publication style options for the pubprint package}
\format{A list with a \code{get} and \code{set} function.}
\usage{
pp_opts_style
}
\description{
A list which functions are used to print in the correct publication style
(like APA).
}
\details{
Using \code{pp_opts_style$get()} to show all currently used publication
style functions. \code{pp_opts_style$set()} allows to change them.
}
\examples{
pp_opts_style$set(pp_init_style())
pp_opts_style$set(pp_init_style("apa"))

}
\seealso{
See \code{\link{pp_init_style}} for initialising this variable in the
correct way and \code{\link{pp_init_out}} for the output format.
}
\keyword{datasets}

