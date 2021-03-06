% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/out.R
\name{pp_init_out}
\alias{pp_init_out}
\title{Setting the output format functions}
\usage{
pp_init_out(x = c("latex", "markdown", "html", "plain"))
}
\arguments{
\item{x}{a character indicating the output format.}
}
\description{
A list is returned which contains all the default functions for the
specified output format.
}
\examples{
pp_opts_out$set(pp_init_out())
pp_opts_out$set(pp_init_out("html"))

}
\seealso{
See \code{\link{pp_opts_out}} to get and set all currently used
output format functions.
}

