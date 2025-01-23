\name{vfunc-package}
\alias{vfunc-package}
\alias{vfunc}
\docType{package}
\title{
\packageTitle{vfunc}
}
\description{
\packageDescription{vfunc}
}
\details{

If \code{f <- function(x){x^2}} and \code{g <- function(x){x+1}} it is a
constant source of annoyance to me that \dQuote{\code{f+g}} is not
defined.  Package vfunc allows you to do this.

}
\author{
\packageAuthor{vfunc}

Maintainer: \packageMaintainer{vfunc}
}
\keyword{package}
\section{Documentation Index}{\packageIndices{vfunc}}

\examples{
f <- as.vf(function(x){x^2})

f + Sin

as.function(f*Sin + Exp)(1:4)
}
