\name{vfunc-package}
\alias{vfunc-package}
\alias{vfunc}
\docType{package}
\title{
\packageTitle{vfunc}
}
\description{
If \code{f <- function(x){x^2}} and \code{g <- function(x){x+1}} it is a
constant source of annoyance to me that \dQuote{\code{f+g}} is not
defined.  Package vfunc allows you to do this.
}
\details{

The package defines a single \proglang{S4} class, \code{vf}.  This has a
single slot, \code{.Data}, of type \code{function} which means that
\code{vf} objects inherit much of the behaviour of functions, but for
which new methods (such as the \code{Arith} group of \code{S4} generics)
may be defined.

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
