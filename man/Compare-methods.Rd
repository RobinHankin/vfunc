\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,ANY,vf-method}
\alias{Compare,function,vf-method}
\alias{Compare,vf,ANY-method}
\alias{Compare,vf,function-method}
\alias{Compare,vf,vf-method}
\title{\code{Compare} methods for \code{vf} objects}
\description{

  Wouldn't it be nice to say \code{(f > g)(x)} rather than the terrible,
  tedious and error-prone \code{f(x) > g(x)}?  Well, now you can!
  
}
\section{Methods}{
\describe{
\item{\code{signature(e1 = "ANY", e2 = "vf")}}{}
\item{\code{signature(e1 = "function", e2 = "vf")}}{}
\item{\code{signature(e1 = "vf", e2 = "ANY")}}{}
\item{\code{signature(e1 = "vf", e2 = "function")}}{}
\item{\code{signature(e1 = "vf", e2 = "vf")}}{}
}}
\keyword{methods}
\examples{
x <- seq(from=0, to=2*pi, len=100)
(Sin > Cos*Tan)(x)
}
