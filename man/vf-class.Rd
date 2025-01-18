\name{vf-class}
\docType{class}
\alias{vf-class}
\alias{as.function,vf-method}
\alias{as.vf,vf-method}
\alias{coerce,function,vf-method}
\alias{coerce,numeric,vf-method}
\alias{coerce,vf,function-method}
\title{Class \code{"vf"}}
\description{
Class \code{vf} stands for \dQuote{virtual function}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("vf", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"function"}  }
  }
}
\section{Methods}{
  \describe{
    \item{Arith}{\code{signature(e1 = "function", e2 = "vf")}: ... }
    \item{Arith}{\code{signature(e1 = "numeric", e2 = "vf")}: ... }
    \item{Arith}{\code{signature(e1 = "vf", e2 = "function")}: ... }
    \item{Arith}{\code{signature(e1 = "vf", e2 = "missing")}: ... }
    \item{Arith}{\code{signature(e1 = "vf", e2 = "numeric")}: ... }
    \item{Arith}{\code{signature(e1 = "vf", e2 = "vf")}: ... }
    \item{as.function}{\code{signature(x = "vf")}: ... }
    \item{as.vf}{\code{signature(x = "vf")}: ... }
    \item{coerce}{\code{signature(from = "function", to = "vf")}: ... }
    \item{coerce}{\code{signature(from = "numeric", to = "vf")}: ... }
    \item{coerce}{\code{signature(from = "vf", to = "function")}: ... }
    \item{Compare}{\code{signature(e1 = "function", e2 = "vf")}: ... }
    \item{Compare}{\code{signature(e1 = "numeric", e2 = "vf")}: ... }
    \item{Compare}{\code{signature(e1 = "vf", e2 = "function")}: ... }
    \item{Compare}{\code{signature(e1 = "vf", e2 = "numeric")}: ... }
    \item{Compare}{\code{signature(e1 = "vf", e2 = "vf")}: ... }
    \item{Math}{\code{signature(x = "vf")}: ... }
	 }
}
\author{Robin K. S. Hankin}
\examples{
showClass("vf")
}
\keyword{classes}
