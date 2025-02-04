\name{as.vf}
\alias{as.vf}
\alias{as.vf,function-method}
\alias{as.vf,ANY-method}
\title{Coerce functions to a virtual function.}
\description{
  
  Coerce objects to a virtual function.  Numeric or complex arguments
  are coerced to a constant function.

}
\usage{
as.vf(x)
}
\arguments{
  \item{x}{Generally, a function or numeric}
}
\value{
Returns an object of class \code{vf}.
}
\author{Robin K. S. Hankin}
\note{
  It is rarely necessary to coerce objects such as vectors or matrices
  to class \code{vf} because the Arith methods operate on objects of
  class \code{ANY} directly.
}
\examples{
as.vf(\(x)x^2)
Sin + as.vf(\(p){p^3})

as.vf(1:10)(1e99)

}
