\name{as.vf}
\alias{as.vf}
\alias{as.vf,function-method}
\title{
Coerce a function to a virtual function.
}
\description{
Coerce a function to a virtual function.
}
\usage{
as.vf(x)
}
\arguments{
  \item{x}{Generally a function}
}
\value{
Returns an object of class \code{vf}.
}
\author{Robin K. S. Hankin}
\examples{
as.vf(\(x)x^2)
Sin + as.vf(\(p){p^3})
}
