\name{comp}
\alias{comp}
\alias{\%o\%}
\alias{composition}
\title{Composition of functions}
\description{

If \eqn{f} and \eqn{g} are functions, their composition \eqn{f\circ g}{omitted}
is defined by \eqn{(f\circ g)(x) = f(g(x))}{omitted}

}
\usage{
comp(f,g)
f \%o\% g
}
\arguments{\item{f,g}{Functions, objects of class \code{vf}}}
\value{
  Returns an object of class \code{vf}.
}

\author{Robin K. S. Hankin}
\examples{

f <- as.vf(sin)
g <- as.vf(cos)

comp(f,g)

as.function(f \%o\% g)(3)

}
