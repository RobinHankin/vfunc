\name{pow}
\alias{pow}
\alias{power}
\title{Iterated functions; functional powers}
\description{
  Given a function \eqn{f\colon X\longrightarrow X}{f: X -> X},  we define

  \deqn{f^0 = \mathrm{id_X}}{omitted}

  \deqn{f^{n+1} = f\circ f^n=f^n\circ f,\qquad n\geqslant 0}{omitted}

This gives us \eqn{f^{n+m}=f^n\circ f^m}{omitted} and
\eqn{\left(f^m\right)^n=f^{mn}}{omitted}, which motivates the notation.
For example, \eqn{\sin^3=\sin\circ\sin\circ\sin}{omitted}, so
\eqn{\sin^3(x)=\sin(\sin(\sin x))}{omitted}.

The operator is well-defined due to the power associativity of function
composition.
  
}
\usage{pow(x, n)}
\arguments{
  \item{x}{Object of class \code{vf}}
  \item{n}{Non-negative integer}
}
\value{Returns an object of class \code{vf}}
\author{Robin K. S. Hankin}
\note{
  There are possibly more efficient methods requiring fewer
  compositions, e.g. \code{pow(f,9)} (which would require 8 function
  compositions) could be evaluated by \code{pow(pow(f,3),3)} (which
  requires only four).  But I am not sure that this would actually be
  any faster, and I have not got round to thinking about it yet.

  Also, package idiom for the caret \dQuote{\code{^}} is reserved for
  arithmetic exponentiation [so, for example, \code{(f^3)(x) ==
  f(x)*f(x)*f(x)}].  I believe this is sub-optimal but was unable to
  overload the caret to implement functional iteration.

  }
\examples{

pow(Sin,5)
Sin^5

f <- as.vf(function(x){x^2+1})

pow(f + Sin,4)
pow(f + Sin,4)(2)
}



