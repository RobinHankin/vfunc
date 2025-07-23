\name{Math}
\alias{Math}
\alias{Abs}
\alias{Sign}
\alias{Sqrt}
\alias{Ceiling}
\alias{Floor}
\alias{Trunc}
\alias{Cummax}
\alias{Cummin}
\alias{Cumprod}
\alias{Cumsum}
\alias{Log}
\alias{Log10}
\alias{Log2}
\alias{Log1p}
\alias{Acos}
\alias{Acosh}
\alias{Asin}
\alias{Asinh}
\alias{Atan}
\alias{Atanh}
\alias{Exp}
\alias{Expm1}
\alias{Cos}
\alias{Cosh}
\alias{Cospi}
\alias{Sin}
\alias{Sinh}
\alias{Sinpi}
\alias{Tan}
\alias{Tanh}
\alias{Tanpi}
\alias{Gamma}
\alias{Lgamma}
\alias{Digamma}
\alias{Trigamma}
\alias{abs,vf-method}
\alias{sign,vf-method}
\alias{sqrt,vf-method}
\alias{ceiling,vf-method}
\alias{floor,vf-method}
\alias{trunc,vf-method}
\alias{cummax,vf-method}
\alias{cummin,vf-method}
\alias{cumprod,vf-method}
\alias{cumsum,vf-method}
\alias{log,vf-method}
\alias{log10,vf-method}
\alias{log2,vf-method}
\alias{log1p,vf-method}
\alias{acos,vf-method}
\alias{acosh,vf-method}
\alias{asin,vf-method}
\alias{asinh,vf-method}
\alias{atan,vf-method}
\alias{atanh,vf-method}
\alias{exp,vf-method}
\alias{expm1,vf-method}
\alias{cos,vf-method}
\alias{cosh,vf-method}
\alias{cospi,vf-method}
\alias{sin,vf-method}
\alias{sinh,vf-method}
\alias{sinpi,vf-method}
\alias{tan,vf-method}
\alias{tanh,vf-method}
\alias{tanpi,vf-method}
\alias{gamma,vf-method}
\alias{lgamma,vf-method}
\alias{digamma,vf-method}
\alias{trigamma,vf-method}
\title{
Math group generic functions in the \pkg{vfunc} package: trig, exponential,
log, etc.
}
\description{
The \proglang{S4} \code{Math} group contains 35 functions including
\code{sin()}, \code{log()}, etc.  The \code{vfunc} equivalents are
capitalized, as in \code{Sin()}, \code{Log()}, etc.
}
\usage{
Abs(x)
Sign(x)
Sqrt(x)
Ceiling(x)
Floor(x)
Trunc(x)
Cummax(x)
Cummin(x)
Cumprod(x)
Cumsum(x)
Log(x)
Log10(x)
Log2(x)
Log1p(x)
Acos(x)
Acosh(x)
Asin(x)
Asinh(x)
Atan(x)
Atanh(x)
Exp(x)
Expm1(x)
Cos(x)
Cosh(x)
Cospi(x)
Sin(x)
Sinh(x)
Sinpi(x)
Tan(x)
Tanh(x)
Tanpi(x)
Gamma(x)
Lgamma(x)
Digamma(x)
Trigamma(x)
}
\arguments{
  \item{x}{Generally take a single argument of class \code{numeric},
    \code{function}, or \code{vf}}
}
\details{

The reason for this rather untransparent device is that primitive
functions such as \code{sin()} behave somewhat differently from other
functions.   We have:

\preformatted{
Sin <- as.vf(function(x){sin(x)})
setMethod("sin", "vf", function(x){as.vf(function(o){Sin(x(o))})})
}

We define \code{Sin()} to be an object of class \code{vf}; the call to
\code{setMethod()} ensures that \code{Sin(f)} operates as intended.

}
\value{
Given a numeric, return a numeric; given a \code{vf}, return a \code{vf}
}
\author{Robin K. S. Hankin}
\note{

Note that \dQuote{\code{sin <- as.vf(sin)}} does not work as desired,
  giving a runtime error; trying to get round this with things like
  \dQuote{\code{sin <- as.vf(function(x)sin)}} and similar means that
  \dQuote{\code{sin(3)}} does not work.

There is no way to inform all \code{vf} objects that, if used as a
function with an argument of a primitive such as \code{sin}, to return
another \code{vf} object---and not to try and evaluate
\dQuote{\code{f(sin)}}, which fails:

\preformatted{
f <- as.vf(function(x){x^2 + 1})
f(Sin)
#> An object of class "vf"
#> function (...) 
#> {
#>     e1(...) + e2
#> }
#> <bytecode: 0x6065e7c8a900>
#> <environment: 0x6065e7c8a548>
f(sin)
#> Error in x^2: non-numeric argument to binary operator
}

Above, we see \code{f(sin)} returning an error (it tries to evaluate
\dQuote{\code{sin^2 + 1}}).  Observe that \dQuote{\code{Sin^2 + 1}} is
perfectly OK, for \code{Sin} is a virtual function.

}
\examples{

Sin + Exp

c((Sin + Exp)(.02232) ,sin(0.02232) +  exp(0.02232))
}
