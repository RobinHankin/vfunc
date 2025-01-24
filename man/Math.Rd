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
The \proglang{S4} \code{Math} group conains 35 functions including
\code{sin()}, \code{log()}, etc.  The \code{vfunc} equivalents are
capitalized, as in \code{Sin()}, \code{Log()}, etc.
}
\usage{
Sin(x)
Cos(x)
Tan(x)
Log(x)
Exp(x)
}
\arguments{
  \item{x}{Generally take a single argument of class \code{numeric},
    \code{function}, or \code{vf}}
}
\details{
To appear
}
\value{
Given a numeric, return a numeric; given a \code{vf}, return a \code{vf}
}
\author{Robin K. S. Hankin}
\note{
Notes here to be added
}
\examples{

Sin + Exp

c((Sin + Exp)(.02232) ,sin(0.02232) +  exp(0.02232))
}
