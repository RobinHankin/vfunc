\name{Math}
\alias{Math}
\alias{Sin}
\alias{Cos}
\alias{Tan}
\alias{Log}
\alias{Exp}
\alias{sin,vf-method}
\alias{cos,vf-method}
\alias{tan,vf-method}
\alias{log,vf-method}
\alias{exp,vf-method}
\title{
Mathematical functions in the \pkg{vfunc} package: trig, exponential,
log, etc.
}
\description{
The \code{Math} group conains 35 functions including \code{sin()},
\code{log()}, etc.  The \code{vfunc} equivalents are capitalized, as in
\code{Sin()}, \code{Log()}, etc.
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
