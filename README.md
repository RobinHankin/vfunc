The `vfunc` package: adding two functions in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/vfunc.png" width = "150" align="right" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vfunc)](https://cran.r-project.org/package=vfunc)
[![Codecov test
coverage](https://codecov.io/gh/RobinHankin/vfunc/graph/badge.svg)](https://app.codecov.io/gh/RobinHankin/vfunc)
<!-- badges: end -->

# Overview

In mathematics, given two functions
![f,g\colon\mathbb{R}\longrightarrow\mathbb{R}](https://latex.codecogs.com/png.latex?f%2Cg%5Ccolon%5Cmathbb%7BR%7D%5Clongrightarrow%5Cmathbb%7BR%7D "f,g\colon\mathbb{R}\longrightarrow\mathbb{R}"),
it is natural to define
![f+g](https://latex.codecogs.com/png.latex?f%2Bg "f+g") as the function
that maps
![x\in\mathbb{R}](https://latex.codecogs.com/png.latex?x%5Cin%5Cmathbb%7BR%7D "x\in\mathbb{R}")
to ![f(x) +
g(x)](https://latex.codecogs.com/png.latex?f%28x%29%20%2B%0Ag%28x%29 "f(x) +
g(x)"). However, in base R, objects of class `function` do not have
arithmetic methods defined, so idiom such as `f + g` returns an error,
even though it has a perfectly reasonable expectation. The
`vfunc package` offers this functionality. Other similar features are
provided, which lead to compact and readable idiom. A wide class of
coding bugs is eliminated.

Consider the following R session:

``` r
f <- function(x){x^2}
g <- function(x){1/(1-x)}
f + g
#> Error in f + g: non-numeric argument to binary operator
```

Above, there is a reasonably clear expectation for `f + g`: it should
give a function that returns the sum of `f()` and `g()`; something like
`function(x){f(x) + g(x)}`. However, it returns an error because `f` and
`g` are objects of `S4` class `function`, which do not have an addition
method. The `vfunc` package allows us to do this.

# The package in use

The package is designed so that objects of class `vf` operate as
functions but are subject to arithmetic operations, which are executed
transparently. For example:

``` r
library("vfunc")
#> 
#> Attaching package: 'vfunc'
#> The following object is masked from 'package:stats':
#> 
#>     Gamma
f <- as.vf(f)
g <- as.vf(g)
(f + g)(1:10)
#>  [1]      Inf  3.00000  8.50000 15.66667 24.75000 35.80000 48.83333 63.85714
#>  [9] 80.87500 99.88889
```

Further, functions may be combined arithmetically:

``` r
(f + 4*g - f*g)(1:10)
#>  [1]       NaN   4.00000  11.50000  20.00000  30.25000  42.40000  56.50000
#>  [8]  72.57143  90.62500 110.66667
```

or compositionally:

``` r
(f(g) + g(f))(1:10)
#>  [1]         Inf 0.666666667 0.125000000 0.044444444 0.020833333 0.011428571
#>  [7] 0.006944444 0.004535147 0.003125000 0.002244669
```

The advantages of such idiom fall in to two main categories. Firstly,
code can become considerably more compact; and secondly one can guard
against a wide class of hard-to-find bugs. Now consider `f()` and `g()`
to be trivariate functions, each taking three arguments, say,

``` r
f <- function(x,y,z){x + x*y - x/z}
g <- function(x,y,z){x^2 - z}
```

and ![x=1.2](https://latex.codecogs.com/png.latex?x%3D1.2 "x=1.2"),
![y=1.7](https://latex.codecogs.com/png.latex?y%3D1.7 "y=1.7"),
![z=4.3](https://latex.codecogs.com/png.latex?z%3D4.3 "z=4.3"). Given
this, we wish to calculate

![(f(x,y,z) + g(x,y,z))(f(x,y,z) + 4 - 2f(x,y,z)g(x,y,z)).](https://latex.codecogs.com/png.latex?%28f%28x%2Cy%2Cz%29%20%2B%20g%28x%2Cy%2Cz%29%29%28f%28x%2Cy%2Cz%29%20%2B%204%20-%202f%28x%2Cy%2Cz%29g%28x%2Cy%2Cz%29%29. "(f(x,y,z) + g(x,y,z))(f(x,y,z) + 4 - 2f(x,y,z)g(x,y,z)).")

How would one code up such an expression in R? The standard way would be

``` r
 x <- 1.2
 y <- 1.7
 z <- 4.3       
(f(x,y,z) + g(x,y,z))*(f(x,y,z) + 4 - 2*f(x,y,z)*g(x,y,z))
#> [1] 2.411975
```

Note the repeated specification of argument list `(x,y,z)`, repeated
here five times. Now use the `vfunc` package:

``` r
f <- as.vf(f)
g <- as.vf(g)
((f + g)*(f + 4 - 2*f*g))(x,y,z)
#> [1] 2.411975
```

See how the package allows one to ‘’factorize’’ the argument list so it
appears once, leading to more compact code. It is also arguably less
error-prone, as the following example illustrates. Consider

![f(x+z,y+z,f(x,x,y)-g(x,x,y)) + g(x+z, y+z,f(x,x,y)-g(x,x,y))](https://latex.codecogs.com/png.latex?f%28x%2Bz%2Cy%2Bz%2Cf%28x%2Cx%2Cy%29-g%28x%2Cx%2Cy%29%29%20%2B%20g%28x%2Bz%2C%20y%2Bz%2Cf%28x%2Cx%2Cy%29-g%28x%2Cx%2Cy%29%29 "f(x+z,y+z,f(x,x,y)-g(x,x,y)) + g(x+z, y+z,f(x,x,y)-g(x,x,y))")

(such expressions arise in the study of dynamical systems). Note that
functions ![f](https://latex.codecogs.com/png.latex?f "f") and
![g](https://latex.codecogs.com/png.latex?g "g") are to be evaluated
with two distinct sets of arguments at different levels of nesting,
namely
![(x,x,y)](https://latex.codecogs.com/png.latex?%28x%2Cx%2Cy%29 "(x,x,y)")
at the inner level and
![(x+z,y+z,f(x,x,y)-g(x,x,y)](https://latex.codecogs.com/png.latex?%28x%2Bz%2Cy%2Bz%2Cf%28x%2Cx%2Cy%29-g%28x%2Cx%2Cy%29 "(x+z,y+z,f(x,x,y)-g(x,x,y)")
at the outer. Standard R idiom would be

``` r
f(x + z, y + z, f(x, x, y) - g(x, x, y)) + g(x + z, y + z, f(x, x, y) - g(x, x, y))
#> [1] 64.04918
```

The author can attest that finding bugs in such expressions can be
difficult \[it is easy to mistype `(x,x,y)` in one of its occurences,
yet difficult to detect the error\]. However, `vfunc` idiom would be

``` r
(f + g)(x + z, y + z, (f - g)(x, x, y))
#> [1] 64.04918
```

which is certainly shorter, arguably neater and at least the author
finds such constructions considerably less error-prone. In this form,
one can be sure that both `f()` and `g()` are called with identical
arguments at each of the two levels in the expression, as the arguments
appear only once.

The package includes functions such as `Sin()` which is a `vf`
equivalent to `base::sin()`. This allows one to define composite
functions such as

``` r
j <- as.vf(function(x,y){Cos(x) + Sin(x-y)})
k <- as.vf(function(x,y){Tan(x) + Log(x+y)})
l <- as.vf(function(x,y){Sin(x/2) + x^2   })
```

(note that functions `j()`, `k()` and `l()` are bivariate). Then compare

``` r
(j + k + l)(Sin + Log, Cos + Exp)(Sin + Tan)(0.4)
#> [1] 2.545235
```

with the one-stage idiom which reads:

``` r
j(sin(sin(0.4) + tan(0.4)) + log(sin(0.4) + tan(0.4)), cos(sin(0.4) + tan(0.4)) +
exp(sin(0.4) + tan(0.4))) + k(sin(sin(0.4) + tan(0.4)) + log(sin(0.4) + tan(0.4)),
cos(sin(0.4) + tan(0.4)) + exp(sin(0.4) + tan(0.4)))+ l(sin(sin(0.4) + tan(0.4)) +
log(sin(0.4) + tan(0.4)), cos(sin(0.4) + tan(0.4)) + exp(sin(0.4) + tan(0.4)))
#> [1] 2.545235
```

and the multi-stage idiom:

``` r
A <- function(x,y){j(x,y) + k(x,y) + l(x,y)}
B <- function(x){sin(x) + log(x)}
C <- function(x){cos(x) + exp(x)}
D <- function(x){sin(x) + tan(x)}
x <- 0.4
A(B(D(x)), C(D(x)))
#> [1] 2.545235
```

See how the one-stage idiom is very long, and the multi-stage idiom is
opaque \[and nevertheless has repeated instances of `(x,y)` and `x`\].

# Conclusions

The `vfunc` package allows functions to be ‘’factorized’’, that is,
`f(x) + g(x)` to be re-written `(f + g)(x)`. This allows for concise
idiom and eliminates a certain class of coding errors. The package also
allows for recursive application of such ideas.

# Further information

For more detail, see the package vignette

`vignette("vfunc")`
