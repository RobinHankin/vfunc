## The S4 class "function" is a sealed class, it can't be modified.

setClass("vf", slots = c(x = "function") )

setAs("function", "vf", function(from){new("vf", x = from)})  # coerces from function to vf
setAs("vf", "function", function(from){from@x})               # coerces from vf to function
setAs("numeric", "vf",function(from){"not allowed"})

setMethod("as.function", signature(x = "vf"),function(x){as(x, "function")})

setGeneric("as.vf", function(x){standardGeneric("as.vf")})
setMethod("as.vf", signature(x = "function"),function(x){as(x,"vf")})
setMethod("as.vf", signature(x = "vf"),function(x){x})

`.vf.negative`   <- function(e1){as.vf(function(...){ -as.function(e1)(...)})}
`.vf.reciprocal` <- function(e1){as.vf(function(...){1/as.function(e1)(...)})}

`.vf.vf.add`   <- function(e1, e2){as.vf(function(...){as.function(e1)(...) + as.function(e2)(...)})}
`.vf.vf.mult`  <- function(e1, e2){as.vf(function(...){as.function(e1)(...) * as.function(e2)(...)})}
`.vf.vf.power` <- function(e1, e2){as.vf(function(...){as.function(e1)(...) ^ as.function(e2)(...)})}

`.vf.numeric.add`  <- function(e1, e2){as.vf(function(...){as.function(e1)(...) + e2})}
`.vf.numeric.mult` <- function(e1, e2){as.vf(function(...){as.function(e1)(...) * e2})}

`.vf.numeric.power` <- function(e1, e2){as.vf(function(...){as.function(e1)(...) ^ e2})}
`.numeric.vf.power` <- function(e1, e2){as.vf(function(...){e1 ^ as.function(e2)(...)})}

`.vf.vf.arith` <- function(e1,e2){ # e1: vf, e2: vf
    e1 <- as.vf(e1)
    e2 <- as.vf(e2)
    switch(.Generic,
           "+" = .vf.vf.add  (e1, e2),
           "-" = .vf.vf.add  (e1, .vf.negative(e2)),
           "*" = .vf.vf.mult (e1, e2),
           "/" = .vf.vf.mult (e1, .vf.reciprocal(e2)),
           "^" = .vf.vf.power(e1, e2),
           stop(gettextf("binary operator %s not implemented on vf objects", dQuote(.Generic)))
           ) }

`.vf.numeric.arith` <- function(e1,e2){ # e1: vf, e2: numeric
    e1 <- as.vf(e1)
    switch(.Generic,
           "+" = .vf.numeric.add  (e1, e2),
           "-" = .vf.numeric.add  (e1, -e2),
           "*" = .vf.numeric.mult (e1, e2),
           "/" = .vf.numeric.mult (e1, 1/e2),
           "^" = .vf.numeric.power(e1, e2),
           stop(gettextf("binary operator %s not implemented on vf objects", dQuote(.Generic)))
           ) }

`.numeric.vf.arith` <- function(e1,e2){ # e1: numeric, e2: vf
    e2 <- as.vf(e2)
    switch(.Generic,
           "+" = .vf.numeric.add  ( e2, e1),
           "-" = .vf.numeric.add  (-e2, e1),
           "*" = .vf.numeric.mult ( e2, e1),
           "/" = .vf.numeric.mult (.vf.reciprocal(e2), e1),
           "^" = .numeric.vf.power(e1, e2),
           stop(gettextf("binary operator %s not implemented on vf objects", dQuote(.Generic)))
           ) }

setMethod("Arith", signature(e1 = "vf"      , e2 = "vf"      ), .vf.vf.arith)
setMethod("Arith", signature(e1 = "vf"      , e2 = "function"), .vf.vf.arith)
setMethod("Arith", signature(e1 = "function", e2 = "vf"      ), .vf.vf.arith)

## Following line [and similar lines for function/numeric etc] would
## be desirable in this context but does not work as 'function' and
## 'numeric' are sealed classes
## setMethod("Arith", signature(e1 = "function", e2 = "function"), .vf.arith        )

setMethod("Arith", signature(e1 = "vf"      , e2 = "numeric" ), .vf.numeric.arith)
setMethod("Arith", signature(e1 = "numeric" , e2 = "vf"      ), .numeric.vf.arith)

setMethod("Arith", signature(e1 = "vf", e2="missing"),
          function(e1, e2){
              switch(.Generic,
                     "+" = e1,
                     "-" = .vf.negative(e1),
                     stop(gettextf("unary operator %s not implemented on vf objects", dQuote(.Generic)))
                     )
          })



`.vf.vf.eq` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) == as.function(e2)(...)})}
`.vf.vf.gt` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) >  as.function(e2)(...)})}
`.vf.vf.ge` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) >= as.function(e2)(...)})}
`.vf.vf.lt` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) <  as.function(e2)(...)})}
`.vf.vf.le` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) <= as.function(e2)(...)})}
`.vf.vf.ne` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) != as.function(e2)(...)})}

`.vf.numeric.eq` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) == e2})}
`.vf.numeric.gt` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) >  e2})}
`.vf.numeric.ge` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) >= e2})}
`.vf.numeric.lt` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) <  e2})}
`.vf.numeric.le` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) <= e2})}
`.vf.numeric.ne` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) != e2})}

`.numeric.vf.eq` <-  function(e1, e2){as.vf(function(...){e1 == as.function(e2)(...)})}
`.numeric.vf.gt` <-  function(e1, e2){as.vf(function(...){e1 >  as.function(e2)(...)})}
`.numeric.vf.ge` <-  function(e1, e2){as.vf(function(...){e1 >= as.function(e2)(...)})}
`.numeric.vf.lt` <-  function(e1, e2){as.vf(function(...){e1 <  as.function(e2)(...)})}
`.numeric.vf.le` <-  function(e1, e2){as.vf(function(...){e1 <= as.function(e2)(...)})}
`.numeric.vf.ne` <-  function(e1, e2){as.vf(function(...){e1 != as.function(e2)(...)})}

`.vf.vf.compare`   <- function(e1, e2){
    e1 <- as.vf(e1)
    e2 <- as.vf(e2)
    switch(.Generic,
           "==" = .vf.vf.eq(e1, e2),
           ">"  = .vf.vf.gt(e1, e2),
           ">=" = .vf.vf.ge(e1, e2),
           "<"  = .vf.vf.lt(e1, e2),
           "<=" = .vf.vf.le(e1, e2),
           "!=" = .vf.vf.ne(e1, e2),
           stop(gettextf("Comparison operator %s not implemented on vf objects", dQuote(.Generic)))
           )}

`.vf.numeric.compare`   <- function(e1, e2){
    e1 <- as.vf(e1)
    switch(.Generic,
           "==" = .vf.numeric.eq(e1, e2),
           ">"  = .vf.numeric.gt(e1, e2),
           ">=" = .vf.numeric.ge(e1, e2),
           "<"  = .vf.numeric.lt(e1, e2),
           "<=" = .vf.numeric.le(e1, e2),
           "!=" = .vf.numeric.ne(e1, e2),
           stop(gettextf("Comparison operator %s not implemented on vf objects", dQuote(.Generic)))
           )}

`.numeric.vf.compare`   <- function(e1, e2){
    e2 <- as.vf(e2)
    switch(.Generic,
           "==" = .numeric.vf.eq(e1, e2),
           ">"  = .numeric.vf.gt(e1, e2),
           ">=" = .numeric.vf.ge(e1, e2),
           "<"  = .numeric.vf.lt(e1, e2),
           "<=" = .numeric.vf.le(e1, e2),
           "!=" = .numeric.vf.ne(e1, e2),
           stop(gettextf("Comparison operator %s not implemented on vf objects", dQuote(.Generic)))
           )}

setMethod("Compare", signature(e1="vf"      , e2="vf"      ), .vf.vf.compare     )
setMethod("Compare", signature(e1="vf"      , e2="function"), .vf.vf.compare     )
setMethod("Compare", signature(e1="function", e2="vf"      ), .vf.vf.compare     )
setMethod("Compare", signature(e1="vf"      , e2="numeric" ), .vf.numeric.compare)
setMethod("Compare", signature(e1="numeric" , e2="vf"      ), .numeric.vf.compare)

setMethod("Math", "vf",
          function(x){
              switch(.Generic,
                     abs      = as.vf(function(o){abs(as.function(x)(o))}),
                     abs      = as.vf(function(o){abs(as.function(x)(o))}),
                     sign     = as.vf(function(o){sign(as.function(x)(o))}),
                     sqrt     = as.vf(function(o){sqrt(as.function(x)(o))}),
                     ceiling  = as.vf(function(o){ceiling(as.function(x)(o))}),
                     floor    = as.vf(function(o){floor(as.function(x)(o))}),
                     trunc    = as.vf(function(o){trunc(as.function(x)(o))}),
                     cummax   = as.vf(function(o){cummax(as.function(x)(o))}),
                     cummin   = as.vf(function(o){cummin(as.function(x)(o))}),
                     cumprod  = as.vf(function(o){cumprod(as.function(x)(o))}),
                     cumsum   = as.vf(function(o){cumsum(as.function(x)(o))}),
                     log      = as.vf(function(o){log(as.function(x)(o))}),
                     log10    = as.vf(function(o){log10(as.function(x)(o))}),
                     log2     = as.vf(function(o){log2(as.function(x)(o))}),
                     log1p    = as.vf(function(o){log1p(as.function(x)(o))}),
                     acos     = as.vf(function(o){acos(as.function(x)(o))}),
                     acosh    = as.vf(function(o){acosh(as.function(x)(o))}),
                     asin     = as.vf(function(o){asin(as.function(x)(o))}),
                     asinh    = as.vf(function(o){asinh(as.function(x)(o))}),
                     atan     = as.vf(function(o){atan(as.function(x)(o))}),
                     atanh    = as.vf(function(o){atanh(as.function(x)(o))}),
                     exp      = as.vf(function(o){exp(as.function(x)(o))}),
                     expm1    = as.vf(function(o){expm1(as.function(x)(o))}),
                     cos      = as.vf(function(o){cos(as.function(x)(o))}),
                     cosh     = as.vf(function(o){cosh(as.function(x)(o))}),
                     cospi    = as.vf(function(o){cospi(as.function(x)(o))}),
                     sin      = as.vf(function(o){sin(as.function(x)(o))}),
                     sinh     = as.vf(function(o){sinh(as.function(x)(o))}),
                     sinpi    = as.vf(function(o){sinpi(as.function(x)(o))}),
                     tan      = as.vf(function(o){tan(as.function(x)(o))}),
                     tanh     = as.vf(function(o){tanh(as.function(x)(o))}),
                     tanpi    = as.vf(function(o){tanpi(as.function(x)(o))}),
                     gamma    = as.vf(function(o){gamma(as.function(x)(o))}),
                     lgamma   = as.vf(function(o){lgamma(as.function(x)(o))}),
                     digamma  = as.vf(function(o){digamma(as.function(x)(o))}),
                     trigamma = as.vf(function(o){trigamma(as.function(x)(o))}),
                     stop("not defined")
                     )
          })

`comp` <- function(f,g){as.vf(function(...){as.function(f)(as.function(g)(...))})}
`%o%`  <- function(f,g){comp(f,g)}
