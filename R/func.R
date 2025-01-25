## The S4 class "function" is a sealed class, it can't be modified.

setClass("vf", slots = c(.Data = "function") )

setAs("function", "vf", function(from){new("vf", .Data = from)})  # coerces from function to vf
setAs("vf", "function", function(from){from@.Data})               # coerces from vf to function
setAs("numeric", "vf", function(from){"not allowed"})


setMethod("as.function", signature(x = "vf"), function(x){as(x, "function")})

setGeneric("as.vf", function(x){standardGeneric("as.vf")})
setMethod("as.vf", signature(x = "function"), function(x){as(x, "vf")})
setMethod("as.vf", signature(x = "vf"), function(x){x})
setMethod("as.vf", signature(x = "numeric"), function(x){as(x, "vf")})

`.vf.negative`   <- function(e1){as.vf(function(...){ -as.function(e1)(...)})}
`.vf.reciprocal` <- function(e1){as.vf(function(...){1/as.function(e1)(...)})}

`.vf.vf.add`   <- function(e1, e2){as.vf(function(...){e1(...) + e2(...)})}
`.vf.vf.mult`  <- function(e1, e2){as.vf(function(...){e1(...) * e2(...)})}
`.vf.vf.power` <- function(e1, e2){as.vf(function(...){e1(...) ^ e2(...)})}

`.vf.ANY.add`  <- function(e1, e2){as.vf(function(...){e1(...) + e2})}
`.vf.ANY.mult` <- function(e1, e2){as.vf(function(...){e1(...) * e2})}

`.vf.ANY.power` <- function(e1, e2){as.vf(function(...){e1(...) ^ e2})}
`.ANY.vf.power` <- function(e1, e2){as.vf(function(...){e1 ^ e2(...)})}

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

`.vf.ANY.arith` <- function(e1,e2){ # e1: vf, e2: ANY
    e1 <- as.vf(e1)
    switch(.Generic,
           "+" = .vf.ANY.add  (e1, e2),
           "-" = .vf.ANY.add  (e1, -e2),
           "*" = .vf.ANY.mult (e1, e2),
           "/" = .vf.ANY.mult (e1, 1/e2),
           "^" = .vf.ANY.power(e1, e2),
           stop(gettextf("binary operator %s not implemented on vf objects", dQuote(.Generic)))
           ) }

`.ANY.vf.arith` <- function(e1,e2){ # e1: ANY, e2: vf
    e2 <- as.vf(e2)
    switch(.Generic,
           "+" = .vf.ANY.add  ( e2, e1),
           "-" = .vf.ANY.add  (-e2, e1),
           "*" = .vf.ANY.mult ( e2, e1),
           "/" = .vf.ANY.mult (.vf.reciprocal(e2), e1),
           "^" = .ANY.vf.power(e1, e2),
           stop(gettextf("binary operator %s not implemented on vf objects", dQuote(.Generic)))
           ) }

setMethod("Arith", signature(e1 = "vf"      , e2 = "vf"      ), .vf.vf.arith)
setMethod("Arith", signature(e1 = "vf"      , e2 = "function"), .vf.vf.arith)
setMethod("Arith", signature(e1 = "function", e2 = "vf"      ), .vf.vf.arith)

## Following line [and similar lines for function/ANY etc] would
## be desirable in this context but does not work as 'function' is a sealed class
## setMethod("Arith", signature(e1 = "function", e2 = "function"), .vf.arith        )

setMethod("Arith", signature(e1 = "vf" , e2 = "ANY"     ), .vf.ANY.arith)
setMethod("Arith", signature(e1 = "ANY", e2 = "vf"      ), .ANY.vf.arith)

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

`.vf.ANY.eq` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) == e2})}
`.vf.ANY.gt` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) >  e2})}
`.vf.ANY.ge` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) >= e2})}
`.vf.ANY.lt` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) <  e2})}
`.vf.ANY.le` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) <= e2})}
`.vf.ANY.ne` <-  function(e1, e2){as.vf(function(...){as.function(e1)(...) != e2})}

`.ANY.vf.eq` <-  function(e1, e2){as.vf(function(...){e1 == as.function(e2)(...)})}
`.ANY.vf.gt` <-  function(e1, e2){as.vf(function(...){e1 >  as.function(e2)(...)})}
`.ANY.vf.ge` <-  function(e1, e2){as.vf(function(...){e1 >= as.function(e2)(...)})}
`.ANY.vf.lt` <-  function(e1, e2){as.vf(function(...){e1 <  as.function(e2)(...)})}
`.ANY.vf.le` <-  function(e1, e2){as.vf(function(...){e1 <= as.function(e2)(...)})}
`.ANY.vf.ne` <-  function(e1, e2){as.vf(function(...){e1 != as.function(e2)(...)})}

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

`.vf.ANY.compare`   <- function(e1, e2){
    e1 <- as.vf(e1)
    switch(.Generic,
           "==" = .vf.ANY.eq(e1, e2),
           ">"  = .vf.ANY.gt(e1, e2),
           ">=" = .vf.ANY.ge(e1, e2),
           "<"  = .vf.ANY.lt(e1, e2),
           "<=" = .vf.ANY.le(e1, e2),
           "!=" = .vf.ANY.ne(e1, e2),
           stop(gettextf("Comparison operator %s not implemented on vf objects", dQuote(.Generic)))
           )}

`.ANY.vf.compare`   <- function(e1, e2){
    e2 <- as.vf(e2)
    switch(.Generic,
           "==" = .ANY.vf.eq(e1, e2),
           ">"  = .ANY.vf.gt(e1, e2),
           ">=" = .ANY.vf.ge(e1, e2),
           "<"  = .ANY.vf.lt(e1, e2),
           "<=" = .ANY.vf.le(e1, e2),
           "!=" = .ANY.vf.ne(e1, e2),
           stop(gettextf("Comparison operator %s not implemented on vf objects", dQuote(.Generic)))
           )}

setMethod("Compare", signature(e1="vf"      , e2="vf"      ), .vf.vf.compare )
setMethod("Compare", signature(e1="vf"      , e2="function"), .vf.vf.compare )
setMethod("Compare", signature(e1="function", e2="vf"      ), .vf.vf.compare )
setMethod("Compare", signature(e1="vf"      , e2="ANY"     ), .vf.ANY.compare)
setMethod("Compare", signature(e1="ANY"     , e2="vf"      ), .ANY.vf.compare)

Abs      <- as.vf(function(x){     abs(x)})
Sign     <- as.vf(function(x){    sign(x)})
Sqrt     <- as.vf(function(x){    sqrt(x)})
Ceiling  <- as.vf(function(x){ ceiling(x)})
Floor    <- as.vf(function(x){   floor(x)})
Trunc    <- as.vf(function(x){   trunc(x)})
Cummax   <- as.vf(function(x){  cummax(x)})
Cummin   <- as.vf(function(x){  cummin(x)})
Cumprod  <- as.vf(function(x){ cumprod(x)})
Cumsum   <- as.vf(function(x){  cumsum(x)})
Log      <- as.vf(function(x){     log(x)})
Log10    <- as.vf(function(x){   log10(x)})
Log2     <- as.vf(function(x){    log2(x)})
Log1p    <- as.vf(function(x){   log1p(x)})
Acos     <- as.vf(function(x){    acos(x)})
Acosh    <- as.vf(function(x){   acosh(x)})
Asin     <- as.vf(function(x){    asin(x)})
Asinh    <- as.vf(function(x){   asinh(x)})
Atan     <- as.vf(function(x){    atan(x)})
Atanh    <- as.vf(function(x){   atanh(x)})
Exp      <- as.vf(function(x){     exp(x)})
Expm1    <- as.vf(function(x){   expm1(x)})
Cos      <- as.vf(function(x){     cos(x)})
Cosh     <- as.vf(function(x){    cosh(x)})
Cospi    <- as.vf(function(x){   cospi(x)})
Sin      <- as.vf(function(x){     sin(x)})
Sinh     <- as.vf(function(x){    sinh(x)})
Sinpi    <- as.vf(function(x){   sinpi(x)})
Tan      <- as.vf(function(x){     tan(x)})
Tanh     <- as.vf(function(x){    tanh(x)})
Tanpi    <- as.vf(function(x){   tanpi(x)})
Gamma    <- as.vf(function(x){   gamma(x)})
Lgamma   <- as.vf(function(x){  lgamma(x)})
Digamma  <- as.vf(function(x){ digamma(x)})
Trigamma <- as.vf(function(x){trigamma(x)})


setMethod("abs"     , "vf", function(x){as.vf(function(o){     Abs(x(o))})})
setMethod("sign"    , "vf", function(x){as.vf(function(o){    Sign(x(o))})})
setMethod("sqrt"    , "vf", function(x){as.vf(function(o){    Sqrt(x(o))})})
setMethod("ceiling" , "vf", function(x){as.vf(function(o){ Ceiling(x(o))})})
setMethod("floor"   , "vf", function(x){as.vf(function(o){   Floor(x(o))})})
setMethod("trunc"   , "vf", function(x){as.vf(function(o){   Trunc(x(o))})})
setMethod("cummax"  , "vf", function(x){as.vf(function(o){  Cummax(x(o))})})
setMethod("cummin"  , "vf", function(x){as.vf(function(o){  Cummin(x(o))})})
setMethod("cumprod" , "vf", function(x){as.vf(function(o){ Cumprod(x(o))})})
setMethod("cumsum"  , "vf", function(x){as.vf(function(o){  Cumsum(x(o))})})
setMethod("log"     , "vf", function(x){as.vf(function(o){     Log(x(o))})})
setMethod("log10"   , "vf", function(x){as.vf(function(o){   Log10(x(o))})})
setMethod("log2"    , "vf", function(x){as.vf(function(o){    Log2(x(o))})})
setMethod("log1p"   , "vf", function(x){as.vf(function(o){   Log1p(x(o))})})
setMethod("acos"    , "vf", function(x){as.vf(function(o){    Acos(x(o))})})
setMethod("acosh"   , "vf", function(x){as.vf(function(o){   Acosh(x(o))})})
setMethod("asin"    , "vf", function(x){as.vf(function(o){    Asin(x(o))})})
setMethod("asinh"   , "vf", function(x){as.vf(function(o){   Asinh(x(o))})})
setMethod("atan"    , "vf", function(x){as.vf(function(o){    Atan(x(o))})})
setMethod("atanh"   , "vf", function(x){as.vf(function(o){   Atanh(x(o))})})
setMethod("exp"     , "vf", function(x){as.vf(function(o){     Exp(x(o))})})
setMethod("expm1"   , "vf", function(x){as.vf(function(o){   Expm1(x(o))})})
setMethod("cos"     , "vf", function(x){as.vf(function(o){     Cos(x(o))})})
setMethod("cosh"    , "vf", function(x){as.vf(function(o){    Cosh(x(o))})})
setMethod("cospi"   , "vf", function(x){as.vf(function(o){   Cospi(x(o))})})
setMethod("sin"     , "vf", function(x){as.vf(function(o){     Sin(x(o))})})
setMethod("sinh"    , "vf", function(x){as.vf(function(o){    Sinh(x(o))})})
setMethod("sinpi"   , "vf", function(x){as.vf(function(o){   Sinpi(x(o))})})
setMethod("tan"     , "vf", function(x){as.vf(function(o){     Tan(x(o))})})
setMethod("tanh"    , "vf", function(x){as.vf(function(o){    Tanh(x(o))})})
setMethod("tanpi"   , "vf", function(x){as.vf(function(o){   Tanpi(x(o))})})
setMethod("gamma"   , "vf", function(x){as.vf(function(o){   Gamma(x(o))})})
setMethod("lgamma"  , "vf", function(x){as.vf(function(o){  Lgamma(x(o))})})
setMethod("digamma" , "vf", function(x){as.vf(function(o){ Digamma(x(o))})})
setMethod("trigamma", "vf", function(x){as.vf(function(o){Trigamma(x(o))})})
