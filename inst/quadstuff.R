library("vfunc")
library("quadform")
M <- matrix(rnorm(9),3,3)
x <- 1:3

f <- as.vf(function(M){M + diag(3)})
g <- as.vf(function(M){M + 2})
h <- as.vf(function(M){solve(M)})

setGeneric("cprod")
setMethod("cprod",
          signature(x="vf", y="vf"),
          function(x,y){as.vf(function(M){cprod(x(M),y(M))})})

setGeneric("tcprod")
setMethod("tcprod",
          signature(x="vf", y="vf"),
          function(x,y){as.vf(function(M){tcprod(x(M),y(M))})})

setGeneric("quad.form")
setMethod("quad.form",
           signature(M="vf", x="vf"),
           function(M,x){as.vf(function(a,b){quad.form(M(a),x(b))})})


setGeneric("Conj")
setMethod("Conj","vf",function(z){as.vf(function(o){Conj(z(o))})})

cprod(f,g)(M) == cprod(f(M),g(M))
cprod(f+g,f)(M) == cprod(f(M)+g(M),f(M))
print(cprod(f+g,cprod(f,g))(M) - cprod(f(M)+g(M),cprod(f(M),g(M))))
print(quad.form(f,g)(M,x) - quad.form(f(M),g(x)))
print(
    quad.form(f + cprod(f,g),g+5)(M,x) - 
    quad.form(f(M) + cprod(f(M),g(M)),g(x)+5))

