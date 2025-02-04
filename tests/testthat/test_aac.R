test_that("Test suite aac",{  # mostly testing Sin, etc

    expect_close <- function(x,y){expect_true(all(abs(x-y) < 1e-8))}



    testvf <- function(vf1,vf2,x){
        usevf <- c((vf1 + vf2)(x), (vf1(vf2))(x), vf2(vf1)(x), vf2(vf2(x)),(vf2 + vf2)(x))
        usebr <- c(vf1(x) + vf2(x),vf1(vf2(x)), vf2(vf1(x)), vf2(vf2(x)), vf2(x) + vf2(x))
        expect_close(usevf,usebr)
    }
    
    l1 <-list(as.vf(function(x){x^2}), as.vf(function(x){x^2 + 1}))

    l2 <- list(Abs, Sign, Sqrt, Ceiling, Floor, Trunc, Cummax, Cummin,
               Cumprod, Cumsum, Log, Log10, Log2, Log1p, Acos, Acosh,
               Asin, Asinh, Atan, Atanh, Exp, Expm1, Cos, Cosh, Cospi,
               Sin, Sinh, Sinpi, Tan, Tanh, Tanpi, Gamma, Lgamma,
               Digamma, Trigamma)

    for(i in seq_along(l1)){
        for(j in seq_along(l2)){
            testvf(l1[[i]], l2[[j]], 1.1)
        }
    }

})
