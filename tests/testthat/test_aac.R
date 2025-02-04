test_that("Test suite aac",{  # testing arith ANY vf methods

    expect_close <- function(x,y){expect_true(all(abs(x-y) < 1e-8))}

    testvf <- function(vf1,vf2,x){
        usevf <- c((vf1 + vf2)(x), (vf1(vf2))(x), vf2(vf1)(x), vf2(vf2(x)),(vf2 + vf2)(x))
        usebr <- c(vf1(x) + vf2(x),vf1(vf2(x)), vf2(vf1(x)), vf2(vf2(x)), vf2(x) + vf2(x))
        expect_close(usevf,usebr)
    }

    testANYmethod <- function(vf1,val,x){
        usevf <- c(
        (vf1 + val)(x), (vf1 * val)(x), (vf1 - val)(x), (vf1 / val)(x),
        (val + vf1)(x), (val * vf1)(x), (val - vf1)(x), (val / vf1)(x)
        )

        usebr <- c(
        vf1(x) + val(x), vf1(x) * val(x), vf1(x) - val(x), vf1(x) / val(x),
        val(x) + vf1(x), val(x) * vf1(x), val(x) - vf1(x), val(x) / vf1(x)
        )

        expect_close(usevf,usebr)
    }

            
           
    
    f <- as.vf(function(x){x^2 + 1})
    g <- as.vf(function(x){1/x})
    
    testvf(Abs,f,0.5)
    testvf(Sign,f,0.5)
    testvf(Sqrt,f,0.5)
    testvf(Ceiling,f,0.5)
    testvf(Floor,f,0.5)
    testvf(Trunc,f,0.5)
    testvf(Cummax,f,0.5)
    testvf(Cummin,f,0.5)
    testvf(Cumprod,f,0.5)
    testvf(Cumsum,f,0.5)
    testvf(Log,f,0.5)
    testvf(Log10,f,0.5)
    testvf(Log2,f,0.5)
    testvf(Log1p,f,0.5)
    ## testvf(Acos,g,1.5)
    ## testvf(Acosh,f,0.5)
    ## testvf(Asin,f,0.5)
    ## testvf(Asinh,f,0.5)
    ## testvf(Atan,f,0.5)
    ## testvf(Atanh,f,0.5)
    testvf(Exp,f,0.5)
    testvf(Expm1,f,0.5)
    testvf(Cos,f,0.5)
    testvf(Cosh,f,0.5)
    testvf(Cospi,f,0.5)
    testvf(Sin,f,0.5)
    testvf(Sinh,f,0.5)
    ## testvf(Sinpi,f,0.5)
    testvf(Tan,f,0.5)
    testvf(Tanh,f,0.5)
    ## testvf(Tanpi,f,0.5)
    testvf(Gamma,f,0.5)
    testvf(Lgamma,f,0.5)
    testvf(Digamma,f,0.5)
    testvf(Trigamma,f,0.5)

    expect_close(Acos(Acos(0.9)), Acos(Acos)(0.9))
    expect_close(Acos(g(2)), Acos(g)(2))
    expect_close(g(Acos(0.2)), g(Acos)(0.2))
    expect_close((g + Acos)(0.2), g(0.2) + Acos(0.2))

    expect_close(Acosh(Acosh(1.9)), Acosh(Acosh)(1.9))
    expect_close(Acosh(f(2)), Acosh(f)(2))
    expect_close(f(Acosh(2.2)), f(Acosh)(2.2))
    expect_close((g + Acosh)(1.2), g(1.2) + Acosh(1.2))

    expect_close(Asin(Asin(0.2)), Asin(Asin)(0.2))
    expect_close(Asin(g(2.2)), Asin(g)(2.2))
    expect_close(f(Asin(0.2)), f(Asin)(0.2))
    expect_close((g + Asin)(0.2), g(0.2) + Asin(0.2))

    expect_close(Asinh(Asinh(1.9)), Asinh(Asinh)(1.9))
    expect_close(Asinh(f(2)), Asinh(f)(2))
    expect_close(f(Asinh(2.2)), f(Asinh)(2.2))
    expect_close((g + Asinh)(1.2), g(1.2) + Asinh(1.2))

    expect_close(Atan(Atan(0.2)), Atan(Atan)(0.2))
    expect_close(Atan(g(2.2)), Atan(g)(2.2))
    expect_close(f(Atan(0.2)), f(Atan)(0.2))
    expect_close((g + Atan)(0.2), g(0.2) + Atan(0.2))

    expect_close(Atanh(Atanh(0.2)), Atanh(Atanh)(0.2))
    expect_close(Atanh(g(2.2)), Atanh(g)(2.2))
    expect_close(f(Atanh(0.2)), f(Atanh)(0.2))
    expect_close((g + Atanh)(0.2), g(0.2) + Atanh(0.2))

    expect_close(Sinpi(Sinpi(0.9)), Sinpi(Sinpi)(0.9))
    expect_close(Sinpi(g(2)), Sinpi(g)(2))
    expect_close(g(Sinpi(0.2)), g(Sinpi)(0.2))
    expect_close((g + Sinpi)(0.2), g(0.2) + Sinpi(0.2))    

    expect_close(Tanpi(Tanpi(0.9)), Tanpi(Tanpi)(0.9))
    expect_close(Tanpi(g(0.21)), Tanpi(g)(0.21))
    expect_close(g(Tanpi(0.21)), g(Tanpi)(0.21))
    expect_close((g + Tanpi)(0.21), g(0.21) + Tanpi(0.21))    

    
})
