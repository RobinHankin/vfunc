test_that("Test suite aad",{  # testing arith ANY vf methods

    expect_close <- function(x,y){expect_true(all(abs(x-y) < 1e-8))}

    testANYmethod <- function(vf1,val,x){
        usevf <- c(
        (vf1 + val)(x), (vf1 * val)(x), (vf1 - val)(x), (vf1 / val)(x),
        (val + vf1)(x), (val * vf1)(x), (val - vf1)(x), (val / vf1)(x)
        )

        usebr <- c(
        vf1(x) + val, vf1(x) * val, vf1(x) - val, vf1(x) / val,
        val + vf1(x), val * vf1(x), val - vf1(x), val / vf1(x)
        )

        expect_close(usevf,usebr)
    }
   
    f <- as.vf(function(x){x^2 + 1})
    g <- as.vf(function(x){1/x})

    testANYmethod(f, 2:10, 1.3)
    testANYmethod(Sin, 2:10, 1.3)
    
})
