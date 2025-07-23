test_that("Test suite aae",{  # testing pow() and "^"

    expect_close <- function(x,y){expect_true(all(abs(x-y) < 1e-8))}

    f <- as.vf(function(x){x^2 + 1})
    g <- as.vf(function(x){1/x})

    test_f <- function(f,x){
        expect_close(pow(f,0)(x),        x    )
        expect_close(pow(f,1)(x),      f(x   ))
        expect_close(pow(f,2)(x),    f(f(x  )))
        expect_close(pow(f,3)(x),  f(f(f(x ))))
        expect_close(pow(f,4)(x),f(f(f(f(x)))))
    }

    test_f(f,3.2)
    test_f(g,3.2)
    test_f(Sin, 3.2)
    test_f(f(Sin),0.3)


    expect_true((f^g)(1) == f(1)^g(1))
    expect_true((g^f)(1) == g(1)^f(1))
    expect_true((2^f)(3) == 2^f(3))
    expect_true((+f)(3) == f(3))
})
