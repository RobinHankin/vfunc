test_that("Test suite aaa.R",{

    expect_close <- function(x,y){expect_true(all(abs(x-y) < 1e-8))}

    f <- as.vf(function(x){x^2})
    g <- as.vf(function(x){1/(1+x^2)})

    expect_true(f(3) ==  9)
    expect_true(f(4) == 16)
    expect_true(f(5) == 25)

    expect_true(f(f(3)) == 81)
    expect_true(f(f)(3) == 81)

    expect_close(f(3),9)


    for(x in c(0.2,0.4)){
        expect_close((f+g)(x), x^2 + 1/(1+x^2))
        expect_close((f-g)(x), x^2 - 1/(1+x^2))
        expect_close((f*g)(x), x^2 * 1/(1+x^2))
        expect_close((f/g)(x), x^2 *   (1+x^2))
    }
    
    expect_silent(ignore <- f(Sin))
    expect_silent(ignore <- Sin(f))
    expect_silent(ignore <- f(f))
    expect_silent(ignore <- Sin(Sin))

    vf_funs <- list(f(Sin),Sin(f),f(f),Sin(Sin))
    R_funs  <- list(
        function(x){f(sin(x))},
        function(x){sin(f(x))},
        function(x){f(f(x))},
        function(x){sin(sin(x))}
    )
        
    for(i in seq_along(vf_funs)){
        for(j in seq_along(vf_funs)){
            x <- runif(1)
            expect_close(vf_funs[[i]](x), R_funs[[i]](x))
        }
    }


    f <- as.vf(function(x,y,z){x + x*y - x/z})
    g <- as.vf(function(x,y,z){x + Sin(x-y) + z})

    x <- 1.2
    y <- 1.7
    z <- 4.3
    expect_close((f + g)(x,y,z), f(x,y,z) + g(x,y,z))

    expect_close(
    ((f + g)*(f + 4 - 2*f*g))(x,y,z),
    (f(x,y,z) + g(x,y,z))*(f(x,y,z) + 4 - 2*f(x,y,z)*g(x,y,z))
    )

    expect_close(
    ((f + g)*(f + 4 - 2*f*g))(x/z,y+z,z-x^2+1),
    (f(x/z,y+z,z-x^2+1) + g(x/z,y+z,z-x^2+1))*(f(x/z,y+z,z-x^2+1) + 4 - 2*f(x/z,y+z,z-x^2+1)*g(x/z,y+z,z-x^2+1))
    )

    expect_close(
    ((f + g)*(f + 4 - 2*f*g))(x/z,y+z,z-x^2+f(x,x,y)),
    (f(x/z,y+z,z - x^2 + f(x,x,y)) + g(x/z, y+z, z - x^2 + f(x,x,y))) * (f(x/z,y+z,z-x^2 + f(x,x,y)) + 4 - 2*f(x/z,y+z,z-x^2 + f(x,x,y))*g(x/z,y+z,z-x^2+ f(x,x,y)))
    )

    
})
