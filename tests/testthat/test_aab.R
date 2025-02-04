test_that("Test suite aab",{

    expect_close <- function(x,y){expect_true(all(abs(x-y) < 1e-8))}

    f <- as.vf(function(x){x^2})
    g <- as.vf(function(x){x^2 + 1})


    ## test vf.vf.eq() et seq:

    expect_true ((f == f)(5))
    expect_true ((g == g)(5))
    expect_false((f == g)(5))

    expect_false((f != f)(5))
    expect_false((g != g)(5))
    expect_true ((f != g)(5))
    expect_true ((g != f)(5))

    expect_false((f >  g)(5))
    expect_true ((g >  f)(5))

    expect_false((f >= g)(5))
    expect_true ((g >= f)(5))

    expect_true ((f <  g)(5))
    expect_false((g <  f)(5))

    expect_true ((f <= g)(5))
    expect_false((g <= f)(5))


    ##  now .vf.ANY.eq() et seq
    
    expect_true ((f == 25)(5))
    expect_false((f == 26)(5))

    expect_true ((f != 24)(5))
    expect_false((f != 25)(5))

    expect_true ((f >  24)(5))
    expect_false((f >  25)(5))
    expect_false((f >  26)(5))

    expect_true ((f >= 24)(5))
    expect_true ((f >= 25)(5))
    expect_false((f >= 26)(5))

    expect_false((f <  24)(5))
    expect_false((f <  25)(5))
    expect_true ((f <  26)(5))

    expect_false((f <= 24)(5))
    expect_true ((f <= 25)(5))
    expect_true ((f <= 26)(5))


    ##  now .ANY.vf.eq() et seq
    
    expect_true ((25 == f)(5))
    expect_false((26 == f)(5))

    expect_true ((24 != f)(5))
    expect_false((25 != f)(5))

    expect_false((24 >  f)(5))
    expect_false((25 >  f)(5))
    expect_true ((26 >  f)(5))

    expect_false((24 >= f)(5))
    expect_true ((25 >= f)(5))
    expect_true ((26 >= f)(5))

    expect_true ((24 <  f)(5))
    expect_false((25 <  f)(5))
    expect_false((26 <  f)(5))

    expect_true ((24 <= f)(5))
    expect_true ((25 <= f)(5))
    expect_false((26 <= f)(5))
    
})
