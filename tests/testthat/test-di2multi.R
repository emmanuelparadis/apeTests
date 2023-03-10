test_that("Random Yule trees", {
    x <- c(rcoal(100), rcoal(100), rcoal(100))
    y <- .compressTipLabel(x)
    z <- reorder(y, "postorder")

    x_m <- di2multi(x, .1)
    y_m <- di2multi(y, .1)
    z_m <- di2multi(z, .1) # this caused an error

    expect_true(all(Nnode(x) >= Nnode(x_m)))
    expect_equal(Nnode(x_m), Nnode(y_m))
    expect_equal(y_m, z_m)

    phy <- z_m
    labs <- attr(phy, "TipLabel")
    oc <- oldClass(phy)
    class(phy) <- NULL

    set.seed(42)
    x_d <- multi2di(x_m, random = TRUE, equiprob = TRUE)
    set.seed(42)
    y_d <- multi2di(y_m, random = TRUE, equiprob = TRUE)
    expect_equal(.compressTipLabel(x_d), y_d)

    # Tests tip2root argument
    expect_false(all(is.ultrametric(di2multi(x, tol=1e-3))))
    expect_true(all(is.ultrametric(di2multi(x, tol=1e-3, tip2root=TRUE))))
    expect_false(all(is.ultrametric(di2multi(y, tol=1e-3))))
    expect_true(all(is.ultrametric(di2multi(y, tol=1e-3, tip2root=TRUE))))
})
