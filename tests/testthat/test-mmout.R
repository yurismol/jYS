test_that("mMOUT works", {
    set.seed(42)
    n <- 80
    df <- data.frame(
        grp = factor(sample(c("G1", "G2"), n, replace = TRUE)),
        x1 = rnorm(n),
        x2 = rnorm(n),
        x3 = rnorm(n)
    )
    # Inject outliers
    df$x1[c(1, 2)] <- c(10, -10)
    df$x2[c(1, 2)] <- c(10, -10)
    
    # 1. Test robust MCD method without grouping
    res_robust <- jYS::mMOUT(
        data = df,
        vars = c("x1", "x2", "x3"),
        group = NULL,
        method = "robust",
        alpha = "0.01",
        outind = TRUE
    )
    
    expect_s3_class(res_robust, "mMOUTResults")
    expect_s3_class(res_robust$stat, "Table")
    expect_s3_class(res_robust$oind, "Table")
    expect_equal(res_robust$stat$rowCount, 1)
    expect_equal(res_robust$oind$rowCount, 1)
    
    # 2. Test classical Mahalanobis distance with grouping
    res_classic <- jYS::mMOUT(
        data = df,
        vars = c("x1", "x2", "x3"),
        group = "grp",
        method = "classic",
        alpha = "0.05",
        outind = TRUE
    )
    
    expect_s3_class(res_classic, "mMOUTResults")
    expect_s3_class(res_classic$stat, "Table")
    expect_s3_class(res_classic$oind, "Table")
    expect_equal(res_classic$stat$rowCount, 2)
})
