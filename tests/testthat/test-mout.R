test_that("mOUT works", {
    set.seed(42)
    df <- data.frame(
        g = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x = c(rnorm(95), 10, -10, 15, -15, 20)  # Add outliers
    )
    
    # Test mOUT (IQR method)
    res <- jYS::mOUT(
        data = df,
        vars = "x",
        outlcheck = "IQR",
        fence = "1.5"
    )
    
    expect_s3_class(res, "mOUTClass")
    expect_s3_class(res$results$estim, "Table")
    
    # Test mOUT (Z-score method) with grouping
    res_zs <- jYS::mOUT(
        data = df,
        vars = "x",
        group = "g",
        outlcheck = "ZS",
        tholdZS = "3.0"
    )
    
    expect_s3_class(res_zs, "mOUTClass")
    expect_s3_class(res_zs$results$estim, "Table")
})
