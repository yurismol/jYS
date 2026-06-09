test_that("mMF works", {
    set.seed(42)
    df <- data.frame(
        x1 = rnorm(50),
        x2 = rnorm(50),
        x3 = rnorm(50)
    )
    
    # Introduce some artificial missingness
    df$x1[sample(1:50, 5)] <- NA
    df$x2[sample(1:50, 5)] <- NA
    
    # Test imputation using missRanger (mR)
    res <- jYS::mMF(
        data = df,
        imputevar = c("x1", "x2"),
        learnvar = "x3",
        alg = "mR",
        setseed = TRUE,
        seed = 42
    )
    
    expect_s3_class(res, "mMFClass")
    expect_s3_class(res$results$estim$mcar, "Table")
    expect_s3_class(res$results$estim$mars, "Table")
})
