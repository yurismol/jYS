test_that("mLR works", {
    set.seed(42)
    df <- data.frame(
        y = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rnorm(100)
    )
    
    # Test basic mLR (Logistic Regression)
    res <- jYS::mLR(
        data = df,
        dep = "y",
        covs = c("x1", "x2", "x3"),
        method = "manual",
        partition = "kfold",
        cv_folds = 3,
        show_formula = TRUE
    )
    
    expect_s3_class(res, "mLRClass")
    expect_s3_class(res$results$infoTable, "Table")
    expect_s3_class(res$results$coeffTable, "Table")
    expect_s3_class(res$results$formulaHtml, "Html")
    
    # Test ElasticNet and holdout validation
    res_elastic <- jYS::mLR(
        data = df,
        dep = "y",
        covs = c("x1", "x2", "x3"),
        method = "elastic",
        elastic_alpha = 0.5,
        partition = "holdout",
        val_split = 30
    )
    
    expect_s3_class(res_elastic, "mLRClass")
    expect_s3_class(res_elastic$results$coeffTable, "Table")
})
