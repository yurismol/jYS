test_that("mRF works", {
    set.seed(42)
    n <- 100
    df <- data.frame(
        y = factor(sample(c("ClassA", "ClassB"), n, replace = TRUE)),
        f1 = factor(sample(c("Low", "High"), n, replace = TRUE)),
        x1 = rnorm(n),
        x2 = rnorm(n),
        x3 = rnorm(n)
    )
    
    # 1. Test basic classification with k-fold cross-validation and importance
    res_kfold <- jYS::mRF(
        data = df,
        dep = "y",
        covs = c("x1", "x2", "x3"),
        factors = "f1",
        ntree = 50,
        partition = "kfold",
        cv_folds = 3,
        show_imp = TRUE,
        show_matrix = TRUE,
        seed = 42
    )
    
    expect_s3_class(res_kfold, "mRFClass")
    expect_s3_class(res_kfold$results$infoTable, "Table")
    expect_s3_class(res_kfold$results$importanceTable, "Table")
    expect_s3_class(res_kfold$results$matrixTable, "Table")
    expect_equal(res_kfold$results$importanceTable$rowCount, 4)
    
    # 2. Test holdout partition
    res_holdout <- jYS::mRF(
        data = df,
        dep = "y",
        covs = c("x1", "x2"),
        ntree = 50,
        partition = "holdout",
        val_split = 30,
        show_matrix = TRUE,
        seed = 42
    )
    
    expect_s3_class(res_holdout, "mRFClass")
    expect_s3_class(res_holdout$results$infoTable, "Table")
    expect_s3_class(res_holdout$results$matrixTable, "Table")
})
