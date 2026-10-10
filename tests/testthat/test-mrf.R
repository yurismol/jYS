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
    
    expect_s3_class(res_kfold, "mRFResults")
    expect_s3_class(res_kfold$infoTable, "Table")
    expect_s3_class(res_kfold$importanceTable, "Table")
    expect_s3_class(res_kfold$matrixTable, "Table")
    expect_equal(res_kfold$importanceTable$rowCount, 4)
    
    # 2. Test holdout partition
    res_holdout <- jYS::mRF(
        data = df,
        dep = "y",
        covs = c("x1", "x2"),
        factors = NULL,
        ntree = 50,
        partition = "holdout",
        val_split = 30,
        show_matrix = TRUE,
        seed = 42
    )
    
    expect_s3_class(res_holdout, "mRFResults")
    expect_s3_class(res_holdout$infoTable, "Table")
    expect_s3_class(res_holdout$matrixTable, "Table")

    # 3. Test OOB error curve with seed known to have missing OOB observations at small tree counts
    res_oob_curve <- jYS::mRF(
        data = df,
        dep = "y",
        covs = c("x1", "x2"),
        factors = NULL,
        ntree = 50,
        partition = "oob",
        show_matrix = TRUE,
        show_oob_curve = TRUE,
        seed = 123
    )

    expect_s3_class(res_oob_curve, "mRFResults")
    expect_s3_class(res_oob_curve$infoTable, "Table")
    expect_s3_class(res_oob_curve$matrixTable, "Table")
    curve_state <- res_oob_curve$oobCurvePlot$state
    expect_true(is.data.frame(curve_state))
    expect_true(nrow(curve_state) > 0)
    expect_true(all(c("Trees", "OOB_Error") %in% colnames(curve_state)))

    # 4. Test low tree count (e.g. ntree = 10)
    res_small_tree <- jYS::mRF(
        data = df,
        dep = "y",
        covs = "x1",
        factors = NULL,
        ntree = 10,
        partition = "oob",
        show_matrix = TRUE,
        show_oob_curve = TRUE,
        seed = 123
    )
    expect_s3_class(res_small_tree, "mRFResults")
})

