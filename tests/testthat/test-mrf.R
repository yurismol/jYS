test_that("mRF works, differentiates Training and OOB metrics, and supports filtered row predictions", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists(".renderClassifierRocPlot", mode = "function")) {
        r_roc <- list.files(c("R", "../../R", "../R"), pattern = "^classifier_roc\\.R$", full.names = TRUE)
        if (length(r_roc) > 0) source(r_roc[1])
    }
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mrf\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mRF_fn <- mRF

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
    res_kfold <- mRF_fn(
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
    res_holdout <- mRF_fn(
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

    # 3. Test OOB error curve with distinct Training and OOB performance metrics (Item 4)
    res_oob_curve <- mRF_fn(
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
    expect_s3_class(res_oob_curve$statsTable, "Table")
    
    # Assert Item 4: Training and OOB metrics in statsTable are distinct
    stats_df <- res_oob_curve$statsTable$asDF
    expect_true(nrow(stats_df) > 0)
    expect_true("trainVal" %in% colnames(stats_df) && "cvVal" %in% colnames(stats_df))
    expect_false(identical(stats_df$trainVal, stats_df$cvVal))

    curve_state <- res_oob_curve$oobCurvePlot$state
    expect_true(is.data.frame(curve_state))
    expect_true(nrow(curve_state) > 0)
    expect_true(all(c("Trees", "OOB_Error") %in% colnames(curve_state)))

    # 4. Test low tree count (e.g. ntree = 10)
    res_small_tree <- mRF_fn(
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

    # 5. Test saving predictions with missing/filtered rows (Item 3)
    df_filtered <- df
    df_filtered$x1[1:10] <- NA
    opts <- mRFOptions$new(
        dep = "y",
        covs = c("x1", "x2"),
        ntree = 50,
        partition = "oob",
        seed = 42
    )
    opt_pc <- opts$option("predClass")
    opt_pc$value <- list(value = TRUE)
    opt_pp <- opts$option("predProb")
    opt_pp$value <- list(value = TRUE)

    analysis <- mRFClass$new(options = opts, data = df_filtered)
    analysis$run()

    expect_true(analysis$results$predClass$isFilled())
    expect_true(analysis$results$predProb$isFilled())
})
