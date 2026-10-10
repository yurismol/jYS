test_that("mLR works and supports refLevel, deterministic seed, and filtered row predictions", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists(".renderClassifierRocPlot", mode = "function")) {
        r_roc <- list.files(c("R", "../../R", "../R"), pattern = "^classifier_roc\\.R$", full.names = TRUE)
        if (length(r_roc) > 0) source(r_roc[1])
    }
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mlr\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mLR_fn <- mLR

    set.seed(42)
    df <- data.frame(
        y = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rnorm(100)
    )
    
    # 1. Test basic mLR (Logistic Regression)
    res <- mLR_fn(
        data = df,
        dep = "y",
        refLevel = "A",
        covs = c("x1", "x2", "x3"),
        factors = NULL,
        group = NULL,
        selgroup = NULL,
        method = "manual",
        partition = "kfold",
        cv_folds = 3,
        show_formula = TRUE
    )
    
    expect_s3_class(res, "mLRResults")
    expect_s3_class(res$infoTable, "Table")
    expect_s3_class(res$coeffTable, "Table")
    expect_s3_class(res$formulaHtml, "Html")
    
    # 2. Test ElasticNet and holdout validation with seed reproducibility (Item 6)
    res_elastic1 <- mLR_fn(
        data = df,
        dep = "y",
        refLevel = "A",
        covs = c("x1", "x2", "x3"),
        factors = NULL,
        group = NULL,
        selgroup = NULL,
        method = "elastic",
        elastic_alpha = 0.5,
        partition = "holdout",
        val_split = 30,
        seed = 123
    )
    
    res_elastic2 <- mLR_fn(
        data = df,
        dep = "y",
        refLevel = "A",
        covs = c("x1", "x2", "x3"),
        factors = NULL,
        group = NULL,
        selgroup = NULL,
        method = "elastic",
        elastic_alpha = 0.5,
        partition = "holdout",
        val_split = 30,
        seed = 123
    )
    
    expect_s3_class(res_elastic1, "mLRResults")
    expect_s3_class(res_elastic2, "mLRResults")
    df_coef1 <- res_elastic1$coeffTable$asDF
    df_coef2 <- res_elastic2$coeffTable$asDF
    expect_equal(df_coef1$estimate, df_coef2$estimate)

    # 3. Test saving predictions with missing/filtered rows (Item 3)
    df_filtered <- df
    df_filtered$x1[1:10] <- NA # First 10 rows excluded as incomplete cases
    opts <- mLROptions$new(
        dep = "y",
        refLevel = "A",
        covs = c("x1", "x2")
    )
    opt_pc <- opts$option("predClass")
    opt_pc$value <- list(value = TRUE)
    opt_pp <- opts$option("predProb")
    opt_pp$value <- list(value = TRUE)

    analysis <- mLRClass$new(options = opts, data = df_filtered)
    analysis$run()
    
    expect_true(analysis$results$predClass$isFilled())
    expect_true(analysis$results$predProb$isFilled())
})
