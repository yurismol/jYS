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

test_that("mLR supports Firth's penalized likelihood", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists(".renderClassifierRocPlot", mode = "function")) {
        r_roc <- list.files(c("R", "../../R", "../R"), pattern = "^classifier_roc\\.R$", full.names = TRUE)
        if (length(r_roc) > 0) source(r_roc[1])
    }
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mlr\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mLR_fn <- mLR

    df_sep <- data.frame(
        y = factor(c(rep("0", 15), rep("1", 15))),
        x = c(seq(-15, -1), seq(1, 15))
    )

    if (requireNamespace("logistf", quietly = TRUE)) {
        res <- mLR_fn(
            data = df_sep,
            dep = "y",
            refLevel = "0",
            covs = "x",
            factors = NULL,
            group = NULL,
            selgroup = NULL,
            firth = TRUE,
            method = "manual",
            partition = "kfold",
            showForest = TRUE
        )
        expect_s3_class(res, "mLRResults")
        coef_df <- res$coeffTable$asDF
        expect_true(all(is.finite(coef_df$est)))
        info_df <- res$infoTable$asDF
        expect_true(any(grepl("Firth", info_df$value)))
        expect_true("firth_note" %in% names(res$coeffTable$notes))
        expect_match(res$coeffTable$notes[["firth_note"]]$note, "penalized maximum likelihood")
        expect_match(res$coeffTable$notes[["firth_note"]]$note, "signed z-statistics")
        valid_idx <- which(!is.na(coef_df$p) & coef_df$p > 0 & coef_df$p <= 1)
        expect_true(length(valid_idx) > 0)
        expected_z <- sign(coef_df$est[valid_idx]) * sqrt(stats::qchisq(coef_df$p[valid_idx], df = 1, lower.tail = FALSE))
        expect_equal(coef_df$z[valid_idx], expected_z, tolerance = 1e-4)

        # Stepwise with Firth includes firth_step_note
        res_step <- mLR_fn(
            data = infert,
            dep = "case",
            refLevel = "0",
            covs = c("spontaneous", "induced"),
            factors = NULL,
            group = NULL,
            selgroup = NULL,
            firth = TRUE,
            method = "forward",
            partition = "kfold"
        )
        expect_true("firth_step_note" %in% names(res_step$coeffTable$notes))

        # Multinomial outcome excludes Firth from infoTable
        df_multi <- data.frame(
            y = factor(c(rep("A", 10), rep("B", 10), rep("C", 10))),
            x = rnorm(30)
        )
        res_multi <- mLR_fn(
            data = df_multi,
            dep = "y",
            refLevel = "A",
            covs = "x",
            factors = NULL,
            group = NULL,
            selgroup = NULL,
            firth = TRUE,
            method = "manual",
            partition = "kfold"
        )
        info_multi <- res_multi$infoTable$asDF
        expect_false(any(grepl("Firth", info_multi$value)))
    } else {
        expect_error(
            mLR_fn(
                data = df_sep,
                dep = "y",
                refLevel = "0",
                covs = "x",
                factors = NULL,
                group = NULL,
                selgroup = NULL,
                firth = TRUE,
                method = "manual",
                partition = "kfold"
            ),
            "logistf"
        )
    }
})
