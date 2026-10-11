test_that("mPCA works", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mpca\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mPCA_fn <- if (exists("mPCA", mode = "function")) mPCA else jYS::mPCA

    set.seed(42)
    df <- data.frame(
        g = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rnorm(100),
        x4 = rnorm(100)
    )
    
    # Test PCA without classVar
    res <- mPCA_fn(
        data = df,
        vars = c("x1", "x2", "x3", "x4"),
        group = NULL,
        selgroup = NULL,
        classVar = NULL,
        npc = 2,
        screePlot = TRUE,
        loadingsTable = TRUE
    )
    
    expect_s3_class(res, "mPCAResults")
    expect_s3_class(res$loadingsTable, "Table")
    expect_equal(res$loadingsTable$rowCount, 4)
    
    # Test PCA with classVar (K-means comparison, CV regression, and ROC table)
    res_group <- mPCA_fn(
        data = df,
        vars = c("x1", "x2", "x3", "x4"),
        group = NULL,
        selgroup = NULL,
        classVar = "g",
        npc = 2,
        kmeansTable = TRUE,
        regTable = TRUE,
        regKFold = 3,
        show_roc_table = TRUE
    )
    
    expect_s3_class(res_group, "mPCAResults")
    expect_s3_class(res_group$kmeansTable, "Table")
    expect_s3_class(res_group$regTable, "Table")
    expect_s3_class(res_group$rocTable, "Table")
    expect_true(res_group$rocTable$rowCount >= 2)
})

