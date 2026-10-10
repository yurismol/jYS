test_that("mDCA works", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mdca\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mDCA_fn <- mDCA

    set.seed(42)
    n <- 120
    x <- rnorm(n)
    pr <- 1 / (1 + exp(-x))
    y <- factor(ifelse(pr > runif(n), "1", "0"), levels = c("0", "1"))
    
    df <- data.frame(
        outcome = y,
        prob = pr,
        score = x
    )
    
    # 1. Basic DCA with probability and score predictors
    res <- mDCA_fn(
        data = df,
        dep = "outcome",
        targetLevel = "1",
        probs = "prob",
        scores = "score",
        threshMin = 0.05,
        threshMax = 0.50,
        threshStep = 0.05,
        showDcaTable = TRUE,
        showWindowTable = TRUE,
        showCalTable = TRUE
    )
    
    expect_s3_class(res, "mDCAResults")
    expect_s3_class(res$dcaTable, "Table")
    expect_s3_class(res$windowTable, "Table")
    expect_s3_class(res$calTable, "Table")
    expect_true(res$dcaTable$rowCount > 0)
    expect_true(res$windowTable$rowCount > 0)
    expect_true(res$calTable$rowCount > 0)
    
    # 2. DCA with recalibration and non-zero harm
    res_harm <- mDCA_fn(
        data = df,
        dep = "outcome",
        targetLevel = "1",
        probs = "prob",
        scores = character(0),
        harm = 0.02,
        recalibrate = TRUE,
        showDcaTable = TRUE,
        showCalTable = TRUE
    )
    
    expect_s3_class(res_harm, "mDCAResults")
    expect_s3_class(res_harm$dcaTable, "Table")
    expect_s3_class(res_harm$calTable, "Table")
})
