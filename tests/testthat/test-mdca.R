test_that("mDCA works", {
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
    res <- jYS::mDCA(
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
    
    expect_s3_class(res, "mDCAClass")
    expect_s3_class(res$results$dcaTable, "Table")
    expect_s3_class(res$results$windowTable, "Table")
    expect_s3_class(res$results$calTable, "Table")
    expect_true(res$results$dcaTable$rowCount > 0)
    expect_true(res$results$windowTable$rowCount > 0)
    expect_true(res$results$calTable$rowCount > 0)
    
    # 2. DCA with recalibration and non-zero harm
    res_harm <- jYS::mDCA(
        data = df,
        dep = "outcome",
        targetLevel = "1",
        probs = "prob",
        harm = 0.02,
        recalibrate = TRUE,
        showDcaTable = TRUE,
        showCalTable = TRUE
    )
    
    expect_s3_class(res_harm, "mDCAClass")
    expect_s3_class(res_harm$results$dcaTable, "Table")
    expect_s3_class(res_harm$results$calTable, "Table")
})
