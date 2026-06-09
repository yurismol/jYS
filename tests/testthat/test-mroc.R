test_that("mROC works", {
    set.seed(42)
    class_val <- sample(c("A", "B"), 100, replace = TRUE)
    df <- data.frame(
        class = as.factor(class_val),
        x1 = rnorm(100) + ifelse(class_val == "B", 1, 0),
        x2 = rnorm(100) + ifelse(class_val == "B", 1.5, 0)
    )
    
    # Test mROC (ROC classification)
    res <- jYS::mROC(
        data = df,
        vars = c("x1", "x2"),
        class = "class",
        theBest = "bssY",
        cmpDeLong = "succ"
    )
    
    expect_s3_class(res, "mROCClass")
    expect_s3_class(res$results$estim, "Table")
    expect_equal(res$results$estim$rowCount, 2)
})
