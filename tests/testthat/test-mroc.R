test_that("mROC works", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mroc\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mROC_fn <- mROC

    set.seed(42)
    class_val <- sample(c("A", "B"), 100, replace = TRUE)
    df <- data.frame(
        class = as.factor(class_val),
        x1 = rnorm(100) + ifelse(class_val == "B", 1, 0),
        x2 = rnorm(100) + ifelse(class_val == "B", 1.5, 0)
    )
    
    # Test mROC (ROC classification)
    res <- mROC_fn(
        data = df,
        vars = c("x1", "x2"),
        class = "class",
        groups = NULL,
        selgroup = NULL,
        theBest = "bssY",
        cmpDeLong = "succ",
        showTable = TRUE
    )
    
    expect_s3_class(res, "mROCResults")
    expect_s3_class(res$rocTable, "Table")
    expect_equal(res$rocTable$rowCount, 2)
})
