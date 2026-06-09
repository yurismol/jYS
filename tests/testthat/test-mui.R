test_that("mUI works", {
    set.seed(42)
    ref_val <- sample(c("A", "B"), 100, replace = TRUE)
    test_val <- rnorm(100) + ifelse(ref_val == "B", 2, 0)
    
    df <- data.frame(
        ref = as.factor(ref_val),
        test = test_val
    )
    
    # Test mUI (Uncertain Interval)
    res <- jYS::mUI(
        data = df,
        test = "test",
        ref = "ref",
        refval = "B",
        UImethod = "TGR",
        isMCI = TRUE,
        isProp = TRUE
    )
    
    expect_s3_class(res, "mUIClass")
    expect_s3_class(res$results$uitable, "Table")
    expect_s3_class(res$results$mcitable, "Table")
})
