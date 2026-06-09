test_that("mPCA works", {
    set.seed(42)
    df <- data.frame(
        g = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rnorm(100),
        x4 = rnorm(100)
    )
    
    # Test PCA without classVar
    res <- jYS::mPCA(
        data = df,
        vars = c("x1", "x2", "x3", "x4"),
        npc = 2,
        screePlot = TRUE,
        loadingsTable = TRUE
    )
    
    expect_s3_class(res, "mPCAClass")
    expect_s3_class(res$results$loadingsTable, "Table")
    expect_equal(res$results$loadingsTable$rowCount, 4)
    
    # Test PCA with classVar (K-means comparison and CV regression)
    res_group <- jYS::mPCA(
        data = df,
        vars = c("x1", "x2", "x3", "x4"),
        classVar = "g",
        npc = 2,
        kmeansTable = TRUE,
        regTable = TRUE,
        regKFold = 3
    )
    
    expect_s3_class(res_group, "mPCAClass")
    expect_s3_class(res_group$results$kmeansTable, "Table")
    expect_s3_class(res_group$results$regTable, "Table")
})
