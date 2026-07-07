test_that("mCOR works", {
    set.seed(42)
    df <- data.frame(
        x1 = rnorm(50),
        x2 = rnorm(50),
        x3 = rnorm(50),
        x4 = rnorm(50)
    )
    
    # Run basic mCOR
    res <- jYS::mCOR(
        data = df,
        vars = c("x1", "x2", "x3", "x4")
    )
    
    expect_s3_class(res, "mCORResults")
    expect_s3_class(res$matrix, "Table")
    expect_equal(res$matrix$rowCount, 4)
    
    # Test GLASSO and Hub table
    res_glasso <- jYS::mCOR(
        data = df,
        vars = c("x1", "x2", "x3", "x4"),
        group = NULL,
        selgroup = NULL,
        glasso = TRUE,
        glassoTable = TRUE,
        glassoHub = TRUE
    )
    
    expect_s3_class(res_glasso$glassoGroup$glassoTable, "Table")
    expect_s3_class(res_glasso$glassoGroup$glassoHubTable, "Table")
    expect_equal(res_glasso$glassoGroup$glassoTable$rowCount, 4)
})
