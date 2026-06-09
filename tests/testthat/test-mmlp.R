test_that("mMLP works", {
    set.seed(42)
    df <- data.frame(
        y = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rnorm(100)
    )
    
    # Test mMLP classifier
    res <- jYS::mMLP(
        data = df,
        dep = "y",
        covs = c("x1", "x2", "x3"),
        hidden_structure = "5,3",
        activation = "relu",
        partition = "kfold",
        cv_folds = 3,
        maxit = 20
    )
    
    expect_s3_class(res, "mMLPClass")
    expect_s3_class(res$results$statsTable, "Table")
    expect_s3_class(res$results$infoTable, "Table")
})
