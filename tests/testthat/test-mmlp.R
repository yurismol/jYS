test_that("mMLP works and binary CV accuracy matches encoding", {
    set.seed(42)
    x1 <- rnorm(100)
    x2 <- rnorm(100)
    y_prob <- 1 / (1 + exp(-(2 * x1 + 2 * x2)))
    y <- factor(ifelse(y_prob > 0.5, "B", "A"), levels = c("A", "B"))
    df <- data.frame(y = y, x1 = x1, x2 = x2)
    
    # Test mMLP classifier
    res <- jYS::mMLP(
        data = df,
        dep = "y",
        covs = c("x1", "x2"),
        factors = NULL,
        hidden_structure = "5,3",
        activation = "relu",
        partition = "kfold",
        cv_folds = 3,
        maxit = 100,
        seed = 42,
        show_imp = TRUE
    )
    
    expect_s3_class(res, "mMLPResults")
    expect_s3_class(res$statsTable, "Table")
    expect_s3_class(res$infoTable, "Table")
    
    info_df <- res$infoTable$asDF
    train_acc_str <- info_df$value[grepl("Training Accuracy", info_df$metric)]
    cv_acc_str <- info_df$value[grepl("K-Fold CV Accuracy", info_df$metric)]
    
    train_acc <- as.numeric(sub("%", "", train_acc_str))
    cv_acc <- as.numeric(sub("%", "", cv_acc_str))
    
    expect_true(!is.na(cv_acc) && cv_acc > 50)
    expect_true(abs(train_acc - cv_acc) < 25)
})
