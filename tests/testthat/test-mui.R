test_that("mUI works", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mui\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mUI_fn <- mUI

    set.seed(42)
    ref_val <- sample(c("A", "B"), 100, replace = TRUE)
    test_val <- rnorm(100) + ifelse(ref_val == "B", 2, 0)
    
    df <- data.frame(
        ref = as.factor(ref_val),
        test = test_val
    )
    
    # Test mUI (Uncertain Interval)
    res <- mUI_fn(
        data = df,
        test = "test",
        ref = "ref",
        refval = "B",
        UImethod = "TGR",
        isMCI = TRUE,
        isProp = TRUE
    )
    
    expect_s3_class(res, "mUIResults")
    expect_s3_class(res$stat$uistat, "Table")
    expect_s3_class(res$stat$mcistat, "Table")
    expect_s3_class(res$stat$intstat, "Table")
    expect_equal(res$stat$intstat$rowCount, 3)
})

test_that("mUI handles NaN in normality test without error or false asterisk symbol (Item 8)", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mui\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mUI_fn <- mUI

    # Constant values in reference group "A" so shapiro.test errors out and produces NaN
    df_const <- data.frame(
        ref = factor(c(rep("A", 20), rep("B", 20)), levels = c("A", "B")),
        test = c(rep(5.0, 20), rnorm(20, mean = 10, sd = 1))
    )

    res_const <- mUI_fn(
        data = df_const,
        test = "test",
        ref = "ref",
        refval = "B",
        UImethod = "TGR",
        isMCI = TRUE,
        isProp = TRUE
    )

    expect_s3_class(res_const, "mUIResults")
    int_tbl <- res_const$stat$intstat
    expect_s3_class(int_tbl, "Table")
    int_df <- int_tbl$asDF
    expect_equal(nrow(int_df), 3)
    # The second row (group A with constant values) should have NaN for norm
    expect_true(is.nan(int_df$norm[2]))
})
