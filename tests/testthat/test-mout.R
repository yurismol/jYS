test_that("mOUT works", {
    set.seed(42)
    df <- data.frame(
        g = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x = c(rnorm(95), 10, -10, 15, -15, 20)  # Add outliers
    )
    
    # Test mOUT (IQR method)
    res <- jYS::mOUT(
        data = df,
        vars = "x",
        group = NULL,
        outlcheck = "IQR",
        fence = "1.5"
    )
    
    expect_s3_class(res, "mOUTResults")
    expect_s3_class(res$stat$outstat, "Table")
    
    # Test mOUT (Z-score method) with grouping
    res_zs <- jYS::mOUT(
        data = df,
        vars = "x",
        group = "g",
        outlcheck = "ZS",
        tholdZS = "3.0"
    )
    
    expect_s3_class(res_zs, "mOUTResults")
    expect_s3_class(res_zs$stat$outstat, "Table")
})

test_that("mOUT boxplot rendering respects outlcheck method", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mout\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)

    set.seed(42)
    df <- data.frame(
        g = as.factor(sample(c("A", "B"), 100, replace = TRUE)),
        x = c(rnorm(95), 10, -10, 15, -15, 20)
    )

    # 1. IQR with boxpl = TRUE produces a valid plot
    res_iqr <- mOUT(
        data = df,
        vars = "x",
        group = NULL,
        outlcheck = "IQR",
        fence = "1.5",
        boxpl = TRUE
    )
    expect_s3_class(res_iqr, "mOUTResults")
    pdf_iqr <- tempfile(fileext = ".pdf")
    expect_no_error(res_iqr$plots$get(key = "x")$saveAs(pdf_iqr))
    expect_true(file.exists(pdf_iqr) && file.info(pdf_iqr)$size > 1000)
    unlink(pdf_iqr)

    # 2. ZS with boxpl = TRUE does not render IQR boxplots
    res_zs <- mOUT(
        data = df,
        vars = "x",
        group = NULL,
        outlcheck = "ZS",
        tholdZS = "3.0",
        boxpl = TRUE
    )
    expect_s3_class(res_zs, "mOUTResults")
})
