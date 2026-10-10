test_that("mSNP works and supports local source", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    set.seed(42)
    n <- 120
    df <- data.frame(
        rs1 = factor(sample(c("AA", "AG", "GG"), n, replace = TRUE, prob = c(0.25, 0.5, 0.25))),
        rs2 = factor(sample(c("CC", "CT", "TT"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
        outcome = factor(sample(c("0", "1"), n, replace = TRUE)),
        gender = factor(sample(c("M", "F"), n, replace = TRUE))
    )
    
    # 1. Test basic HWE and frequency tables
    res_hwe <- mSNP_fn(
        data = df,
        vars = c("rs1", "rs2"),
        group = NULL,
        outcome = NULL,
        covs = NULL,
        gender = NULL,
        freqTable = TRUE,
        hwTests = TRUE
    )
    
    expect_s3_class(res_hwe, "mSNPResults")
    expect_s3_class(res_hwe$hweGroup$freqTable, "Table")
    expect_s3_class(res_hwe$hweGroup$hwTable, "Table")
    expect_equal(res_hwe$hweGroup$freqTable$rowCount, 2)
    expect_equal(res_hwe$hweGroup$hwTable$rowCount, 2)
    
    # 2. Test genetic association and LD analysis
    res_assoc <- mSNP_fn(
        data = df,
        vars = c("rs1", "rs2"),
        group = NULL,
        outcome = "outcome",
        covs = NULL,
        gender = NULL,
        assocEnable = TRUE,
        ldEnable = TRUE,
        ldTable = TRUE
    )
    
    expect_s3_class(res_assoc, "mSNPResults")
    expect_s3_class(res_assoc$assocGroup$assocTable, "Table")
    expect_s3_class(res_assoc$ldGroup$ldTable, "Table")
    expect_true(res_assoc$assocGroup$assocTable$rowCount > 0)
    expect_true(res_assoc$ldGroup$ldTable$rowCount > 0)
})

test_that("mSNP correctly estimates risk allele OR > 1", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    # Test that risk alleles yield OR > 1 for allelic, dominant, and log-additive models
    set.seed(42)
    n_ctrl <- 100
    n_case <- 100
    
    # In controls, allele A is dominant and B is rare: 90 AA, 10 AB, 0 BB
    # In cases, minor allele B is enriched: 10 AA, 50 AB, 40 BB
    df_risk <- data.frame(
        rs_risk = factor(c(rep("AA", 90), rep("AB", 10), rep("AA", 10), rep("AB", 50), rep("BB", 40))),
        outcome = factor(c(rep("Control", n_ctrl), rep("Case", n_case)), levels = c("Control", "Case"))
    )
    
    res_risk <- mSNP_fn(
        data = df_risk,
        vars = "rs_risk",
        group = NULL,
        outcome = "outcome",
        covs = NULL,
        gender = NULL,
        assocEnable = TRUE,
        geneticModel = "all"
    )
    
    expect_s3_class(res_risk, "mSNPResults")
    assoc_table <- res_risk$assocGroup$assocTable
    expect_s3_class(assoc_table, "Table")
    
    df_assoc <- assoc_table$asDF
    or_allelic <- df_assoc$or_val[grepl("^Allelic", df_assoc$model, ignore.case = TRUE)]
    or_dom     <- df_assoc$or_val[grepl("^Dominant", df_assoc$model, ignore.case = TRUE)]
    or_logadd  <- df_assoc$or_val[grepl("^Log-additive", df_assoc$model, ignore.case = TRUE)]
    
    expect_true(length(or_allelic) > 0 && all(!is.na(or_allelic)) && all(or_allelic > 1.0))
    expect_true(length(or_dom) > 0 && all(!is.na(or_dom)) && all(or_dom > 1.0))
    expect_true(length(or_logadd) > 0 && all(!is.na(or_logadd)) && all(or_logadd > 1.0))
})

test_that("mSNP safely caps MDR order and sets footnote when order > marker count (Item 5)", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    set.seed(42)
    n <- 100
    df <- data.frame(
        rs1 = factor(sample(c("AA", "AG", "GG"), n, replace = TRUE)),
        outcome = factor(sample(c("0", "1"), n, replace = TRUE))
    )

    # user specifies mdrOrder = 2, but only 1 SNP marker is provided
    res_mdr <- mSNP_fn(
        data = df,
        vars = "rs1",
        group = NULL,
        outcome = "outcome",
        covs = NULL,
        gender = NULL,
        mdrEnable = TRUE,
        mdrOrder = 2,
        mdrFolds = 5
    )

    expect_s3_class(res_mdr, "mSNPResults")
    tbl <- res_mdr$mdrGroup$mdrBestTable
    expect_s3_class(tbl, "Table")
    # Exactly 1 row for order 1 model since max order was capped to 1
    expect_equal(tbl$rowCount, 1)
    # Check that footnote note 'order_note' is present
    expect_true("order_note" %in% names(tbl$notes))
})
