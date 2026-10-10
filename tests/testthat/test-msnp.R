test_that("mSNP works", {
    set.seed(42)
    n <- 120
    df <- data.frame(
        rs1 = factor(sample(c("AA", "AG", "GG"), n, replace = TRUE, prob = c(0.25, 0.5, 0.25))),
        rs2 = factor(sample(c("CC", "CT", "TT"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
        outcome = factor(sample(c("0", "1"), n, replace = TRUE)),
        gender = factor(sample(c("M", "F"), n, replace = TRUE))
    )
    
    # 1. Test basic HWE and frequency tables
    res_hwe <- jYS::mSNP(
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
    res_assoc <- jYS::mSNP(
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
    
    res_risk <- jYS::mSNP(
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
