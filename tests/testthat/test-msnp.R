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
        freqTable = TRUE,
        hwTests = TRUE
    )
    
    expect_s3_class(res_hwe, "mSNPClass")
    expect_s3_class(res_hwe$results$hweGroup$freqTable, "Table")
    expect_s3_class(res_hwe$results$hweGroup$hwTable, "Table")
    expect_equal(res_hwe$results$hweGroup$freqTable$rowCount, 2)
    expect_equal(res_hwe$results$hweGroup$hwTable$rowCount, 2)
    
    # 2. Test genetic association and LD analysis
    res_assoc <- jYS::mSNP(
        data = df,
        vars = c("rs1", "rs2"),
        outcome = "outcome",
        assocEnable = TRUE,
        ldEnable = TRUE,
        ldTable = TRUE
    )
    
    expect_s3_class(res_assoc, "mSNPClass")
    expect_s3_class(res_assoc$results$assocGroup$assocTable, "Table")
    expect_s3_class(res_assoc$results$ldGroup$ldTable, "Table")
    expect_true(res_assoc$results$assocGroup$assocTable$rowCount > 0)
    expect_true(res_assoc$results$ldGroup$ldTable$rowCount > 0)
})
