test_that("mPWR works", {
    # Test power calculation for independent samples
    res <- jYS::mPWR(
        calc = "n",
        design = "independent",
        es = 0.5,
        power = 0.8,
        alt = "two.sided",
        alpha = 0.05
    )
    
    expect_s3_class(res, "mPWRResults")
    expect_s3_class(res$powerTable, "Table")
    expect_equal(res$powerTable$rowCount, 1)
    
    # Test power calculation for Welch
    res_welch <- jYS::mPWR(
        calc = "power",
        design = "welch",
        es = 0.5,
        n = 40,
        var_ratio = 1.5,
        n_ratio = 1.2
    )
    
    expect_s3_class(res_welch, "mPWRResults")
    expect_s3_class(res_welch$powerTable, "Table")
})
