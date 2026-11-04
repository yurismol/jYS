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

test_that("mPWR CRT visibility expression evaluates cleanly across designs", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists("mPWR", mode = "function") || !exists("mPWRClass")) {
        r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mpwr\\.[hb]\\.R$", full.names = TRUE)
        for (rf in sort(r_files)) source(rf, encoding = "UTF-8")
    }

    # 1. Non-CRT designs like survival must not crash with FALSE.sample error
    an_surv <- mPWRClass$new(
        options = mPWROptions$new(design = "survival", crt = TRUE),
        data = data.frame()
    )
    an_surv$run()
    expect_false(an_surv$results$powerTable$getColumn("k1")$visible)
    expect_false(an_surv$results$powerTable$getColumn("k2")$visible)

    # 2. one_sample design with CRT enabled
    an_os <- mPWRClass$new(
        options = mPWROptions$new(design = "one_sample", crt = TRUE),
        data = data.frame()
    )
    an_os$run()
    expect_true(an_os$results$powerTable$getColumn("k1")$visible)
    expect_true(an_os$results$powerTable$getColumn("k2")$visible)

    # 3. Legacy one.sample design string compatibility
    an_legacy <- mPWRClass$new(
        options = mPWROptions$new(design = "one.sample", crt = TRUE),
        data = data.frame()
    )
    an_legacy$run()
    expect_true(an_legacy$results$powerTable$getColumn("k1")$visible)
    expect_true(an_legacy$results$powerTable$getColumn("k2")$visible)

    # 4. Calculation execution with one_sample
    mPWR_fn <- if (exists("mPWR", mode = "function")) mPWR else jYS::mPWR
    res_os <- mPWR_fn(
        calc = "power",
        design = "one_sample",
        es = 0.5,
        n = 30
    )
    expect_s3_class(res_os, "mPWRResults")
    expect_s3_class(res_os$powerTable, "Table")
    expect_equal(res_os$powerTable$rowCount, 1)
})

test_that("mPWR equivalence, non-inferiority, and superiority margin validations work", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists("mPWR", mode = "function") || !exists("mPWRClass")) {
        r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mpwr\\.[hb]\\.R$", full.names = TRUE)
        for (rf in sort(r_files)) source(rf, encoding = "UTF-8")
    }
    mPWR_fn <- if (exists("mPWR", mode = "function")) mPWR else jYS::mPWR

    # 1. Margin <= 0 rejects with informative error
    expect_error(
        mPWR_fn(calc = "n", design = "independent", es = 0.1, power = 0.8, alt = "equivalent", margin = 0),
        "strictly greater than 0"
    )

    # 2. Equivalence with calc = 'es' rejects with informative error
    expect_error(
        mPWR_fn(calc = "es", design = "independent", n = 50, power = 0.8, alt = "equivalent", margin = 0.2),
        "not supported for equivalence"
    )

    # 3. Equivalence with es >= margin rejects with informative error
    expect_error(
        mPWR_fn(calc = "n", design = "independent", es = 0.5, power = 0.8, alt = "equivalent", margin = 0.2),
        "strictly less than the hypothesis margin"
    )

    # 4. Valid equivalence calculates successfully
    res_equiv <- mPWR_fn(calc = "n", design = "independent", es = 0.1, power = 0.8, alt = "equivalent", margin = 0.2)
    expect_s3_class(res_equiv, "mPWRResults")
    expect_equal(res_equiv$powerTable$rowCount, 1)

    # 5. Valid non-inferiority calculates successfully
    res_ni <- mPWR_fn(calc = "n", design = "independent", es = 0.2, power = 0.8, alt = "non-inferior", margin = 0.2)
    expect_s3_class(res_ni, "mPWRResults")
    expect_equal(res_ni$powerTable$rowCount, 1)

    # 6. Equivalence on non-supported designs rejects with informative error
    expect_error(
        mPWR_fn(calc = "n", design = "survival", es = 0.7, power = 0.8, alt = "equivalent", margin = 0.5),
        "only supported for t-test and Wilcoxon designs"
    )
    expect_error(
        mPWR_fn(calc = "power", design = "linear", es = 0.15, n = 100, alt = "non-inferior", margin = 0.2),
        "only supported for t-test and Wilcoxon designs"
    )
})

