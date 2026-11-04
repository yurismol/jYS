test_that("mSNP works and supports local source", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
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
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
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
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
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

test_that("mSNP MDR properly assigns controls to test folds and evaluates testing balanced accuracy", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    set.seed(123)
    n <- 200
    rs1 <- sample(c("AA", "AG", "GG"), n, replace = TRUE)
    rs2 <- sample(c("AA", "AG", "GG"), n, replace = TRUE)
    prob_case <- ifelse(rs1 == "GG" & rs2 == "GG", 0.95, ifelse(rs1 == "AA" & rs2 == "AA", 0.05, 0.5))
    outcome <- factor(ifelse(runif(n) < prob_case, "1", "0"))

    df <- data.frame(rs1 = factor(rs1), rs2 = factor(rs2), outcome = outcome)

    res_mdr <- mSNP_fn(
        data = df,
        vars = c("rs1", "rs2"),
        group = NULL,
        outcome = "outcome",
        covs = NULL,
        gender = NULL,
        mdrEnable = TRUE,
        mdrOrder = 2,
        mdrFolds = 5
    )

    tbl <- res_mdr$mdrGroup$mdrBestTable
    expect_s3_class(tbl, "Table")
    expect_equal(tbl$rowCount, 2)
    test_ba <- tbl$getCell(rowNo = 2, "test_ba")$value
    expect_true(!is.na(test_ba) && test_ba > 0.50)
})

test_that("mSNP Manhattan and Ternary plots respect alpha and tested markers", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    set.seed(42)
    n <- 100
    df <- data.frame(
        rs1 = factor(sample(c("AA", "AG", "GG"), n, replace = TRUE)),
        rs2 = factor(sample(c("AA", "AG", "GG"), n, replace = TRUE)),
        outcome = factor(sample(c("0", "1"), n, replace = TRUE))
    )

    res <- mSNP_fn(
        data = df,
        vars = c("rs1", "rs2"),
        group = NULL,
        outcome = "outcome",
        covs = NULL,
        gender = NULL,
        alpha = "0.01",
        assocEnable = TRUE,
        manhattanPlot = TRUE,
        ternaryPlot = TRUE
    )

    # Manhattan state checks
    manh_state <- res$assocGroup$manhattanPlot$state
    expect_false(is.null(manh_state))
    expect_equal(manh_state$alpha, 0.01)
    expect_equal(manh_state$bonf_p, 0.01 / 16)

    # Ternary state checks
    tern_state <- res$hweGroup$ternaryPlot$state
    expect_false(is.null(tern_state))
    expect_equal(tern_state$alpha, 0.01)
})

test_that("mSNP X-linked allele frequencies correctly exclude male heterozygotes from denominator", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    # Construct dataset with known calls including invalid male AB
    df_x <- data.frame(
        rsX = factor(c(
            rep("AA", 20), rep("AB", 20), rep("BB", 20),
            rep("AA", 20), rep("BB", 20), rep("AB", 5)
        )),
        sex = factor(c(rep("F", 60), rep("M", 45)))
    )

    res_x <- mSNP_fn(
        data = df_x,
        vars = "rsX",
        group = NULL,
        outcome = NULL,
        covs = NULL,
        gender = "sex",
        xLinked = TRUE,
        popMetrics = TRUE,
        freqTable = TRUE
    )

    pop_tbl <- res_x$popGroup$popTable
    expect_equal(pop_tbl$rowCount, 1)
    maf <- pop_tbl$getCell(rowNo = 1, "maf")$value
    expect_equal(maf, 0.5, tolerance = 1e-4)
})

test_that("mSNP ternary plot respects exact test vs chi-square region and coloring", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    set.seed(42)
    df_tern <- data.frame(
        rs1 = factor(c(rep("AA", 30), rep("AB", 10), rep("BB", 30)))
    )
    res_exact <- mSNP_fn(
        data = df_tern,
        vars = "rs1",
        group = NULL,
        outcome = NULL,
        covs = NULL,
        gender = NULL,
        hwTests = TRUE,
        ternaryPlot = TRUE,
        ternaryRegion = "7"
    )
    expect_equal(res_exact$hweGroup$ternaryPlot$state$region_type, 7)

    res_chi <- mSNP_fn(
        data = df_tern,
        vars = "rs1",
        group = NULL,
        outcome = NULL,
        covs = NULL,
        gender = NULL,
        hwTests = TRUE,
        ternaryPlot = TRUE,
        ternaryRegion = "1"
    )
    expect_equal(res_chi$hweGroup$ternaryPlot$state$region_type, 1)
})

test_that("mSNP polymorphism test df is 2*(G-1) and X-linked plots mask missing genotypes", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R", "jYS/R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    # Data with outcome (2 groups) and gender
    set.seed(42)
    n <- 120
    df_test <- data.frame(
        rs1 = factor(sample(c("AA", "AG", "GG"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))),
        status = factor(rep(c("Case", "Control"), each = n / 2)),
        sex = factor(rep(c("Female", "Male"), n / 2))
    )
    # Introduce some NAs in rs1
    df_test$rs1[c(1, 5, 10)] <- NA

    res <- mSNP_fn(
        data = df_test,
        vars = "rs1",
        group = NULL,
        outcome = "status",
        covs = NULL,
        groupTable = TRUE,
        xLinked = TRUE,
        gender = "sex",
        ternaryPlot = TRUE,
        assocEnable = TRUE
    )

    # 1. Polymorphism test df: G=2 groups => df = 2*(2-1) = 2
    grp_tbl <- res$hweGroup$groupAnalysisTable
    poly_col <- grp_tbl$getColumn("poly_chi")
    expect_true(grepl("df=2", poly_col$title, fixed = TRUE))

    # 2. X-linked ternary plot state counts match non-missing female genotypes
    tern_state <- res$hweGroup$ternaryPlot$state
    expect_false(is.null(tern_state))
    # Females in df_test with non-missing rs1:
    females_df <- df_test[!is.na(df_test$rs1) & df_test$sex == "Female", ]
    expected_female_n <- nrow(females_df)
    # Total counts across groups in ternary state for rs1
    total_ternary_n <- sum(tern_state$counts_mat)
    expect_equal(total_ternary_n, expected_female_n)

    # 3. Model note n5 does not state 'is highlighted'
    assoc_notes <- res$assocGroup$assocTable$notes
    expect_true("n5" %in% names(assoc_notes))
    expect_false(grepl("is highlighted", assoc_notes[["n5"]]$note, ignore.case = TRUE))
    expect_match(assoc_notes[["n5"]]$note, "Model comparison", fixed = TRUE)
})

test_that("mSNP Step 5: polymorphism df keeps test parameter after dropped groups and sex notice", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msnp\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSNP_fn <- mSNP

    # Test polymorphism df when one group level has 0 cases
    df_poly <- data.frame(
        snp = c(rep("AA", 10), rep("AG", 5), rep("GG", 1), rep("AA", 2), rep("AG", 8), rep("GG", 15)),
        outcome = factor(c(rep("G1", 16), rep("G2", 25)), levels = c("G1", "G2", "G3_Empty"))
    )

    res_poly <- mSNP_fn(
        data = df_poly,
        vars = "snp",
        group = NULL,
        outcome = "outcome",
        covs = NULL,
        gender = NULL,
        groupTable = TRUE
    )
    poly_tbl <- res_poly$hweGroup$groupAnalysisTable
    poly_df <- poly_tbl$asDF
    # For 3 genotypes and 2 observed groups (G3 is empty), poly_p matches pchisq with df=2, not df=4
    expect_equal(poly_df$poly_p[1], pchisq(poly_df$poly_chi[1], df = 2, lower.tail = FALSE))

    # Test sex auto-detection notice
    df_sex <- data.frame(
        snp = rep(c("AA", "AG", "GG"), 10),
        sex = rep(c("female", "male"), 15)
    )
    res_sex <- mSNP_fn(
        data = df_sex,
        vars = "snp",
        group = NULL,
        outcome = NULL,
        covs = NULL,
        gender = "sex",
        hwTests = TRUE,
        xLinked = TRUE
    )
    expect_true("sexLevelsAuto" %in% res_sex$itemNames)
})
