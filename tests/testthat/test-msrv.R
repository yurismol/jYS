test_that("mSRV works and supports kmMaxTime", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists("mSRV", mode = "function")) {
        r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msrv\\.[hb]\\.R$", full.names = TRUE)
        for (rf in sort(r_files)) source(rf)
    }
    mSRV_fn <- if (exists("mSRV", mode = "function")) mSRV else jYS::mSRV

    set.seed(42)
    n <- 100
    df <- data.frame(
        time = rexp(n, rate = 0.05),
        status = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
        grp = sample(c("Control", "Treatment"), n, replace = TRUE)
    )

    # 1. Test basic mSRV execution
    res <- mSRV_fn(
        data = df,
        elapsed = "time",
        status = "status",
        group = "grp",
        showKm = TRUE,
        kmTable = TRUE
    )

    expect_s3_class(res, "mSRVResults")
    expect_s3_class(res$kmSection$kmSummaryTable, "Table")
    expect_true(res$kmSection$kmSummaryTable$rowCount > 0)
    expect_s3_class(res$kmSection$kmRiskTable, "Table")
    expect_true(res$kmSection$kmRiskTable$rowCount > 0)

    # 2. Test mSRV with kmMaxTime
    limit_val <- 30
    res_limited <- mSRV_fn(
        data = df,
        elapsed = "time",
        status = "status",
        group = "grp",
        showKm = TRUE,
        kmTable = TRUE,
        kmMaxTime = limit_val
    )

    expect_s3_class(res_limited, "mSRVResults")
    risk_df <- res_limited$kmSection$kmRiskTable$asDF
    expect_true(all(risk_df$time <= limit_val))

    # 3. Test mSRV with adjusted survival curves (Direct / G-computation)
    df$age <- rnorm(n, mean = 50, sd = 10)
    res_adj <- mSRV_fn(
        data = df,
        elapsed = "time",
        status = "status",
        group = "grp",
        covariates = "age",
        showCox = TRUE,
        showAdjCurves = TRUE
    )

    expect_s3_class(res_adj, "mSRVResults")
    expect_s3_class(res_adj$adjSection$adjSummaryTable, "Table")
    expect_equal(res_adj$adjSection$adjSummaryTable$rowCount, 2)

    # 3b. Test adjusted survival curves when showCox is FALSE (fully decoupled)
    res_adj_nocox <- mSRV_fn(
        data = df,
        elapsed = "time",
        status = "status",
        group = "grp",
        covariates = "age",
        showCox = FALSE,
        showAdjCurves = TRUE
    )
    expect_s3_class(res_adj_nocox, "mSRVResults")
    expect_s3_class(res_adj_nocox$adjSection$adjSummaryTable, "Table")
    expect_equal(res_adj_nocox$adjSection$adjSummaryTable$rowCount, 2)
    expect_equal(res_adj_nocox$coxSection$coxFitTable$rowCount, 0)

    # 3c. Test with all GUI options turned off (no empty tables displayed)
    res_off <- mSRV_fn(
        data = df,
        elapsed = "time",
        status = "status",
        group = "grp",
        showKm = FALSE,
        kmPlot = FALSE,
        showLogRank = FALSE,
        showCox = FALSE,
        showAdjCurves = FALSE,
        showCompRisks = FALSE,
        showRoc = FALSE
    )
    expect_s3_class(res_off, "mSRVResults")
    expect_equal(res_off$kmSection$kmSummaryTable$rowCount, 0)
    expect_equal(res_off$logRankTable$rowCount, 0)
    expect_equal(res_off$coxSection$coxFitTable$rowCount, 0)
    expect_equal(res_off$adjSection$adjSummaryTable$rowCount, 0)
    expect_equal(res_off$compRisksSection$cifSummaryTable$rowCount, 0)
    expect_equal(res_off$rocSection$rocTable$rowCount, 0)

    # 4. Test Competing Risks Analysis (Aalen-Johansen CIF, Gray's test, Fine-Gray)
    etime <- with(survival::mgus2, ifelse(pstat == 1, ptime, futime))
    event <- with(survival::mgus2, ifelse(pstat == 1, 1, 2 * death))
    df_cr <- data.frame(
        time = etime,
        status = factor(event, levels = 0:2, labels = c("censor", "pcm", "death")),
        sex = survival::mgus2$sex,
        age = survival::mgus2$age
    )

    res_cr <- mSRV_fn(
        data = df_cr,
        elapsed = "time",
        status = "status",
        group = "sex",
        covariates = "age",
        showKm = FALSE,
        showCox = FALSE,
        showCompRisks = TRUE,
        compEvent = "pcm",
        compCensor = "censor",
        showCifTable = TRUE,
        showCifPlot = TRUE,
        showGrayTest = TRUE,
        showFineGray = TRUE
    )

    expect_s3_class(res_cr, "mSRVResults")
    expect_s3_class(res_cr$compRisksSection$cifSummaryTable, "Table")
    expect_true(res_cr$compRisksSection$cifSummaryTable$rowCount >= 2)

    cif_df <- res_cr$compRisksSection$cifSummaryTable$asDF
    expect_true(all(cif_df$cif >= 0 & cif_df$cif <= 1))

    expect_s3_class(res_cr$compRisksSection$grayTestTable, "Table")
    expect_true(res_cr$compRisksSection$grayTestTable$rowCount >= 2)

    gray_df <- res_cr$compRisksSection$grayTestTable$asDF
    expect_true(all(!is.na(gray_df$stat) & gray_df$stat >= 0))
    expect_true(all(!is.na(gray_df$p) & gray_df$p >= 0 & gray_df$p <= 1))

    expect_s3_class(res_cr$compRisksSection$fineGrayTable, "Table")
    expect_true(res_cr$compRisksSection$fineGrayTable$rowCount >= 2)

    fg_df <- res_cr$compRisksSection$fineGrayTable$asDF
    expect_true(all(!is.na(fg_df$shr) & fg_df$shr > 0))
    expect_true(all(!is.na(fg_df$p) & fg_df$p >= 0 & fg_df$p <= 1))

    # 4b. Test Gray's test is bypassed and tstart_note is set when tstart is specified (Item 10)
    df_cr_tstart <- df_cr
    df_cr_tstart$t0 <- 0
    res_cr_tstart <- mSRV_fn(
        data = df_cr_tstart,
        tstart = "t0",
        elapsed = "time",
        status = "status",
        group = "sex",
        covariates = "age",
        showKm = FALSE,
        showCox = FALSE,
        showCompRisks = TRUE,
        compEvent = "pcm",
        compCensor = "censor",
        showGrayTest = TRUE
    )
    expect_s3_class(res_cr_tstart, "mSRVResults")
    gray_tbl <- res_cr_tstart$compRisksSection$grayTestTable
    expect_equal(gray_tbl$rowCount, 0)
    expect_true("tstart_note" %in% names(gray_tbl$notes))

    # 4c. Test Competing Risks auto-resolves identical initial default levels
    df_cr_default <- data.frame(
        time = etime,
        status = factor(event, levels = 0:2, labels = c("Alive", "pcm", "death")),
        sex = survival::mgus2$sex,
        age = survival::mgus2$age
    )

    res_cr_default <- mSRV_fn(
        data = df_cr_default,
        elapsed = "time",
        status = "status",
        group = "sex",
        covariates = "age",
        showKm = FALSE,
        showCox = FALSE,
        showCompRisks = TRUE,
        compEvent = "Alive",
        compCensor = "Alive",
        statusEvent = "Alive",
        showCifTable = TRUE,
        showCifPlot = TRUE
    )
    expect_s3_class(res_cr_default, "mSRVResults")
    expect_s3_class(res_cr_default$compRisksSection$cifSummaryTable, "Table")
    expect_true(res_cr_default$compRisksSection$cifSummaryTable$rowCount > 0)

    # 4c. Test Competing Risks with single status level handles gracefully without error
    df_single <- data.frame(
        time = 1:10,
        status = factor(rep("Alive", 10)),
        sex = factor(rep(c("M", "F"), 5)),
        age = 50
    )
    res_single <- mSRV_fn(
        data = df_single,
        elapsed = "time",
        status = "status",
        group = "sex",
        covariates = "age",
        showKm = FALSE,
        showCox = FALSE,
        showCompRisks = TRUE,
        compEvent = "Alive",
        compCensor = "Alive",
        statusEvent = "Alive",
        showCifTable = TRUE
    )
    expect_s3_class(res_single, "mSRVResults")
    expect_equal(res_single$compRisksSection$cifSummaryTable$rowCount, 0)

    # 5. Test Counting Process Data (Start-Stop format, cluster)
    res_cp <- mSRV_fn(
        data = survival::heart,
        tstart = "start",
        elapsed = "stop",
        status = "event",
        group = "transplant",
        covariates = c("age", "surgery"),
        subjectId = "id",
        showKm = TRUE,
        showLogRank = TRUE,
        showCox = TRUE
    )

    expect_s3_class(res_cp, "mSRVResults")
    expect_s3_class(res_cp$kmSection$kmSummaryTable, "Table")
    expect_true(res_cp$kmSection$kmSummaryTable$rowCount >= 2)
    expect_s3_class(res_cp$logRankTable, "Table")
    expect_true(res_cp$logRankTable$rowCount >= 1)
    expect_s3_class(res_cp$coxSection$coxCoefTable, "Table")
    expect_true(res_cp$coxSection$coxCoefTable$rowCount >= 3)

    # 6. Test Time-dependent Covariates / Effects (timeDepVars, showTimeDepEffects)
    res_td <- mSRV_fn(
        data = survival::heart,
        tstart = "start",
        elapsed = "stop",
        status = "event",
        group = "transplant",
        covariates = c("age", "surgery"),
        subjectId = "id",
        timeDepVars = "age",
        showTimeDepEffects = TRUE,
        timeDepFunc = "log",
        showCox = TRUE
    )

    expect_s3_class(res_td, "mSRVResults")
    expect_s3_class(res_td$coxSection$timeDepTable, "Table")
    expect_true(res_td$coxSection$timeDepTable$rowCount >= 4)
    td_df <- res_td$coxSection$timeDepTable$asDF
    expect_true(any(grepl("log\\(t\\)", td_df$var)))
    expect_true(all(!is.na(td_df$hr) & td_df$hr > 0))
    expect_true(all(!is.na(td_df$p) & td_df$p >= 0 & td_df$p <= 1))

    # Also test with linear time transform
    res_td_lin <- mSRV_fn(
        data = survival::heart,
        tstart = "start",
        elapsed = "stop",
        status = "event",
        group = "transplant",
        covariates = c("age", "surgery"),
        subjectId = "id",
        timeDepVars = "age",
        showTimeDepEffects = TRUE,
        timeDepFunc = "linear",
        showCox = TRUE
    )
    expect_s3_class(res_td_lin, "mSRVResults")
    td_lin_df <- res_td_lin$coxSection$timeDepTable$asDF
    expect_true(any(grepl("× t", td_lin_df$var)))

    # Test with only timeDepVars (no group, no covariates)
    res_td_only <- mSRV_fn(
        data = survival::heart,
        tstart = "start",
        elapsed = "stop",
        status = "event",
        subjectId = "id",
        timeDepVars = "age",
        showTimeDepEffects = TRUE,
        timeDepFunc = "log",
        showCox = TRUE
    )
    expect_s3_class(res_td_only, "mSRVResults")
    expect_s3_class(res_td_only$coxSection$timeDepTable, "Table")
    expect_true(res_td_only$coxSection$timeDepTable$rowCount >= 2)
})

test_that("mSRV Schoenfeld residuals plots persist state and export properly", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    if (!exists("mSRV", mode = "function") || !exists("mSRVClass")) {
        r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msrv\\.[hb]\\.R$", full.names = TRUE)
        for (rf in sort(r_files)) source(rf, encoding = "UTF-8")
    }
    mSRV_fn <- if (exists("mSRV", mode = "function")) mSRV else jYS::mSRV

    set.seed(42)
    n <- 80
    df <- data.frame(
        time = rexp(n, rate = 0.05),
        status = sample(c(0, 1), n, replace = TRUE, prob = c(0.3, 0.7)),
        grp = sample(c("Control", "Treatment"), n, replace = TRUE),
        cov1 = rnorm(n)
    )

    opts <- mSRVOptions$new(
        elapsed = "time",
        status = "status",
        group = "grp",
        covariates = "cov1",
        showCox = TRUE,
        coxAssump = TRUE,
        showSchoenfeldPlot = TRUE
    )

    an <- mSRVClass$new(options = opts, data = df)
    an$init()

    # Pre-population check in .init()
    sp_init <- an$results$coxSection$schoenfeldPlots
    expect_s3_class(sp_init, "Array")
    expect_true(length(sp_init$itemKeys) >= 2)

    an$run()

    sp <- an$results$coxSection$schoenfeldPlots
    expect_true(length(sp$items) >= 2)
    expect_false(is.null(sp$state))

    # Verify item state contains pts and smooth data frames
    grp_item <- sp$get(key = "..group")
    expect_false(is.null(grp_item$state))
    expect_true(is.data.frame(grp_item$state$pts))
    expect_true(is.data.frame(grp_item$state$smooth))

    # Verify PDF export via saveAs
    tmp_pdf1 <- tempfile(fileext = ".pdf")
    on.exit(unlink(c(tmp_pdf1)), add = TRUE)
    grp_item$saveAs(tmp_pdf1)
    expect_true(file.exists(tmp_pdf1))
    expect_gt(file.info(tmp_pdf1)$size, 1000)

    # Verify .savePart lookup with unquoted key
    tmp_pdf2 <- tempfile(fileext = ".pdf")
    on.exit(unlink(c(tmp_pdf2)), add = TRUE)
    an$.savePart(tmp_pdf2, "coxSection/schoenfeldPlots/..group")
    expect_true(file.exists(tmp_pdf2))
    expect_gt(file.info(tmp_pdf2)$size, 1000)

    # Verify .savePart lookup with array
    tmp_pdf3 <- tempfile(fileext = ".pdf")
    on.exit(unlink(c(tmp_pdf3)), add = TRUE)
    an$.savePart(tmp_pdf3, "coxSection/schoenfeldPlots")
    expect_true(file.exists(tmp_pdf3))
    expect_gt(file.info(tmp_pdf3)$size, 1000)

    # Verify state hydration from parent when item state is NULL
    grp_item$setState(NULL)
    expect_null(grp_item$state)
    tmp_pdf4 <- tempfile(fileext = ".pdf")
    on.exit(unlink(c(tmp_pdf4)), add = TRUE)
    an$.savePart(tmp_pdf4, "coxSection/schoenfeldPlots/..group")
    expect_true(file.exists(tmp_pdf4))
    expect_gt(file.info(tmp_pdf4)$size, 1000)
})


