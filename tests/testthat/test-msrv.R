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
})

test_that("mSRV supports Firth's penalized Cox regression", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msrv\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSRV_fn <- mSRV

    set.seed(42)
    df_surv <- data.frame(
        time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        status = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
        x = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
    )

    if (requireNamespace("coxphf", quietly = TRUE)) {
        res <- mSRV_fn(
            data = df_surv,
            elapsed = "time",
            status = "status",
            covariates = "x",
            showCox = TRUE,
            coxFirth = TRUE,
            coxForest = TRUE,
            coxAssump = TRUE
        )
        expect_s3_class(res, "mSRVResults")
        coef_df <- res$coxSection$coxCoefTable$asDF
        expect_true(nrow(coef_df) > 0)
        expect_true(all(is.finite(coef_df$coef)))

        # Verify Schoenfeld residuals test shows note instead of failing
        assump_table <- res$coxSection$coxAssumpTable
        expect_equal(assump_table$rowCount, 0)

        # Verify firth_note explains penalized ML vs profile LR and Wald z
        coef_tbl <- res$coxSection$coxCoefTable
        expect_true("firth_note" %in% names(coef_tbl$notes))
        expect_match(coef_tbl$notes[["firth_note"]]$note, "penalized maximum likelihood")
        expect_match(coef_tbl$notes[["firth_note"]]$note, "signed z-statistics")
        coef_firth_df <- coef_tbl$asDF
        valid_idx <- which(!is.na(coef_firth_df$p) & coef_firth_df$p > 0 & coef_firth_df$p <= 1)
        expect_true(length(valid_idx) > 0)
        expected_z <- sign(coef_firth_df$coef[valid_idx]) * sqrt(stats::qchisq(coef_firth_df$p[valid_idx], df = 1, lower.tail = FALSE))
        expect_equal(coef_firth_df$z[valid_idx], expected_z, tolerance = 1e-4)

        # Verify droplevels handles empty unused factor level without NaN
        df_surv$unused_fac <- factor(rep(c("A", "B"), length.out = nrow(df_surv)), levels = c("A", "B", "Unused"))
        res_fac <- mSRV_fn(
            data = df_surv,
            elapsed = "time",
            status = "status",
            covariates = c("x", "unused_fac"),
            showCox = TRUE,
            coxFirth = TRUE
        )
        expect_s3_class(res_fac, "mSRVResults")
        coef_fac_df <- res_fac$coxSection$coxCoefTable$asDF
        expect_true(nrow(coef_fac_df) > 0)
        expect_false(any(is.nan(coef_fac_df$coef)))

        # Verify tables where Firth does not apply receive firth_na_note
        df_surv$grp <- factor(rep(c("A", "B"), length.out = nrow(df_surv)))
        res_na_notes <- mSRV_fn(
            data = df_surv,
            elapsed = "time",
            status = "status",
            group = "grp",
            covariates = "x",
            showCox = TRUE,
            coxFirth = TRUE,
            showTimeDepEffects = TRUE,
            timeDepVars = "x",
            showAdjCurves = TRUE,
            adjExposure = "grp"
        )
        expect_true("firth_na_note" %in% names(res_na_notes$coxSection$timeDepTable$notes))
        expect_true("firth_na_note" %in% names(res_na_notes$adjSection$adjSummaryTable$notes))

        # Verify ROC note contains IPCW censoring caveat
        res_roc <- mSRV_fn(
            data = df_surv,
            elapsed = "time",
            status = "status",
            showRoc = TRUE,
            rocPredictors = "x"
        )
        expect_true("roc_note" %in% names(res_roc$rocSection$rocTable$notes))
        expect_match(res_roc$rocSection$rocTable$notes[["roc_note"]]$note, "IPCW")
    } else {
        # Without coxphf, fitting fails with informative notice, without crashing
        res <- mSRV_fn(
            data = df_surv,
            elapsed = "time",
            status = "status",
            covariates = "x",
            showCox = TRUE,
            coxFirth = TRUE
        )
        expect_s3_class(res, "mSRVResults")
        expect_equal(res$coxSection$coxCoefTable$rowCount, 0)
    }
})

test_that("mSRV compact plot states and clustered competing risks work correctly", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msrv\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSRV_fn <- mSRV

    set.seed(123)
    etime <- with(survival::mgus2, ifelse(pstat == 1, ptime, futime))
    event <- with(survival::mgus2, ifelse(pstat == 1, 1, 2 * death))
    df_cr <- data.frame(
        time = etime,
        status = factor(event, levels = 0:2, labels = c("censor", "pcm", "death")),
        sex = survival::mgus2$sex,
        age = survival::mgus2$age,
        id = survival::mgus2$id
    )

    res <- mSRV_fn(
        data = df_cr,
        elapsed = "time",
        status = "status",
        group = "sex",
        covariates = "age",
        subjectId = "id",
        showKm = TRUE,
        kmPlot = TRUE,
        showCox = TRUE,
        coxForest = TRUE,
        showCompRisks = TRUE,
        compEvent = "pcm",
        compCensor = "censor",
        showCifTable = TRUE,
        showCifPlot = TRUE,
        showFineGray = TRUE
    )

    expect_s3_class(res, "mSRVResults")

    # 1. Verify KM plot state and render
    km_state <- res$kmSection$kmPlot$state
    expect_false(is.null(km_state))
    expect_true(is.data.frame(km_state$df))
    expect_true("surv" %in% names(km_state$df))
    expect_equal(km_state$group_var, "sex")
    tmp_km <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_km), add = TRUE)
    res$kmSection$kmPlot$saveAs(tmp_km)
    expect_true(file.exists(tmp_km))
    expect_gt(file.info(tmp_km)$size, 500)

    # 2. Verify Cox Forest plot state and render
    forest_state <- res$coxSection$coxForestPlot$state
    expect_false(is.null(forest_state))
    expect_true(is.data.frame(forest_state$coef_df))
    expect_true(all(c("var", "coef", "hr", "lower", "upper") %in% names(forest_state$coef_df)))
    tmp_forest <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_forest), add = TRUE)
    res$coxSection$coxForestPlot$saveAs(tmp_forest)
    expect_true(file.exists(tmp_forest))
    expect_gt(file.info(tmp_forest)$size, 500)

    # 3. Verify CIF plot state and render
    cif_state <- res$compRisksSection$cifPlot$state
    expect_false(is.null(cif_state))
    expect_true(is.data.frame(cif_state$plot_data))
    expect_true(all(c("time", "cif", "cause", "group") %in% names(cif_state$plot_data)))
    tmp_cif <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_cif), add = TRUE)
    res$compRisksSection$cifPlot$saveAs(tmp_cif)
    expect_true(file.exists(tmp_cif))
    expect_gt(file.info(tmp_cif)$size, 500)

    # 4. Verify Fine-Gray table with subjectId includes cluster note
    fg_table <- res$compRisksSection$fineGrayTable
    expect_true(fg_table$rowCount >= 2)
    expect_true("cluster_note" %in% names(fg_table$notes))
    expect_match(fg_table$notes[["cluster_note"]]$note, "Huber–White")
})




test_that("mSRV Step 4: Fine-Gray intervals without ID and G-computation earliest interval", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^msrv\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSRV_fn <- mSRV

    # 1. Fine-Gray with tstart and NO subjectId
    set.seed(42)
    df_fg_cp <- data.frame(
        tstart = c(0, 5, 0, 3, 0, 4, 0, 6),
        time   = c(5, 10, 3, 8, 4, 9, 6, 12),
        status = factor(c("0", "1", "0", "2", "0", "1", "0", "2"), levels = c("0", "1", "2")),
        x      = rnorm(8),
        grp    = factor(rep(c("A", "B"), 4))
    )

    res_fg <- mSRV_fn(
        data = df_fg_cp,
        elapsed = "time",
        tstart = "tstart",
        status = "status",
        statusEvent = "1",
        compEvent = "1",
        compCensor = "0",
        group = "grp",
        covariates = "x",
        showCompRisks = TRUE,
        showFineGray = TRUE
    )
    fg_tbl <- res_fg$compRisksSection$fineGrayTable
    expect_true("cluster_note" %in% names(fg_tbl$notes))
    expect_match(fg_tbl$notes[["cluster_note"]]$note, "per interval")

    # 2. G-computation with tstart and NO subjectId: weight_note added
    df_gcomp <- data.frame(
        tstart = c(0, 5, 0, 4, 0, 3, 0, 6, 0, 5, 0, 4),
        time   = c(5, 12, 4, 10, 3, 8, 6, 14, 5, 11, 4, 9),
        event  = c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
        grp    = factor(rep(c("Control", "Treated"), each = 6)),
        age    = c(50, 55, 60, 64, 45, 48, 52, 58, 61, 66, 47, 51)
    )

    res_gcomp <- mSRV_fn(
        data = df_gcomp,
        elapsed = "time",
        tstart = "tstart",
        status = "event",
        statusEvent = "1",
        covariates = "age",
        showAdjCurves = TRUE,
        adjExposure = "grp"
    )
    adj_tbl <- res_gcomp$adjSection$adjSummaryTable
    expect_true("weight_note" %in% names(adj_tbl$notes))
    expect_match(adj_tbl$notes[["weight_note"]]$note, "weighted as independent observations")
})
