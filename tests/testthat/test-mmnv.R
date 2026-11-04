test_that("mMNV works for Repeated Measures MANOVA", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_dir <- if (dir.exists("jYS/R")) "jYS/R" else if (dir.exists("R")) "R" else if (dir.exists("../../R")) "../../R" else "../R"
    source(file.path(r_dir, "mmnv.h.R"))
    source(file.path(r_dir, "mmnv.b.R"))
    mMNV_fn <- mMNV

    set.seed(42)
    n <- 40
    df <- data.frame(
        Group = as.factor(rep(c("A", "B"), each = 20)),
        Age = round(rnorm(n, 45, 10)),
        T1 = rnorm(n, 20, 4),
        T2 = rnorm(n, 18, 4),
        T3 = rnorm(n, 15, 4),
        T4 = rnorm(n, 12, 4),
        Y1 = rnorm(n, 50, 10),
        Y2 = rnorm(n, 100, 15)
    )

    # 1. Repeated Measures MANOVA (Mixed Design: within + between + covariate)
    res_rm <- mMNV_fn(
        data = df,
        rmCells = c("T1", "T2", "T3", "T4"),
        rmFactorName = "Time",
        between = c("Group"),
        covs = c("Age"),
        contrastType = "polynomial",
        testPillai = TRUE,
        testWilks = TRUE,
        testHotelling = TRUE,
        testRoy = TRUE,
        showUnivariate = TRUE,
        showSphericity = TRUE,
        showProfileAnalysis = TRUE,
        showContrastsTable = TRUE,
        showAssumptionChecks = TRUE,
        showMeansTable = TRUE,
        showProfilePlot = TRUE,
        showOutlierPlot = TRUE,
        showHePlot = TRUE
    )

    expect_true(inherits(res_rm, "mMNVResults"))
    expect_true(inherits(res_rm$multivariateTable, "Table"))
    expect_true(res_rm$multivariateTable$rowCount > 0)
    expect_true(inherits(res_rm$univariateTable, "Table"))
    expect_true(res_rm$univariateTable$rowCount > 0)
    univ_df <- res_rm$univariateTable$asDF
    expect_true("df1" %in% names(univ_df) && !is.na(univ_df$df1[1]))
    expect_true("df2" %in% names(univ_df) && !is.na(univ_df$df2[1]))
    expect_true(inherits(res_rm$sphericityTable, "Table"))
    expect_true(res_rm$sphericityTable$rowCount > 0)

    # Check sphericity approx chi-square is numeric and not NA
    sph_df <- res_rm$sphericityTable$asDF
    expect_true(!is.null(sph_df$chisq) && !is.na(sph_df$chisq[1]))

    # Check profile analysis contains Parallelism row
    expect_true(inherits(res_rm$profileTable, "Table"))
    expect_true(res_rm$profileTable$rowCount >= 2)
    prof_df <- res_rm$profileTable$asDF
    expect_true(any(grepl("Parallelism", prof_df$hypothesis)))

    # Check within-subjects contrasts table is populated with non-NA values
    expect_true(inherits(res_rm$contrastsTable, "Table"))
    expect_true(res_rm$contrastsTable$rowCount > 0)
    cnt_df <- res_rm$contrastsTable$asDF
    expect_true(!is.na(cnt_df$f[1]))
    expect_true(!is.na(cnt_df$p[1]))
    expect_true(!is.na(cnt_df$ss[1]))

    # Check plot states are populated for export
    expect_true(!is.null(res_rm$profilePlot$state))
    expect_true(!is.null(res_rm$profilePlot$state$summary))
    expect_true(!is.null(res_rm$outlierPlot$state))
    expect_true(!is.null(res_rm$outlierPlot$state$d2))
    expect_true(!is.null(res_rm$hePlot$state))
    expect_true(!is.null(res_rm$hePlot$state$vars))

    # Check means table has valid t-based CIs and note
    means_df <- res_rm$meansTable$asDF
    expect_true(means_df$ciLower[1] < means_df$mean[1] && means_df$mean[1] < means_df$ciUpper[1])

    # Check assumptions table has Box's M
    assump_df <- res_rm$assumptionsTable$asDF
    expect_true(any(grepl("Box's M", assump_df$test)))

    # Check repeated contrasts mode
    res_rep <- mMNV_fn(
        data = df,
        rmCells = c("T1", "T2", "T3"),
        contrastType = "repeated",
        showContrastsTable = TRUE
    )
    expect_true(res_rep$contrastsTable$rowCount >= 2)
    rep_df <- res_rep$contrastsTable$asDF
    expect_true(!is.na(rep_df$f[1]))
    expect_true(grepl("\u2013", rep_df$contrast[1]))

    # 2. Pure Within-Subjects Design (No between factors, no covariates)
    res_pure <- mMNV_fn(
        data = df,
        rmCells = c("T1", "T2", "T3"),
        showAssumptionChecks = TRUE,
        showMeansTable = TRUE,
        showOutlierPlot = TRUE
    )

    expect_true(inherits(res_pure, "mMNVResults"))
    expect_true(inherits(res_pure$multivariateTable, "Table"))
    expect_true(res_pure$multivariateTable$rowCount > 0)
    expect_true(inherits(res_pure$assumptionsTable, "Table"))
    expect_true(res_pure$assumptionsTable$rowCount > 0)
    expect_true(!is.null(res_pure$outlierPlot$state))

    # 3. Small sample Box's M messaging (n <= p)
    df_small <- df[c(1:3, 21:23), ]
    res_small <- mMNV_fn(data = df_small, rmCells = c("T1", "T2", "T3", "T4"), between = "Group")
    assump_small <- res_small$assumptionsTable$asDF
    expect_true(any(grepl("Box's M", assump_small$test)))
    box_row <- assump_small[grepl("Box's M", assump_small$test), ]
    expect_true(grepl("n <= p", box_row$assessment))

    # 4. Instructions visibility: shown when empty, hidden when configured
    res_empty <- mMNV_fn(data = data.frame())
    expect_true(res_empty$instructions$visible)
    expect_false(res_pure$instructions$visible)

    df_unused <- df
    df_unused$Group <- factor(df_unused$Group, levels = c("A", "B", "UnusedGroup"))
    res_unused <- mMNV_fn(
        data = df_unused,
        rmCells = c("T1", "T2", "T3"),
        rmFactorName = "Time Point",
        between = "Group"
    )
    expect_true(inherits(res_unused, "mMNVResults"))
    expect_true(res_unused$multivariateTable$rowCount > 0)

    # 5. Spaced between factor and covariate names (formula parsing test)
    df_spaces <- df
    names(df_spaces)[names(df_spaces) == "Group"] <- "Treatment Group"
    names(df_spaces)[names(df_spaces) == "Age"] <- "Age at baseline"
    res_spaces <- mMNV_fn(
        data = df_spaces,
        rmCells = c("T1", "T2", "T3"),
        rmFactorName = "Time Point",
        between = "Treatment Group",
        covs = "Age at baseline"
    )
    expect_true(inherits(res_spaces, "mMNVResults"))
    expect_true(res_spaces$multivariateTable$rowCount > 0)
    expect_true(res_spaces$profileTable$rowCount > 0)

    # 6. Singular error matrix in profile analysis (small sample N < k)
    set.seed(42)
    df_sing <- data.frame(
        ID = 1:4,
        Group = factor(c("G1", "G1", "G2", "G2")),
        Y1 = rnorm(4, 10, 1),
        Y2 = rnorm(4, 11, 1),
        Y3 = rnorm(4, 12, 1),
        Y4 = rnorm(4, 13, 1),
        Y5 = rnorm(4, 14, 1)
    )
    res_sing <- mMNV_fn(
        data = df_sing,
        rmCells = c("Y1", "Y2", "Y3", "Y4", "Y5"),
        rmFactorName = "Measure",
        between = "Group"
    )
    expect_true(inherits(res_sing, "mMNVResults"))
    expect_true(inherits(res_sing$profileTable, "Table"))

    # 7. Verification of Stage 3 fixes:
    # 7a. Tests of Within-Subjects Effects excludes purely between-subjects terms
    expect_false(any(univ_df$term %in% c("(Intercept)", "Group", "Age")))
    expect_true(all(grepl("Time", univ_df$term)))

    # 7b. Neutral profile conclusions without capitalized "NOT"
    expect_false(any(grepl("\\bNOT\\b", prof_df$conclusion)))

    # 7c. Display name with spaces preserved across tables and plots
    spaces_m_df <- res_spaces$multivariateTable$asDF
    spaces_u_df <- res_spaces$univariateTable$asDF
    spaces_s_df <- res_spaces$sphericityTable$asDF
    spaces_p_df <- res_spaces$profileTable$asDF
    spaces_c_df <- res_spaces$contrastsTable$asDF
    expect_true(any(grepl("Time Point", spaces_m_df$term)))
    expect_false(any(grepl("Time\\.Point", spaces_m_df$term)))
    expect_true(any(grepl("Time Point", spaces_u_df$term)))
    expect_false(any(grepl("Time\\.Point", spaces_u_df$term)))
    expect_true(any(grepl("Time Point", spaces_s_df$term)))
    expect_false(any(grepl("Time\\.Point", spaces_s_df$term)))
    expect_true(any(grepl("Time Point", spaces_p_df$hypothesis)))
    expect_false(any(grepl("Time\\.Point", spaces_p_df$hypothesis)))
    expect_true(any(grepl("Time Point", spaces_c_df$contrast)))
    expect_false(any(grepl("Time\\.Point", spaces_c_df$contrast)))
    expect_equal(res_spaces$profilePlot$state$rmFactorName, "Time Point")

    # 7d. Ungated covariate x trend rows when between-factors are absent
    res_cov_only <- mMNV_fn(
        data = df,
        rmCells = c("T1", "T2", "T3"),
        covs = "Age",
        showContrastsTable = TRUE
    )
    expect_true(inherits(res_cov_only$contrastsTable, "Table"))
    cov_cnt_df <- res_cov_only$contrastsTable$asDF
    expect_true(any(grepl("Age", cov_cnt_df$contrast)))

    # 7e. Duplicate multiBetweenNotice prevention
    df_multi_b <- df
    df_multi_b$Center <- factor(rep(c("C1", "C2"), 20))
    res_multi_b <- mMNV_fn(
        data = df_multi_b,
        rmCells = c("T1", "T2", "T3"),
        between = c("Group", "Center"),
        showMeansTable = TRUE
    )
    notice_names <- vapply(res_multi_b$items, function(it) it$name, character(1))
    expect_equal(sum(notice_names == "multiBetweenNotice"), 1)

    # 8. Verification of [LOW] audit fixes:
    # 8a. Objective interpretations in profileTable without fixed-alpha verdicts
    expect_true(any(grepl("Evaluates parallelism", prof_df$conclusion)))
    expect_false(any(grepl("supported|deviate", prof_df$conclusion, ignore.case = TRUE)))

    # 8b. Objective assessments in assumptionsTable
    expect_true(any(grepl("Evaluates equality of covariance matrices", assump_df$assessment)))
    expect_true(any(grepl("Evaluates multivariate skewness", assump_df$assessment)))

    # 8c. En dash in univariate note
    uni_notes <- res_rm$univariateTable$notes
    expect_true(any(grepl("Greenhouse\u2013Geisser", unlist(lapply(uni_notes, function(n) n$note)))))
    expect_true(any(grepl("Huynh\u2013Feldt", unlist(lapply(uni_notes, function(n) n$note)))))

    # 8d. Descriptives note wording
    means_notes <- res_multi_b$meansTable$notes
    expect_true(any(grepl("Descriptive statistics are evaluated", unlist(lapply(means_notes, function(n) n$note)))))
})
