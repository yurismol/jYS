test_that("mSTAB works", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    r_files <- list.files(c("R", "../../R", "../R"), pattern = "^mstab\\.[hb]\\.R$", full.names = TRUE)
    for (rf in sort(r_files)) source(rf)
    mSTAB_fn <- mSTAB

    # Load test stability dataset
    csv_paths <- c(
        "../../SAMPLE/STAB/STAB_ICH.csv",
        "SAMPLE/STAB/STAB_ICH.csv",
        "../SAMPLE/STAB/STAB_ICH.csv"
    )
    csv_file <- csv_paths[file.exists(csv_paths)][1]
    
    if (is.na(csv_file)) {
        # Fallback synthetic dataset
        set.seed(42)
        batches <- rep(c("1", "2", "3"), each = 16)
        months <- rep(rep(c(0, 3, 6, 9, 12, 18, 24, 36), each = 2), 3)
        assay <- 100.5 - 0.16 * months + rnorm(48, 0, 0.35)
        impurity <- 0.12 + 0.016 * months + rnorm(48, 0, 0.02)
        df <- data.frame(
            Batch = factor(batches),
            Month = months,
            Assay = assay,
            Impurity = impurity
        )
    } else {
        df <- read.csv(csv_file)
        df$Batch <- as.factor(df$Batch)
    }

    # 1. Basic Stability Analysis with Assay and Lower Specification Limit (LSL = 90)
    res_assay <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        alphaPool = 0.25,
        confLevel = 0.95,
        modelChoice = "auto",
        accelCondition = "no_change",
        showPoolTable = TRUE,
        showShelfTable = TRUE,
        showCoefTable = TRUE,
        showStabPlot = TRUE,
        showShelfPlot = TRUE,
        showFacetPlot = TRUE,
        showRatePlot = TRUE,
        showPredPlot = TRUE,
        showDiagPlot = TRUE
    )

    expect_s3_class(res_assay, "mSTABResults")
    expect_s3_class(res_assay$poolTable, "Table")
    expect_s3_class(res_assay$shelfTable, "Table")
    expect_s3_class(res_assay$coefTable, "Table")
    expect_s3_class(res_assay$diagTable, "Table")
    expect_true(res_assay$poolTable$rowCount >= 3)
    expect_true(res_assay$shelfTable$rowCount >= 4)
    expect_true(res_assay$coefTable$rowCount >= 2)
    expect_true(res_assay$diagTable$rowCount >= 4)

    # Verify detailed table notes
    expect_true(length(res_assay$poolTable$notes) >= 5)
    expect_true(length(res_assay$shelfTable$notes) >= 5)
    expect_true(length(res_assay$coefTable$notes) >= 3)
    expect_true(length(res_assay$diagTable$notes) >= 5)

    # Verify plot rendering without graphics/grid errors for all 6 plots
    pdf_tmp1 <- tempfile(fileext = ".pdf")
    pdf_tmp2 <- tempfile(fileext = ".pdf")
    pdf_tmp3 <- tempfile(fileext = ".pdf")
    pdf_tmp4 <- tempfile(fileext = ".pdf")
    pdf_tmp5 <- tempfile(fileext = ".pdf")
    pdf_tmp6 <- tempfile(fileext = ".pdf")

    expect_no_error(res_assay$stabPlot$saveAs(pdf_tmp1))
    expect_no_error(res_assay$shelfPlot$saveAs(pdf_tmp2))
    expect_no_error(res_assay$facetPlot$saveAs(pdf_tmp3))
    expect_no_error(res_assay$ratePlot$saveAs(pdf_tmp4))
    expect_no_error(res_assay$predPlot$saveAs(pdf_tmp5))
    expect_no_error(res_assay$diagPlot$saveAs(pdf_tmp6))

    expect_true(file.exists(pdf_tmp1) && file.info(pdf_tmp1)$size > 1000)
    expect_true(file.exists(pdf_tmp2) && file.info(pdf_tmp2)$size > 1000)
    expect_true(file.exists(pdf_tmp3) && file.info(pdf_tmp3)$size > 1000)
    expect_true(file.exists(pdf_tmp4) && file.info(pdf_tmp4)$size > 1000)
    expect_true(file.exists(pdf_tmp5) && file.info(pdf_tmp5)$size > 1000)
    expect_true(file.exists(pdf_tmp6) && file.info(pdf_tmp6)$size > 1000)
    unlink(c(pdf_tmp1, pdf_tmp2, pdf_tmp3, pdf_tmp4, pdf_tmp5, pdf_tmp6))

    # 2. Stability Analysis with Impurity and Upper Specification Limit (USL = 1.0)
    res_imp <- mSTAB_fn(
        data = df,
        dep = "Impurity",
        time = "Month",
        batch = "Batch",
        specType = "upper",
        usl = 1.0,
        alphaPool = 0.25,
        confLevel = 0.95,
        modelChoice = "auto",
        accelCondition = "no_change",
        showPoolTable = TRUE,
        showShelfTable = TRUE,
        showStabPlot = TRUE,
        showShelfPlot = TRUE,
        showFacetPlot = TRUE,
        showRatePlot = TRUE,
        showPredPlot = TRUE,
        showDiagPlot = TRUE
    )

    expect_s3_class(res_imp, "mSTABResults")
    expect_true(res_imp$shelfTable$rowCount >= 4)

    # Test plot rendering for upper specification limit
    pdf_imp1 <- tempfile(fileext = ".pdf")
    pdf_imp2 <- tempfile(fileext = ".pdf")
    expect_no_error(res_imp$predPlot$saveAs(pdf_imp1))
    expect_no_error(res_imp$facetPlot$saveAs(pdf_imp2))
    expect_true(file.exists(pdf_imp1) && file.info(pdf_imp1)$size > 1000)
    expect_true(file.exists(pdf_imp2) && file.info(pdf_imp2)$size > 1000)
    unlink(c(pdf_imp1, pdf_imp2))

    # 3. Forced Model 2 (Common slope, separate intercepts)
    res_m2 <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        modelChoice = "common_slope_sep_int",
        showPoolTable = TRUE,
        showShelfTable = TRUE
    )

    expect_s3_class(res_m2, "mSTABResults")
    expect_true(res_m2$shelfTable$rowCount >= 4)

    # 4. Check that factor levels with 'Batch' prefix do not duplicate to 'BatchBatch'
    df_batch_prefixed <- df
    levels(df_batch_prefixed$Batch) <- paste("Batch", levels(df_batch_prefixed$Batch))
    res_m1_prefixed <- mSTAB_fn(
        data = df_batch_prefixed,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        modelChoice = "sep_slopes_sep_int",
        showCoefTable = TRUE,
        showShelfTable = TRUE
    )
    coef_df <- res_m1_prefixed$coefTable$asDF
    expect_false(any(grepl("BatchBatch", coef_df$term)))
    expect_true(any(grepl("^Batch 2", coef_df$term)))

    shelf_df <- res_m1_prefixed$shelfTable$asDF
    expect_true(any(grepl("Batch 1", shelf_df$batch)))
})
