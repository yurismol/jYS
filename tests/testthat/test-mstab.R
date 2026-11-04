test_that("mSTAB works", {
    if (!exists(".", envir = .GlobalEnv, mode = "function")) .GlobalEnv$. <- jmvcore::.
    try(jmvcore:::initProtoBuf(), silent = TRUE)
    r_files <- list.files(c("R", "jYS/R", "../../R", "../R"), pattern = "^mstab\\.[hb]\\.R$", full.names = TRUE)
    for (rf in rev(sort(r_files))) source(rf)
    mSTAB_fn <- mSTAB

    # Load test stability dataset
    csv_paths <- c(
        "../../../SAMPLE/STAB/STAB_ICH.csv",
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
        dissolution <- 97.0 - 0.14 * months + rnorm(48, 0, 0.5)
        df <- data.frame(
            Batch = factor(batches),
            Month = months,
            Temp = rep(c(25, 30, 40), length.out = 48),
            Condition = factor(rep(c("25°C / 60% RH", "30°C / 65% RH", "40°C / 75% RH"), length.out = 48)),
            Assay = assay,
            Impurity = impurity,
            Dissolution = dissolution
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
        showBatchPlots = TRUE,
        showRatePlot = TRUE,
        showPredPlot = TRUE,
        showDiagPlot = TRUE
    )

    expect_s3_class(res_assay, "mSTABResults")
    expect_s3_class(res_assay$regTable, "Table")
    expect_s3_class(res_assay$poolTable, "Table")
    expect_s3_class(res_assay$shelfTable, "Table")
    expect_s3_class(res_assay$coefTable, "Table")
    expect_s3_class(res_assay$diagTable, "Table")
    expect_true(res_assay$regTable$rowCount >= 8)
    expect_true(res_assay$poolTable$rowCount >= 3)
    expect_true(res_assay$shelfTable$rowCount >= 4)
    expect_true(res_assay$coefTable$rowCount >= 2)
    expect_true(res_assay$diagTable$rowCount >= 4)

    # Verify table notes
    expect_true(length(res_assay$poolTable$notes) >= 1)
    expect_true(length(res_assay$shelfTable$notes) >= 1)
    expect_true(length(res_assay$coefTable$notes) >= 1)
    expect_true(length(res_assay$diagTable$notes) >= 1)

    # Verify plot rendering without graphics/grid errors for all 6 plots
    pdf_tmp1 <- tempfile(fileext = ".pdf")
    pdf_tmp2 <- tempfile(fileext = ".pdf")
    pdf_tmp3 <- tempfile(fileext = ".pdf")
    pdf_tmp4 <- tempfile(fileext = ".pdf")
    pdf_tmp5 <- tempfile(fileext = ".pdf")
    pdf_tmp6 <- tempfile(fileext = ".pdf")
    pdf_tmp7 <- tempfile(fileext = ".pdf")

    expect_no_error(res_assay$stabPlot$saveAs(pdf_tmp1))
    expect_no_error(res_assay$shelfPlot$saveAs(pdf_tmp2))
    expect_no_error(res_assay$facetPlot$saveAs(pdf_tmp3))
    expect_no_error(res_assay$ratePlot$saveAs(pdf_tmp4))
    expect_no_error(res_assay$predPlot$saveAs(pdf_tmp5))
    expect_no_error(res_assay$diagPlot$saveAs(pdf_tmp6))
    expect_true(length(res_assay$batchPlots$itemKeys) >= 1)
    expect_no_error(res_assay$batchPlots$get(key = res_assay$batchPlots$itemKeys[[1]])$saveAs(pdf_tmp7))

    # Verify PDF export for individual batch plots via saveAs (following other jYS modules like mSRV)
    pdf_bp_items <- list()
    for (b_key in res_assay$batchPlots$itemKeys) {
        pdf_f <- tempfile(fileext = ".pdf")
        pdf_bp_items[[length(pdf_bp_items) + 1]] <- pdf_f
        b_item <- res_assay$batchPlots$get(key = b_key)
        expect_no_error(b_item$saveAs(pdf_f))
        expect_true(file.exists(pdf_f) && file.info(pdf_f)$size > 1000)
    }
    # Verify native Jamovi .savePart for individual batch plot (part = 'batchPlots/"key"')
    first_b <- as.character(res_assay$batchPlots$itemKeys[[1]])
    pdf_bp_native <- tempfile(fileext = ".pdf")
    expect_no_error(res_assay$analysis$.savePart(pdf_bp_native, paste0('batchPlots/"', first_b, '"')))
    expect_true(file.exists(pdf_bp_native) && file.info(pdf_bp_native)$size > 1000)
    unlink(c(unlist(pdf_bp_items), pdf_bp_native))

    # Verify that running a new analysis instance produces matching batchPlots items and states
    res_restored <- mSTABClass$new(options = res_assay$options, data = df)
    res_restored$init()
    res_restored$run()
    expect_equal(length(res_restored$results$batchPlots$itemKeys), length(res_assay$batchPlots$itemKeys))
    for (bk in res_restored$results$batchPlots$itemKeys) {
        it_restored <- res_restored$results$batchPlots$get(key = bk)
        expect_false(is.null(it_restored$state))
    }

    expect_true(file.exists(pdf_tmp1) && file.info(pdf_tmp1)$size > 1000)
    expect_true(file.exists(pdf_tmp2) && file.info(pdf_tmp2)$size > 1000)
    expect_true(file.exists(pdf_tmp3) && file.info(pdf_tmp3)$size > 1000)
    expect_true(file.exists(pdf_tmp4) && file.info(pdf_tmp4)$size > 1000)
    expect_true(file.exists(pdf_tmp5) && file.info(pdf_tmp5)$size > 1000)
    expect_true(file.exists(pdf_tmp6) && file.info(pdf_tmp6)$size > 1000)
    expect_true(file.exists(pdf_tmp7) && file.info(pdf_tmp7)$size > 1000)
    unlink(c(pdf_tmp1, pdf_tmp2, pdf_tmp3, pdf_tmp4, pdf_tmp5, pdf_tmp6, pdf_tmp7))

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
        showBatchPlots = TRUE,
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

    # 5. Multi-Attribute Worst-Case Analysis
    res_multi <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        dep2 = "Impurity",
        specType2 = "upper",
        usl2 = 1.0,
        dep3 = "Dissolution",
        specType3 = "lower",
        lsl3 = 80.0,
        showMultiShelfTable = TRUE,
        showMultiBatchTable = TRUE,
        showMultiShelfPlot = TRUE
    )

    expect_s3_class(res_multi, "mSTABResults")
    expect_true(res_multi$multiShelfTable$rowCount >= 4)
    expect_true(res_multi$multiBatchTable$rowCount >= 3)
    pdf_multi <- tempfile(fileext = ".pdf")
    expect_no_error(res_multi$multiShelfPlot$saveAs(pdf_multi))
    expect_true(file.exists(pdf_multi) && file.info(pdf_multi)$size > 1000)
    unlink(pdf_multi)

    # 6. Arrhenius Degradation Kinetics with Temperature Variable
    if ("Temp" %in% names(df)) {
        res_arrh <- mSTAB_fn(
            data = df,
            dep = "Assay",
            time = "Month",
            batch = "Batch",
            specType = "lower",
            lsl = 90.0,
            showArrh = TRUE,
            tempVar = "Temp",
            arrhEa = 83.14,
            showArrhSens = TRUE,
            showArrhPlot = TRUE,
            showArrhTrajPlot = TRUE
        )

        expect_s3_class(res_arrh, "mSTABResults")
        expect_true(res_arrh$arrhTable$rowCount >= 5)
        expect_true(res_arrh$arrhRegTable$rowCount >= 2)
        expect_true(res_arrh$arrhSensTable$rowCount >= 9)

        pdf_arrh1 <- tempfile(fileext = ".pdf")
        pdf_arrh2 <- tempfile(fileext = ".pdf")
        expect_no_error(res_arrh$arrhPlot$saveAs(pdf_arrh1))
        expect_no_error(res_arrh$arrhTrajPlot$saveAs(pdf_arrh2))
        expect_true(file.exists(pdf_arrh1) && file.info(pdf_arrh1)$size > 1000)
        expect_true(file.exists(pdf_arrh2) && file.info(pdf_arrh2)$size > 1000)
        unlink(c(pdf_arrh1, pdf_arrh2))
    }

    # 7. Single Batch Stability Analysis (poolTable NAs and single regression)
    df_single <- df[df$Batch == levels(df$Batch)[1], ]
    res_single <- mSTAB_fn(
        data = df_single,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        showPoolTable = TRUE,
        showShelfTable = TRUE,
        showStabPlot = TRUE,
        showShelfPlot = TRUE
    )
    expect_s3_class(res_single, "mSTABResults")
    expect_true(res_single$poolTable$rowCount >= 3)
    p_df <- res_single$poolTable$asDF
    expect_true(all(is.na(p_df$p)))
    st_df <- res_single$shelfTable$asDF
    expect_true(nrow(st_df) >= 2)
    expect_false(any(st_df == 999, na.rm = TRUE))

    # 8. Two-Sided Specification Bounds (specType = "both")
    res_both <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "both",
        lsl = 90.0,
        usl = 105.0,
        showPoolTable = TRUE,
        showShelfTable = TRUE,
        showStabPlot = TRUE,
        showShelfPlot = TRUE
    )
    expect_s3_class(res_both, "mSTABResults")
    st_both <- res_both$shelfTable$asDF
    expect_true(nrow(st_both) >= 4)
    ov_both <- st_both[grepl("OVERALL", st_both$batch), ]
    expect_true(nrow(ov_both) == 1 && !is.na(ov_both$ichShelf))

    # 9. Infinite Shelf Life / Flat Degradation (no crossing within 240 months, no 999 sentinel)
    set.seed(123)
    df_flat <- df
    df_flat$AssayFlat <- 100 + rnorm(nrow(df), 0, 0.05)
    res_flat <- mSTAB_fn(
        data = df_flat,
        dep = "AssayFlat",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        showPoolTable = TRUE,
        showShelfTable = TRUE,
        showStabPlot = TRUE,
        showShelfPlot = TRUE
    )
    expect_s3_class(res_flat, "mSTABResults")
    st_flat <- res_flat$shelfTable$asDF
    ov_flat <- st_flat[grepl("OVERALL", st_flat$batch), ]
    expect_true(nrow(ov_flat) == 1)
    expect_true(is.na(ov_flat$statShelf))
    expect_false(any(st_flat == 999, na.rm = TRUE))
    expect_true(ov_flat$ichShelf == 36)

    # 10. Single time point batch test (ensures mse_cs is passed and handled without error)
    df_single_tp <- data.frame(
        Batch = factor(c("A", "A", "B")),
        Month = c(0, 6, 0),
        Assay = c(100.0, 98.0, 100.0)
    )
    res_single_tp <- mSTAB_fn(
        data = df_single_tp,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        showPoolTable = TRUE,
        showShelfTable = TRUE
    )
    expect_s3_class(res_single_tp, "mSTABResults")
    pt_df <- res_single_tp$poolTable$asDF
    expect_true(nrow(pt_df) >= 3)
    step2_row <- pt_df[grepl("Step 2", pt_df$step), ]
    expect_true(is.na(step2_row$df))
    expect_true(grepl("Not tested", step2_row$decision))

    # 11. Multi-attribute with missing batch data for an attribute
    df_multi_miss <- data.frame(
        Batch = factor(c("B1", "B1", "B2", "B2", "B3", "B3")),
        Month = c(0, 12, 0, 12, 0, 12),
        Assay = c(100, 96, 100, 95, 100, 97),
        Dissolution = c(95, 90, 95, 91, NA, NA)
    )
    res_multi_miss <- mSTAB_fn(
        data = df_multi_miss,
        dep = "Assay",
        dep2 = "Dissolution",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        specType2 = "lower",
        lsl2 = 80.0
    )
    expect_s3_class(res_multi_miss, "mSTABResults")
    mb_df <- res_multi_miss$multiBatchTable$asDF
    expect_true(nrow(mb_df) >= 4)
    b3_row <- mb_df[grepl("B3", mb_df$batch), ]
    expect_true(is.na(b3_row$attr2))

    # 12. Arrhenius kinetics default long-term behavior and extrapolation cap
    res_arrh_def <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        showArrh = TRUE,
        arrhDataCondition = "long_term",
        accelCondition = "no_change",
        studyDuration = 36
    )
    expect_s3_class(res_arrh_def, "mSTABResults")
    arrh_df <- res_arrh_def$arrhTable$asDF
    long_row <- arrh_df[grepl("Long-term", arrh_df$zone), ]
    expect_true(nrow(long_row) == 1)
    expect_true(!is.na(long_row$predShelf))
    expect_true(long_row$predShelf <= 48)

    # 13. Secondary attribute invalid data triggers warning and undetermined overall shelf life
    df_log_neg <- df
    df_log_neg$NegImpurity <- df_log_neg$Impurity
    df_log_neg$NegImpurity[1] <- -1.0
    res_sec_inv <- mSTAB_fn(
        data = df_log_neg,
        dep = "Assay",
        dep2 = "NegImpurity",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        specType2 = "upper",
        usl2 = 1.0,
        trans2 = "log"
    )
    expect_s3_class(res_sec_inv, "mSTABResults")
    ms_df <- res_sec_inv$multiShelfTable$asDF
    ov_row <- ms_df[grepl("Overall", ms_df$attr), ]
    expect_true(nrow(ov_row) == 1 && is.na(ov_row$statShelf))

    # 14. Multi-attribute slot-based column mapping (dep and dep3 without dep2)
    res_slot <- mSTAB_fn(
        data = df,
        dep = "Assay",
        dep3 = "Dissolution",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        specType3 = "lower",
        lsl3 = 80.0,
        showMultiShelfTable = TRUE,
        showMultiBatchTable = TRUE
    )
    expect_s3_class(res_slot, "mSTABResults")
    mb_slot_df <- res_slot$multiBatchTable$asDF
    # attr1 should have Assay values, attr2 should be all NA, attr3 should have Dissolution values
    expect_true(!all(is.na(mb_slot_df$attr1)))
    expect_true(all(is.na(mb_slot_df$attr2)))
    expect_true(!all(is.na(mb_slot_df$attr3)))

    ms_slot_df <- res_slot$multiShelfTable$asDF
    # LSL formatted as >= 90 (LSL)
    expect_true(any(grepl("\\(LSL\\)", ms_slot_df$spec)))

    # 15. Forced Model 3 does not assign a single batch as limiting
    res_m3 <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        modelChoice = "common_slope_common_int",
        showShelfTable = TRUE
    )
    expect_s3_class(res_m3, "mSTABResults")
    st_m3 <- res_m3$shelfTable$asDF
    expect_true(all(st_m3$limiting[st_m3$batch != "<b>OVERALL / PROPOSED</b>"] == "Common line"))
    ov_m3 <- st_m3[grepl("OVERALL", st_m3$batch), ]
    expect_true(grepl("Common line", ov_m3$limiting) || grepl("extrapolation limit", ov_m3$limiting))

    # 16. Step 3: Data integrity checks & input mistake prevention notices
    df_step3_base <- data.frame(
        Time = c(0, 6, 12, 0, 6, 12),
        Batch = c("B1", "B1", "B1", "B2", "B2", "B2"),
        Assay = c(100, 98, 96, 99.5, 97.5, 95.5)
    )

    # 16.1 Duplicate records detection (exact duplicate rows) vs legitimate replicates
    df_dups <- rbind(df_step3_base, data.frame(Time = 12, Batch = "B1", Assay = 96))
    res_dups <- mSTAB_fn(data = df_dups, dep = "Assay", time = "Time", batch = "Batch", lsl = 90)
    expect_true("duplicate_records_warn" %in% names(res_dups))

    df_repl <- rbind(df_step3_base, data.frame(Time = 12, Batch = "B1", Assay = 96.4))
    res_repl <- mSTAB_fn(data = df_repl, dep = "Assay", time = "Time", batch = "Batch", lsl = 90)
    expect_false("duplicate_records_warn" %in% names(res_repl))

    # 16.2 Time units advisory (> 120 months)
    df_days <- data.frame(
        Time = c(0, 90, 180, 0, 90, 180),
        Batch = c("B1", "B1", "B1", "B2", "B2", "B2"),
        Assay = c(100, 98, 96, 99.5, 97.5, 95.5)
    )
    res_days <- mSTAB_fn(data = df_days, dep = "Assay", time = "Time", batch = "Batch", lsl = 90)
    expect_true("time_units_info" %in% names(res_days))

    # 16.3 Study duration mismatch
    res_dur_mis <- mSTAB_fn(data = df_step3_base, dep = "Assay", time = "Time", batch = "Batch", lsl = 90, studyDuration = 6)
    expect_true("study_duration_mismatch_warn" %in% names(res_dur_mis))

    # 16.4 Specification limit sanity check (LSL >= USL)
    expect_error(
        mSTAB_fn(data = df_step3_base, dep = "Assay", time = "Time", batch = "Batch", specType = "both", lsl = 100, usl = 95),
        "strictly less than"
    )

    # 16.5 Baseline out-of-specification (Time = 0)
    df_oos <- df_step3_base
    df_oos$Assay[df_oos$Time == 0 & df_oos$Batch == "B1"] <- 89
    res_oos <- mSTAB_fn(data = df_oos, dep = "Assay", time = "Time", batch = "Batch", lsl = 90)
    expect_true("init_oos_warn_dep" %in% names(res_oos))

    # 16.6 Regulatory intermediate testing requirement notice
    res_inter_req <- mSTAB_fn(
        data = df_step3_base, dep = "Assay", time = "Time", batch = "Batch", lsl = 90,
        storageCond = "room", accelResult = "change_observed", intermedResult = "not_applicable"
    )
    expect_true("intermed_missing_warn" %in% names(res_inter_req))

    # 16.7 Refrigerated 3-month accelerated change notice
    res_refrig <- mSTAB_fn(
        data = df_step3_base, dep = "Assay", time = "Time", batch = "Batch", lsl = 90,
        storageCond = "refrigerated", accelResult = "change_refrig_early"
    )
    expect_true("refrig_3m_warn" %in% names(res_refrig))

    # 16.8 Frozen product accelerated failure notice
    res_frozen <- mSTAB_fn(
        data = df_step3_base, dep = "Assay", time = "Time", batch = "Batch", lsl = 90,
        storageCond = "frozen", accelResult = "change_observed"
    )
    expect_true("frozen_accel_warn" %in% names(res_frozen))

    # 16.9 Claimed 'little change' vs statistically significant trend notice
    df_steep <- data.frame(
        Time = c(0, 6, 12, 18, 24, 0, 6, 12, 18, 24),
        Batch = c(rep("B1", 5), rep("B2", 5)),
        Assay = c(100, 95, 90, 85, 80, 100, 99, 98, 97, 96)
    )
    res_steep <- mSTAB_fn(
        data = df_steep, dep = "Assay", time = "Time", batch = "Batch", lsl = 70,
        littleChange = TRUE
    )
    expect_true("little_change_discrepancy_info" %in% names(res_steep))

    # 16.10 Proposed shelf life exceeds allowable ICH Q1E shelf life notice
    res_prop_exc <- mSTAB_fn(
        data = df_step3_base, dep = "Assay", time = "Time", batch = "Batch", lsl = 90,
        storageCond = "room", accelResult = "not_tested", proposedShelfLife = 24
    )
    expect_true("proposed_shelf_exceeds_warn" %in% names(res_prop_exc))

    # 16.11 Model diagnostic residual normality violation notice
    df_outlier <- data.frame(
        Time = rep(c(0, 3, 6, 9, 12), 4),
        Batch = rep(paste0("B", 1:4), each = 5),
        Assay = 100 - rep(c(0, 3, 6, 9, 12), 4) * 0.2
    )
    df_outlier$Assay[1] <- 50
    res_outlier <- mSTAB_fn(data = df_outlier, dep = "Assay", time = "Time", batch = "Batch", lsl = 40)
    expect_true("normality_violation_warn" %in% names(res_outlier))

    # 17. Step 4: Multifactor ANCOVA stability studies (ICH Q1E Section B.3.2.2.1)
    set.seed(42)
    strengths <- c("100 mg", "200 mg")
    bottles   <- c("30 count", "60 count")
    times     <- c(0, 6, 12, 18, 24)
    rows_s4 <- list()
    for (s in strengths) {
        batches <- if (s == "100 mg") c("B1", "B2", "B3") else c("B4", "B5", "B6")
        for (b in batches) {
            for (bt in bottles) {
                slope_s <- if (s == "100 mg") -0.22 else -0.25
                slope_bt <- if (s == "200 mg" && bt == "60 count") -0.15 else 0.0
                slope_b <- rnorm(1, 0, 0.005)
                slope_tot <- slope_s + slope_bt + slope_b
                int_b <- 100 + rnorm(1, 0, 0.1)
                for (tm in times) {
                    y_val <- int_b + slope_tot * tm + rnorm(1, 0, 0.08)
                    rows_s4[[length(rows_s4) + 1]] <- data.frame(
                        Strength = s,
                        Bottle = bt,
                        Batch = b,
                        Time = tm,
                        Assay = y_val,
                        stringsAsFactors = FALSE
                    )
                }
            }
        }
    }
    df_sec2 <- do.call(rbind, rows_s4)

    # 17.1 Multifactor mSTAB execution and results structure
    res_mf <- mSTAB_fn(
        data = df_sec2,
        dep = "Assay",
        time = "Time",
        batch = "Batch",
        factors = c("Strength", "Bottle"),
        lsl = 90.0,
        storageCond = "room",
        accelResult = "no_change",
        showComboShelfTable = TRUE,
        showPoolTable = TRUE
    )
    expect_s3_class(res_mf, "mSTABResults")
    expect_true("comboShelfTable" %in% names(res_mf))
    expect_true("regTable" %in% names(res_mf))
    expect_equal(res_mf$regTable$rowCount, 10)
    reg_df <- res_mf$regTable$asDF
    expect_true(any(grepl("Intended Storage Condition", reg_df$param)))
    expect_true(any(grepl("Extrapolation Ceiling", reg_df$param)))

    # 17.2 comboShelfTable factor combinations and limiting governing combination
    combo_df <- res_mf$comboShelfTable$asDF
    expect_equal(nrow(combo_df), 5)
    expect_true(any(grepl("100 mg | 30 count", combo_df$combo, fixed = TRUE)))
    expect_true(any(grepl("100 mg | 60 count", combo_df$combo, fixed = TRUE)))
    expect_true(any(grepl("200 mg | 30 count", combo_df$combo, fixed = TRUE)))
    expect_true(any(grepl("200 mg | 60 count", combo_df$combo, fixed = TRUE)))
    expect_true(any(grepl("OVERALL PRODUCT", combo_df$combo, fixed = TRUE)))

    lim_row <- combo_df[grepl("200 mg | 60 count", combo_df$combo, fixed = TRUE), ]
    expect_true(grepl("Limiting combination", lim_row$limiting))
    expect_true(lim_row$ichShelf < 30)

    overall_row <- combo_df[grepl("OVERALL PRODUCT", combo_df$combo, fixed = TRUE), ]
    expect_true(grepl("Governing Minimum", overall_row$limiting))
    expect_equal(overall_row$ichShelf, lim_row$ichShelf)

    # 17.3 Multifactor poolTable 4-step hierarchy
    pool_df <- res_mf$poolTable$asDF
    expect_true(any(grepl("Step 1", pool_df$step)))
    expect_true(any(grepl("Step 2", pool_df$step)))
    expect_true(any(grepl("Residual Error", pool_df$step)))

    # 17.4 Single-factor backward compatibility
    res_single <- mSTAB_fn(
        data = df_sec2[df_sec2$Strength == "100 mg" & df_sec2$Bottle == "30 count", ],
        dep = "Assay",
        time = "Time",
        batch = "Batch",
        lsl = 90.0
    )
    expect_s3_class(res_single, "mSTABResults")
    expect_true(nrow(res_single$shelfTable$asDF) >= 4)

    # 17.5 Storage conditions (refrigerated, frozen)
    res_refrig <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        storageCond = "refrigerated",
        accelResult = "no_change",
        showRegTable = TRUE
    )
    expect_s3_class(res_refrig, "mSTABResults")
    reg_ref <- res_refrig$regTable$asDF
    expect_true(any(grepl("Refrigerated", reg_ref$value)))

    res_froz <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        storageCond = "frozen",
        accelResult = "not_tested",
        showRegTable = TRUE
    )
    expect_s3_class(res_froz, "mSTABResults")
    reg_frz <- res_froz$regTable$asDF
    expect_true(any(grepl("Frozen", reg_frz$value)))
    expect_true(any(grepl("no extrapolation", reg_frz$value, ignore.case = TRUE)))

    # 17.6 Significant degradation slope with littleChange = TRUE triggers notice
    res_lc_notice <- mSTAB_fn(
        data = df,
        dep = "Assay",
        time = "Month",
        batch = "Batch",
        specType = "lower",
        lsl = 90.0,
        littleChange = TRUE
    )
    expect_s3_class(res_lc_notice, "mSTABResults")
})

