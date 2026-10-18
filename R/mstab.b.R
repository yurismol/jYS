# This file is a generated template, your changes will not be overwritten

mSTABClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mSTABClass",
    inherit = mSTABBase,
    private = list(

        .formatNum = function(x, digits = 1) {
            if (is.null(x) || length(x) == 0) return("")
            format_one <- function(v) {
                if (is.na(v) || is.infinite(v)) return("")
                as.character(round(v, digits))
            }
            if (length(x) == 1) {
                format_one(x)
            } else {
                vapply(x, format_one, character(1), USE.NAMES = FALSE)
            }
        },

        .init = function() {
            # Initial instruction content
            self$results$instructions$setContent(
                paste0(
                    "<p><b>",
                    .("Stability Analysis (ICH Q1E)"),
                    "</b></p><p>",
                    .("Evaluates drug substance or drug product stability data and estimates shelf life / retest period according to the ICH Q1E guideline."),
                    "</p><p><b>",
                    .("Getting started:"),
                    "</b> ",
                    .("Select a quantitative attribute (e.g. Assay % or Impurities %), a continuous time variable (e.g. Months), and a batch/lot factor variable. Specify your acceptance criteria (LSL and/or USL)."),
                    "</p>"
                )
            )

            # Table footnotes
            private$.initNotes()
        },

        .initNotes = function() {
            # 1. poolTable notes
            poolTable <- self$results$poolTable
            poolTable$setNote(
                "ancova_rule",
                .("<b>ICH Q1E Poolability Hierarchy</b>: ANCOVA evaluates whether stability batches can be pooled into a common regression model. Testing proceeds sequentially: Step 1 tests equality of degradation rates (slopes; Batch * Time interaction); Step 2 tests equality of initial levels (intercepts; Batch main effect).")
            )
            poolTable$setNote(
                "step1_slope",
                .("<b>Step 1 (Equality of Slopes)</b>: Tests H<sub>0</sub>: β<sub>1,1</sub> = β<sub>1,2</sub> = ... = β<sub>1,k</sub>. If p >= 0.25, batch degradation rates are statistically comparable (parallel lines). If p < 0.25, degradation rates differ significantly, requiring separate slopes for each batch.")
            )
            poolTable$setNote(
                "step2_int",
                .("<b>Step 2 (Equality of Intercepts)</b>: Evaluated only when slopes are pooled (p >= 0.25 in Step 1). Tests H<sub>0</sub>: β<sub>0,1</sub> = β<sub>0,2</sub> = ... = β<sub>0,k</sub>. If p >= 0.25, batches share a single common regression line. If p < 0.25, batches differ in initial release values, requiring separate intercepts with a common slope.")
            )
            poolTable$setNote(
                "alpha_note",
                .("<b>Significance Level (alpha = 0.25)</b>: In accordance with ICH Q1E Appendix B, a conservative significance level of alpha = 0.25 (rather than 0.05) is employed to minimize Type II error (erroneously pooling batches with clinically relevant differences).")
            )
            poolTable$setNote(
                "mse_note",
                .("<b>Residual Mean Square (MSE)</b>: When separate slopes are required, ICH Q1E recommends testing each batch against specifications using the pooled residual mean square error across all batches if batch error variances are homogeneous.")
            )

            # 2. shelfTable notes
            shelfTable <- self$results$shelfTable
            shelfTable$setNote(
                "shelf_def",
                .("<b>Statistical Shelf Life / Retest Period</b>: Determined by the earliest time point where the appropriate 95% confidence bound for the mean degradation line intersects the specification limit (LSL and/or USL).")
            )
            shelfTable$setNote(
                "bound_rule",
                .("<b>Confidence Bound Direction (ICH Q1E Section 2.4)</b>: For decreasing attributes (e.g., Assay), the 95% lower one-sided confidence bound is used. For increasing attributes (e.g., Impurities / Degradants), the 95% upper one-sided confidence bound is used. If the direction of change is unknown, a 95% two-sided bound is applied.")
            )
            shelfTable$setNote(
                "batch_min",
                .("<b>Batch Minimum Principle</b>: When batch-specific estimates remain (separate slopes or separate intercepts), ICH Q1E mandates that the overall estimated shelf life / retest period is governed by the shortest (earliest) estimate among all evaluated batches.")
            )
            shelfTable$setNote(
                "extrap_rule",
                .("<b>ICH Q1E Appendix A Extrapolation</b>: Permitted shelf life may extend beyond real-time data when supported by accelerated (and, if applicable, intermediate) stability data. Up to 2x observed duration (capped at X + 12 months) is allowed if accelerated data show no significant change; up to 1.5x (capped at X + 6 months) if significant change occurs at accelerated but not intermediate conditions. If not evaluated or change is observed without intermediate support, no extrapolation beyond observed duration X is permitted. Extrapolation is never permitted beyond the statistical estimate.")
            )
            shelfTable$setNote(
                "retest_shelf",
                .("<b>Retest Period vs Shelf Life</b>: Retest period applies to drug substances (active pharmaceutical ingredients), after which material must be re-tested before use. Shelf life / expiry dating applies to final marketed drug products.")
            )

            # 3. coefTable notes
            coefTable <- self$results$coefTable
            coefTable$setNote(
                "term_def",
                .("<b>Model Parameters</b>: Intercept represents the estimated mean attribute value at time zero (t = 0 months). Time slope represents the mean rate of degradation or accumulation per month. Batch terms represent deviations of each batch relative to the reference batch.")
            )
            coefTable$setNote(
                "ci_note",
                .("<b>Confidence Intervals & Standard Errors</b>: Standard errors and 95% confidence intervals reflect parameter estimation precision computed from the model covariance matrix and residual degrees of freedom.")
            )
            coefTable$setNote(
                "slope_interp",
                .("<b>Slope Interpretation</b>: A negative slope indicates loss of active substance over storage time; a positive slope indicates accumulation of degradation impurities or breakdown products.")
            )

            # 4. diagTable notes
            diagTable <- self$results$diagTable
            diagTable$setNote(
                "r2_def",
                .("<b>Coefficient of Determination (R² / Adj R²)</b>: Measures the proportion of total variance in the stability attribute accounted for by storage time and batch factors. Adjusted R² penalizes for model complexity.")
            )
            diagTable$setNote(
                "sigma_def",
                .("<b>Residual Standard Error (s)</b>: Estimate of residual standard deviation (square root of MSE). Reflects unexplained analytical measurement variability and within-batch variation.")
            )
            diagTable$setNote(
                "sw_def",
                .("<b>Residual Normality (Shapiro-Wilk)</b>: Tests whether regression residuals follow a normal distribution (H0: normal). Validates the use of the standard t-distribution for 95% confidence bounds (p >= 0.05 indicates conformity).")
            )
            diagTable$setNote(
                "homo_def",
                .("<b>Homoscedasticity</b>: Evaluates whether residual variance remains constant over time (H0: constant variance). Confirms suitability of linear least-squares regression without weighted errors.")
            )
            diagTable$setNote(
                "dw_def",
                .("<b>Autocorrelation (Durbin-Watson d)</b>: Tests for first-order autocorrelation in residuals across sequential test intervals. Values near 2.0 indicate independent errors; values substantially below 1.5 suggest positive serial correlation.")
            )

            # 5. multiShelfTable notes
            multiShelfTable <- self$results$multiShelfTable
            multiShelfTable$setNote(
                "worst_case_rule",
                .("<b>Multi-Attribute Worst-Case Principle</b>: Shelf life across multiple analytical attributes is governed by the earliest failure time among all evaluated parameters: Shelf-Life<sub>overall</sub> = min(Shelf-Life<sub>1</sub>, ..., Shelf-Life<sub>m</sub>).")
            )
            multiShelfTable$setNote(
                "cqa_rule",
                .("<b>Critical Quality Attribute (CQA) Driver</b>: The parameter that exhibits the shortest shelf life acts as the governing degradation bottleneck and defines the product expiration date.")
            )

            # 6. multiBatchTable notes
            multiBatchTable <- self$results$multiBatchTable
            multiBatchTable$setNote(
                "batch_matrix_rule",
                .("<b>Batch Evaluation Matrix</b>: Displays individual shelf-life estimates for each batch across all parameters. Batch minimum represents the row-wise worst case; product minimum represents the overall matrix minimum.")
            )

            # 7. arrhTable notes
            arrhTable <- self$results$arrhTable
            arrhTable$setNote(
                "arrh_def",
                .("<b>Arrhenius Kinetics Recalculation</b>: Recalculates chemical degradation rates according to the Arrhenius relation ln(k) = ln(A) – Ea/(R*T) + B*RH. Acceleration factors (AF) predict long-term storage kinetics from accelerated data.")
            )
            arrhTable$setNote(
                "zones_def",
                .("<b>Climatic Zones</b>: Predicts degradation rates and expiration dating across standard ICH zones: Zone II (25°C/60% RH), Zone IVa (30°C/65% RH), Zone IVb (30°C/75% RH), and Refrigerated (5°C).")
            )

            # 8. arrhSensTable notes
            arrhSensTable <- self$results$arrhSensTable
            arrhSensTable$setNote(
                "ea_sens_note",
                .("<b>Activation Energy Sensitivity</b>: Assesses prediction stability across realistic activation energies (50 to 120 kJ/mol). Lower Ea values represent more conservative (lower) acceleration factors.")
            )

            # 9. arrhRegTable notes
            arrhRegTable <- self$results$arrhRegTable
            arrhRegTable$setNote(
                "reg_note",
                .("<b>Multi-Temperature Regression</b>: Empirically estimates activation energy (Ea = -slope * R) and pre-exponential factor (A = exp(intercept)) from linear regression of ln(k) vs. 1/T across tested temperatures.")
            )
        },

        .run = function() {
            if (is.null(self$options$dep) || is.null(self$options$time) || is.null(self$options$batch)) {
                return()
            }

            depName   <- self$options$dep
            timeName  <- self$options$time
            batchName <- self$options$batch

            data <- self$data
            if (is.null(data) || nrow(data) == 0) {
                return()
            }

            # Build list of active attributes to evaluate
            attrDefs <- list(
                list(name = depName, specType = self$options$specType, lsl = self$options$lsl, usl = self$options$usl, trans = self$options$trans, id = "dep", title = .("Attribute 1 (Assay / Primary)"))
            )
            if (length(self$options$dep2) > 0 && nzchar(self$options$dep2)) {
                attrDefs[[length(attrDefs) + 1]] <- list(
                    name = self$options$dep2, specType = self$options$specType2, lsl = self$options$lsl2, usl = self$options$usl2, trans = self$options$trans2, id = "dep2", title = .("Attribute 2 (Impurities / Degradants)")
                )
            }
            if (length(self$options$dep3) > 0 && nzchar(self$options$dep3)) {
                attrDefs[[length(attrDefs) + 1]] <- list(
                    name = self$options$dep3, specType = self$options$specType3, lsl = self$options$lsl3, usl = self$options$usl3, trans = self$options$trans3, id = "dep3", title = .("Attribute 3 (Dissolution / Physical)")
                )
            }
            if (length(self$options$dep4) > 0 && nzchar(self$options$dep4)) {
                attrDefs[[length(attrDefs) + 1]] <- list(
                    name = self$options$dep4, specType = self$options$specType4, lsl = self$options$lsl4, usl = self$options$usl4, trans = self$options$trans4, id = "dep4", title = .("Attribute 4 (Optional)")
                )
            }
            if (length(self$options$dep5) > 0 && nzchar(self$options$dep5)) {
                attrDefs[[length(attrDefs) + 1]] <- list(
                    name = self$options$dep5, specType = self$options$specType5, lsl = self$options$lsl5, usl = self$options$usl5, trans = self$options$trans5, id = "dep5", title = .("Attribute 5 (Optional)")
                )
            }

            evalData <- data
            evalIsLong <- FALSE
            if (length(self$options$tempVar) > 0 && nzchar(self$options$tempVar)) {
                tempCol <- self$options$tempVar
                if (tempCol %in% names(data)) {
                    tVals <- as.numeric(data[[tempCol]])
                    uTemps <- unique(tVals[!is.na(tVals)])
                    targetLongTemp <- as.numeric(self$options$arrhTempLong)
                    if (length(uTemps) > 1) {
                        if (any(!is.na(tVals) & tVals == targetLongTemp)) {
                            evalData <- data[!is.na(tVals) & tVals == targetLongTemp, , drop = FALSE]
                            evalIsLong <- TRUE
                        } else {
                            evalData <- data[!is.na(tVals) & tVals == min(uTemps), , drop = FALSE]
                            evalIsLong <- TRUE
                        }
                    }
                }
            }

            evaluatedAttrs <- list()
            for (idx in seq_along(attrDefs)) {
                aDef <- attrDefs[[idx]]
                res <- private$.evaluateSingleAttribute(
                    attrDef = aDef,
                    data = evalData,
                    timeName = timeName,
                    batchName = batchName,
                    alphaPool = self$options$alphaPool,
                    confLevel = self$options$confLevel,
                    modelChoice = self$options$modelChoice,
                    accelCondition = self$options$accelCondition,
                    studyDuration = self$options$studyDuration
                )
                if (!is.null(res)) {
                    evaluatedAttrs[[aDef$name]] <- res
                }
            }

            if (length(evaluatedAttrs) == 0) {
                jmvcore::reject(.("At least 3 valid observations are required for stability analysis."))
                return()
            }

            # Multi-Attribute Worst-Case Synthesis: Shelf-Life<sub>overall</sub> = min(Shelf-Life<sub>1</sub>, ..., Shelf-Life<sub>m</sub>)
            allStatShelves <- sapply(evaluatedAttrs, function(x) x$shelfResults$overallStatShelf)
            allIchShelves  <- sapply(evaluatedAttrs, function(x) x$shelfResults$overallIchShelf)

            minStatShelf <- min(allStatShelves, na.rm = TRUE)
            minIchShelf  <- min(allIchShelves, na.rm = TRUE)

            limAttrIdx <- which(allStatShelves == minStatShelf)[1]
            governingAttrName <- names(evaluatedAttrs)[limAttrIdx]
            governingRes <- evaluatedAttrs[[governingAttrName]]
            governingBatch <- governingRes$shelfResults$limitingBatch

            isMulti <- (length(evaluatedAttrs) > 1)

            if (isMulti) {
                # Populate Multi-Attribute Shelf Life Table
                private$.populateMultiShelfTable(evaluatedAttrs, governingAttrName, minStatShelf, minIchShelf, governingBatch)

                # Populate Multi-Attribute Batch Evaluation Matrix
                batchLevels <- governingRes$batchLevels
                private$.populateMultiBatchTable(evaluatedAttrs, batchLevels, minStatShelf)

                # Store lightweight state for multiShelfPlot (avoids serializing heavy lm models)
                firstSr <- governingRes$shelfResults
                compactAttrs <- lapply(evaluatedAttrs, function(res_a) {
                    list(
                        shelfResults = list(
                            overallIchShelf  = res_a$shelfResults$overallIchShelf,
                            overallStatShelf = res_a$shelfResults$overallStatShelf,
                            limitingBatch    = res_a$shelfResults$limitingBatch
                        )
                    )
                })
                self$results$multiShelfPlot$setState(list(
                    evaluatedAttrs    = compactAttrs,
                    governingAttrName = governingAttrName,
                    minStatShelf      = minStatShelf,
                    minIchShelf       = minIchShelf,
                    governingBatch    = governingBatch,
                    t_dur             = firstSr$t_dur,
                    extrapCap         = firstSr$extrapCap,
                    extrapActive      = isTRUE(firstSr$extrapActive)
                ))
            }

            # Arrhenius Accelerated Stability Kinetics
            if (self$options$showArrh) {
                private$.runArrhenius(
                    governingRes = governingRes,
                    data = data,
                    timeName = timeName,
                    batchName = batchName,
                    tempVarName = self$options$tempVar,
                    arrhEa = self$options$arrhEa,
                    arrhTempAcc = self$options$arrhTempAcc,
                    arrhRhAcc = self$options$arrhRhAcc,
                    arrhTempLong = self$options$arrhTempLong,
                    arrhRhLong = self$options$arrhRhLong,
                    arrhB = self$options$arrhB,
                    showArrhSens = self$options$showArrhSens,
                    evalIsLong = evalIsLong
                )
            }

            # Select attribute for detailed single-attribute tables and plots
            detailOpt <- self$options$detailAttr
            selAttrName <- governingAttrName
            if (detailOpt == "dep" && depName %in% names(evaluatedAttrs)) {
                selAttrName <- depName
            } else if (detailOpt == "dep2" && length(self$options$dep2) > 0 && nzchar(self$options$dep2) && self$options$dep2 %in% names(evaluatedAttrs)) {
                selAttrName <- self$options$dep2
            } else if (detailOpt == "dep3" && length(self$options$dep3) > 0 && nzchar(self$options$dep3) && self$options$dep3 %in% names(evaluatedAttrs)) {
                selAttrName <- self$options$dep3
            } else if (detailOpt == "dep4" && length(self$options$dep4) > 0 && nzchar(self$options$dep4) && self$options$dep4 %in% names(evaluatedAttrs)) {
                selAttrName <- self$options$dep4
            } else if (detailOpt == "dep5" && length(self$options$dep5) > 0 && nzchar(self$options$dep5) && self$options$dep5 %in% names(evaluatedAttrs)) {
                selAttrName <- self$options$dep5
            }
            selRes <- evaluatedAttrs[[selAttrName]]

            # Populate Detailed ANCOVA Table
            private$.populatePoolTable(
                selRes$k, selRes$df_slopes, selRes$ss_slopes, selRes$ms_slopes, selRes$f_slopes, selRes$p_slopes,
                selRes$df_ints, selRes$ss_ints, selRes$ms_ints, selRes$f_ints, selRes$p_ints,
                selRes$df_full, selRes$rss_full, selRes$mse_full, self$options$alphaPool, selRes$selectedModelCode
            )
            if (selRes$k < 2) {
                self$results$poolTable$setNote(
                    "single_batch",
                    .("Only a single batch provided; ANCOVA poolability is not evaluated. Single linear regression is fitted.")
                )
            }
            if (isMulti) {
                self$results$poolTable$setNote(
                    "attr_focus_note",
                    paste0(.("Detailed results shown for attribute:"), " '", selRes$attrDef$name, "'")
                )
            }

            # Populate Detailed Shelf Life Table
            private$.populateShelfTable(selRes$shelfResults, selRes$adoptedModelName, selRes$dat)
            if (isMulti) {
                self$results$shelfTable$setNote(
                    "attr_focus_note",
                    paste0(.("Detailed results shown for attribute:"), " '", selRes$attrDef$name, "'")
                )
            }

            # Store precomputed plot states for single-attribute plots
            private$.preparePlotStates(
                dat = selRes$dat,
                batchLevels = selRes$batchLevels,
                selectedModelCode = selRes$selectedModelCode,
                fit_full = selRes$fit_full,
                fit_cs = selRes$fit_cs,
                fit_cl = selRes$fit_cl,
                chosenFit = selRes$chosenFit,
                mse_full = selRes$mse_full,
                df_full = selRes$df_full,
                mse_cs = selRes$mse_cs,
                df_cs = selRes$df_cs,
                mse_cl = selRes$mse_cl,
                df_cl = selRes$df_cl,
                shelfResults = selRes$shelfResults,
                specType = selRes$specType,
                lsl_val = selRes$lsl_val,
                usl_val = selRes$usl_val,
                lsl_trans = selRes$lsl_trans,
                usl_trans = selRes$usl_trans,
                trans = selRes$trans,
                depName = selRes$attrDef$name,
                timeName = timeName,
                batchName = batchName,
                alphaPool = self$options$alphaPool,
                confLevel = self$options$confLevel,
                p_slopes = selRes$p_slopes,
                f_slopes = selRes$f_slopes
            )

            # Populate Detailed Coefficients Table
            private$.populateCoefTable(selRes$chosenFit)

            # Populate Detailed Diagnostics Table
            private$.populateDiagTable(selRes$chosenFit, selRes$dat)
        },

        .evaluateSingleAttribute = function(
            attrDef, data, timeName, batchName, alphaPool, confLevel, modelChoice, accelCondition, studyDuration
        ) {
            attrName <- attrDef$name
            if (is.null(attrName) || !attrName %in% colnames(data)) return(NULL)

            cols <- c(attrName, timeName, batchName)
            dat  <- jmvcore::naOmit(data[, cols])
            if (nrow(dat) < 3) return(NULL)

            colnames(dat) <- c("Y", "Time", "Batch")
            dat$Y     <- as.numeric(dat$Y)
            dat$Time  <- as.numeric(dat$Time)
            dat$Batch <- as.factor(dat$Batch)

            if (any(!is.finite(dat$Y)) || any(!is.finite(dat$Time))) return(NULL)
            if (any(dat$Time < 0)) return(NULL)

            dat$Batch <- droplevels(dat$Batch)
            batchLevels <- levels(dat$Batch)
            k <- length(batchLevels)
            if (k < 1) return(NULL)

            t_per_batch <- tapply(dat$Time, dat$Batch, function(tt) length(unique(tt)))
            if (all(t_per_batch < 2)) return(NULL)

            trans <- attrDef$trans
            if (trans == "log") {
                if (any(dat$Y <= 0)) {
                    jmvcore::reject(paste0(.("Log transformation for attribute"), " '", attrName, "' ", .("requires strictly positive values of the response variable.")))
                    return(NULL)
                }
                dat$Y_trans <- log(dat$Y)
            } else {
                dat$Y_trans <- dat$Y
            }

            specType <- attrDef$specType
            lsl_val  <- attrDef$lsl
            usl_val  <- attrDef$usl

            if (trans == "log") {
                lsl_trans <- if (!is.null(lsl_val) && lsl_val > 0) log(lsl_val) else -Inf
                usl_trans <- if (!is.null(usl_val) && usl_val > 0) log(usl_val) else Inf
            } else {
                lsl_trans <- if (!is.null(lsl_val)) lsl_val else -Inf
                usl_trans <- if (!is.null(usl_val)) usl_val else Inf
            }

            if (k < 2) {
                fit_cl   <- stats::lm(Y_trans ~ Time, data = dat)
                fit_cs   <- fit_cl
                fit_full <- fit_cl

                rss_cl   <- sum(stats::residuals(fit_cl)^2)
                df_cl    <- stats::df.residual(fit_cl)
                mse_cl   <- if (df_cl > 0) rss_cl / df_cl else NA_real_

                rss_cs   <- rss_cl; df_cs <- df_cl; mse_cs <- mse_cl
                rss_full <- rss_cl; df_full <- df_cl; mse_full <- mse_cl

                ss_slopes <- 0; df_slopes <- 0; ms_slopes <- 0; f_slopes <- NA_real_; p_slopes <- 1.0
                ss_ints   <- 0; df_ints   <- 0; ms_ints   <- 0; f_ints   <- NA_real_; p_ints   <- 1.0
            } else {
                fit_full <- stats::lm(Y_trans ~ Batch * Time, data = dat)
                fit_cs   <- stats::lm(Y_trans ~ Batch + Time, data = dat)
                fit_cl   <- stats::lm(Y_trans ~ Time, data = dat)

                rss_full <- sum(stats::residuals(fit_full)^2)
                df_full  <- stats::df.residual(fit_full)
                mse_full <- if (df_full > 0) rss_full / df_full else NA_real_

                rss_cs   <- sum(stats::residuals(fit_cs)^2)
                df_cs    <- stats::df.residual(fit_cs)
                mse_cs   <- if (df_cs > 0) rss_cs / df_cs else NA_real_

                rss_cl   <- sum(stats::residuals(fit_cl)^2)
                df_cl    <- stats::df.residual(fit_cl)
                mse_cl   <- if (df_cl > 0) rss_cl / df_cl else NA_real_

                if (df_full > 0 && !is.na(mse_full) && mse_full > 0) {
                    an_slopes <- stats::anova(fit_cs, fit_full)
                    df_slopes <- an_slopes[2, "Df"]
                    ss_slopes <- max(0, an_slopes[2, "Sum of Sq"])
                    f_slopes  <- an_slopes[2, "F"]
                    p_slopes  <- an_slopes[2, "Pr(>F)"]
                    ms_slopes <- if (!is.na(df_slopes) && df_slopes > 0) ss_slopes / df_slopes else 0
                } else {
                    ss_slopes <- 0; df_slopes <- 0; ms_slopes <- 0; f_slopes <- NA_real_; p_slopes <- NA_real_
                }

                if (df_cs > 0 && !is.na(mse_cs) && mse_cs > 0) {
                    an_ints <- stats::anova(fit_cl, fit_cs)
                    df_ints <- an_ints[2, "Df"]
                    ss_ints <- max(0, an_ints[2, "Sum of Sq"])
                    f_ints  <- an_ints[2, "F"]
                    p_ints  <- an_ints[2, "Pr(>F)"]
                    ms_ints <- if (!is.na(df_ints) && df_ints > 0) ss_ints / df_ints else 0
                } else {
                    ss_ints <- 0; df_ints <- 0; ms_ints <- 0; f_ints <- NA_real_; p_ints <- NA_real_
                }
            }

            if (k < 2) {
                selectedModelCode <- 3
                adoptedModelName  <- .("Single batch linear regression")
            } else if (modelChoice == "sep_slopes_sep_int") {
                selectedModelCode <- 1
                adoptedModelName  <- .("Separate slopes & separate intercepts (Model 1)")
            } else if (modelChoice == "common_slope_sep_int") {
                selectedModelCode <- 2
                adoptedModelName  <- .("Common slope & separate intercepts (Model 2)")
            } else if (modelChoice == "common_slope_common_int") {
                selectedModelCode <- 3
                adoptedModelName  <- .("Common slope & common intercept (Model 3)")
            } else {
                if (is.na(p_slopes)) {
                    selectedModelCode <- 1
                    adoptedModelName  <- .("Separate slopes & separate intercepts (Model 1)")
                } else if (p_slopes <= alphaPool) {
                    selectedModelCode <- 1
                    adoptedModelName  <- .("Separate slopes & separate intercepts (Model 1)")
                } else if (!is.na(p_ints) && p_ints <= alphaPool) {
                    selectedModelCode <- 2
                    adoptedModelName  <- .("Common slope & separate intercepts (Model 2)")
                } else {
                    selectedModelCode <- 3
                    adoptedModelName  <- .("Common slope & common intercept (Model 3)")
                }
            }

            chosenFit <- if (selectedModelCode == 1) fit_full else if (selectedModelCode == 2) fit_cs else fit_cl

            shelfResults <- private$.computeShelfLife(
                dat = dat,
                batchLevels = batchLevels,
                selectedModelCode = selectedModelCode,
                fit_full = fit_full,
                fit_cs = fit_cs,
                fit_cl = fit_cl,
                mse_full = mse_full,
                df_full = df_full,
                specType = specType,
                lsl_trans = lsl_trans,
                usl_trans = usl_trans,
                confLevel = confLevel,
                trans = trans
            )

            return(list(
                attrDef = attrDef,
                dat = dat,
                batchLevels = batchLevels,
                k = k,
                selectedModelCode = selectedModelCode,
                adoptedModelName = adoptedModelName,
                fit_full = fit_full,
                fit_cs = fit_cs,
                fit_cl = fit_cl,
                chosenFit = chosenFit,
                rss_full = rss_full,
                df_full = df_full,
                mse_full = mse_full,
                rss_cs = rss_cs,
                df_cs = df_cs,
                mse_cs = mse_cs,
                rss_cl = rss_cl,
                df_cl = df_cl,
                mse_cl = mse_cl,
                df_slopes = df_slopes,
                ss_slopes = ss_slopes,
                ms_slopes = ms_slopes,
                f_slopes = f_slopes,
                p_slopes = p_slopes,
                df_ints = df_ints,
                ss_ints = ss_ints,
                ms_ints = ms_ints,
                f_ints = f_ints,
                p_ints = p_ints,
                shelfResults = shelfResults,
                specType = specType,
                lsl_val = lsl_val,
                usl_val = usl_val,
                lsl_trans = lsl_trans,
                usl_trans = usl_trans,
                trans = trans
            ))
        },

        .populatePoolTable = function(
            k, df_slopes, ss_slopes, ms_slopes, f_slopes, p_slopes,
            df_ints, ss_ints, ms_ints, f_ints, p_ints,
            df_full, rss_full, mse_full, alphaPool, selectedModelCode
        ) {
            table <- self$results$poolTable
            table$deleteRows()
            f_slopes_val <- if (is.nan(f_slopes) || is.infinite(f_slopes)) NA_real_ else f_slopes
            p_slopes_val <- if (is.nan(p_slopes)) NA_real_ else p_slopes
            f_ints_val   <- if (is.nan(f_ints) || is.infinite(f_ints)) NA_real_ else f_ints
            p_ints_val   <- if (is.nan(p_ints)) NA_real_ else p_ints

            table$addRow(rowKey = "step1", values = list(
                step = .("Step 1: Slopes (Batch * Time)"),
                df = df_slopes,
                ss = ss_slopes,
                ms = ms_slopes,
                f = f_slopes_val,
                p = p_slopes_val,
                decision = if (k < 2) {
                    .("Single batch – poolability test not applicable")
                } else if (is.na(p_slopes_val)) {
                    .("Insufficient degrees of freedom for slopes test")
                } else if (p_slopes_val <= alphaPool) {
                    paste0(.("Slopes differ"), " (p <= ", alphaPool, ") -> ", .("Adopt Model 1"))
                } else {
                    paste0(.("Equal slopes"), " (p > ", alphaPool, ") -> ", .("Proceed to Step 2"))
                }
            ))

            table$addRow(rowKey = "step2", values = list(
                step = .("Step 2: Intercepts (Batch)"),
                df = df_ints,
                ss = ss_ints,
                ms = ms_ints,
                f = if (!is.na(p_slopes_val) && p_slopes_val <= alphaPool) NA_real_ else f_ints_val,
                p = if (!is.na(p_slopes_val) && p_slopes_val <= alphaPool) NA_real_ else p_ints_val,
                decision = if (k < 2) {
                    .("Single batch – poolability test not applicable")
                } else if (!is.na(p_slopes_val) && p_slopes_val <= alphaPool) {
                    .("Not tested (slopes are unequal)")
                } else if (is.na(p_ints_val)) {
                    .("Insufficient degrees of freedom for intercepts test")
                } else if (p_ints_val <= alphaPool) {
                    paste0(.("Intercepts differ"), " (p <= ", alphaPool, ") -> ", .("Adopt Model 2"))
                } else {
                    paste0(.("Equal intercepts"), " (p > ", alphaPool, ") -> ", .("Adopt Model 3 (Common line)"))
                }
            ))

            table$addRow(rowKey = "resid", values = list(
                step = .("Residual Error (Full Model)"),
                df = df_full,
                ss = rss_full,
                ms = mse_full,
                f = NA_real_,
                p = NA_real_,
                decision = .("Pooled residual variance across batches")
            ))

            # Dynamic adopted model note
            model_summary <- if (k < 2) {
                .("<b>Adopted Structure</b>: Single batch linear regression. No poolability testing.")
            } else if (selectedModelCode == 1) {
                .("<b>Adopted Structure</b>: Model 1 (Separate slopes and separate intercepts). Batches degrade at different rates.")
            } else if (selectedModelCode == 2) {
                .("<b>Adopted Structure</b>: Model 2 (Common slope, separate intercepts). Batches degrade at a parallel rate but start at different initial levels.")
            } else {
                .("<b>Adopted Structure</b>: Model 3 (Common slope, common intercept). All batches are fully pooled into a single regression line.")
            }
            table$setNote("adopted_model", model_summary)
        },

        .computeShelfLife = function(
            dat, batchLevels, selectedModelCode, fit_full, fit_cs, fit_cl,
            mse_full, df_full, specType, lsl_trans, usl_trans, confLevel, trans
        ) {
            # Critical t value for 95% bound
            alpha <- 1 - confLevel
            t_max_obs <- max(dat$Time, na.rm = TRUE)

            calcBound = function(batch_i, t_eval, modelCode, boundType) {
                if (modelCode == 1) {
                    # Model 1: Separate slopes and separate intercepts with pooled MSE
                    sub_dat <- dat[dat$Batch == batch_i, , drop = FALSE]
                    n_i <- nrow(sub_dat)
                    t_bar_i <- mean(sub_dat$Time)
                    S_tt_i <- sum((sub_dat$Time - t_bar_i)^2)

                    if (is.na(mse_full) || mse_full <= 0 || df_full <= 0 || S_tt_i <= 0) {
                        return(rep(NA_real_, length(t_eval)))
                    }

                    fit_i <- stats::lm(Y_trans ~ Time, data = sub_dat)
                    coef_i <- stats::coef(fit_i)
                    y_hat <- coef_i[1] + coef_i[2] * t_eval
                    se_pred <- sqrt(mse_full * (1 / n_i + (t_eval - t_bar_i)^2 / S_tt_i))
                    df_val <- df_full
                } else if (modelCode == 2) {
                    # Model 2: Common slope, separate intercepts
                    new_df <- data.frame(Batch = factor(batch_i, levels = batchLevels), Time = t_eval)
                    pred_res <- stats::predict(fit_cs, newdata = new_df, se.fit = TRUE)
                    y_hat <- pred_res$fit
                    se_pred <- pred_res$se.fit
                    df_val <- stats::df.residual(fit_cs)
                } else {
                    # Model 3: Common slope, common intercept
                    new_df <- data.frame(Time = t_eval)
                    pred_res <- stats::predict(fit_cl, newdata = new_df, se.fit = TRUE)
                    y_hat <- pred_res$fit
                    se_pred <- pred_res$se.fit
                    df_val <- stats::df.residual(fit_cl)
                }

                # Critical value
                # Two-sided 95% bound requires alpha / 2 on each tail
                # One-sided 95% bound requires alpha on that tail
                is_two_sided <- (specType == "both")
                t_crit <- if (is_two_sided) stats::qt(1 - alpha / 2, df = df_val) else stats::qt(1 - alpha, df = df_val)

                if (boundType == "lower") {
                    return(y_hat - t_crit * se_pred)
                } else if (boundType == "upper") {
                    return(y_hat + t_crit * se_pred)
                } else {
                    return(list(
                        lower = y_hat - t_crit * se_pred,
                        upper = y_hat + t_crit * se_pred
                    ))
                }
            }

            findCrossing <- function(batch_i, modelCode) {
                cross_lower <- NA_real_
                cross_upper <- NA_real_

                if (specType %in% c("lower", "both")) {
                    b_0   <- calcBound(batch_i, 0, modelCode, "lower")
                    b_far <- calcBound(batch_i, 240, modelCode, "lower")
                    if (is.na(b_0) || is.na(b_far)) {
                        cross_lower <- NA_real_
                    } else if (b_0 < lsl_trans) {
                        cross_lower <- 0.0 # Out of specification at time zero
                    } else if (b_far <= lsl_trans) {
                        tryCatch({
                            f_root_lower <- function(t) calcBound(batch_i, t, modelCode, "lower") - lsl_trans
                            r_res <- stats::uniroot(f_root_lower, interval = c(0, 240), tol = 1e-4)
                            cross_lower <- r_res$root
                        }, error = function(e) {
                            cross_lower <- NA_real_
                        })
                    } else {
                        cross_lower <- Inf # No crossing within 240 months
                    }
                }

                if (specType %in% c("upper", "both")) {
                    b_0   <- calcBound(batch_i, 0, modelCode, "upper")
                    b_far <- calcBound(batch_i, 240, modelCode, "upper")
                    if (is.na(b_0) || is.na(b_far)) {
                        cross_upper <- NA_real_
                    } else if (b_0 > usl_trans) {
                        cross_upper <- 0.0
                    } else if (b_far >= usl_trans) {
                        tryCatch({
                            f_root_upper <- function(t) calcBound(batch_i, t, modelCode, "upper") - usl_trans
                            r_res <- stats::uniroot(f_root_upper, interval = c(0, 240), tol = 1e-4)
                            cross_upper <- r_res$root
                        }, error = function(e) {
                            cross_upper <- NA_real_
                        })
                    } else {
                        cross_upper <- Inf
                    }
                }

                if (specType == "lower") {
                    return(cross_lower)
                } else if (specType == "upper") {
                    return(cross_upper)
                } else {
                    if (is.na(cross_lower) || is.na(cross_upper)) return(NA_real_)
                    return(min(cross_lower, cross_upper))
                }
            }

            # Calculate for each batch
            batchRows <- list()
            for (b in batchLevels) {
                sub_dat <- dat[dat$Batch == b, , drop = FALSE]
                if (selectedModelCode == 1) {
                    if (length(unique(sub_dat$Time)) >= 2) {
                        fit_b <- stats::lm(Y_trans ~ Time, data = sub_dat)
                        cf <- stats::coef(fit_b)
                        int_val <- if (trans == "log") exp(cf[1]) else cf[1]
                        slp_val <- cf[2]
                    } else {
                        int_val <- if (trans == "log") exp(mean(sub_dat$Y_trans)) else mean(sub_dat$Y_trans)
                        slp_val <- NA_real_
                    }
                } else if (selectedModelCode == 2) {
                    cf_cs <- stats::coef(fit_cs)
                    batch_term <- paste0("Batch", b)
                    offset <- if (batch_term %in% names(cf_cs)) cf_cs[[batch_term]] else 0
                    int_raw <- cf_cs[1] + offset
                    int_val <- if (trans == "log") exp(int_raw) else int_raw
                    slp_val <- cf_cs["Time"]
                } else {
                    cf_cl <- stats::coef(fit_cl)
                    int_val <- if (trans == "log") exp(cf_cl[1]) else cf_cl[1]
                    slp_val <- cf_cl["Time"]
                }

                stat_shelf <- findCrossing(b, selectedModelCode)
                batchRows[[b]] <- list(
                    batch = b,
                    intercept = int_val,
                    slope = slp_val,
                    statShelf = stat_shelf
                )
            }

            # Extrapolation rule (ICH Q1E Appendix A)
            studyDur <- self$options$studyDuration
            t_dur <- if (!is.null(studyDur) && studyDur > 0) studyDur else t_max_obs
            accel <- self$options$accelCondition

            if (accel == "no_change") {
                extrapCap <- min(2 * t_dur, t_dur + 12)
            } else if (accel == "intermediate_ok") {
                extrapCap <- min(1.5 * t_dur, t_dur + 6)
            } else {
                extrapCap <- t_dur
            }

            # Identify limiting batch and overall shelf life
            all_stat_shelves <- sapply(batchRows, function(x) x$statShelf)
            valid_shelves <- all_stat_shelves[!is.na(all_stat_shelves)]

            if (length(valid_shelves) == 0) {
                overall_stat_shelf <- NA_real_
                overall_ich_shelf  <- NA_real_
                limiting_batch_name <- NA_character_
            } else {
                overall_stat_shelf <- min(valid_shelves)
                min_batch_idx <- which(all_stat_shelves == overall_stat_shelf)
                limiting_batch_name <- names(batchRows)[min_batch_idx[1]]
                overall_ich_shelf  <- if (is.infinite(overall_stat_shelf)) extrapCap else min(overall_stat_shelf, extrapCap)
            }

            return(list(
                batchRows = batchRows,
                overallStatShelf = overall_stat_shelf,
                overallIchShelf = overall_ich_shelf,
                limitingBatch = limiting_batch_name,
                extrapCap = extrapCap,
                extrapActive = isTRUE(!is.na(overall_stat_shelf) && overall_stat_shelf > extrapCap),
                t_dur = t_dur
            ))
        },

        .populateShelfTable = function(shelfRes, adoptedModelName, dat) {
            table <- self$results$shelfTable
            table$deleteRows()
            bRows <- shelfRes$batchRows
            limitingBatch <- shelfRes$limitingBatch
            extrapCap <- shelfRes$extrapCap

            for (b in names(bRows)) {
                item <- bRows[[b]]
                s_stat <- item$statShelf

                if (is.na(s_stat)) {
                    stat_display <- NA_real_
                    ich_display  <- NA_real_
                    lim_text     <- .("Undetermined")
                    table$setNote(
                        paste0("na_batch_", b),
                        jmvcore::format(
                            .("Shelf life for batch '{batch}' cannot be computed (insufficient time points or degrees of freedom)."),
                            batch = b
                        )
                    )
                } else {
                    stat_display <- if (is.infinite(s_stat)) 999.0 else s_stat
                    s_ich        <- if (is.infinite(s_stat)) extrapCap else min(s_stat, extrapCap)
                    ich_display  <- if (is.infinite(s_ich)) extrapCap else s_ich
                    is_limit     <- (!is.na(limitingBatch) && b == limitingBatch && length(bRows) > 1)
                    lim_text     <- if (is_limit) .("Limiting batch") else .("Conforming")
                }

                table$addRow(rowKey = b, values = list(
                    batch = as.character(b),
                    model = adoptedModelName,
                    intercept = item$intercept,
                    slope = item$slope,
                    statShelf = stat_display,
                    ichShelf = ich_display,
                    limiting = lim_text
                ))
            }

            # Overall row
            ov_stat <- shelfRes$overallStatShelf
            ov_ich  <- shelfRes$overallIchShelf

            if (is.na(ov_stat)) {
                stat_disp <- NA_real_
                ich_disp  <- NA_real_
                lim_reason <- .("Undetermined")
            } else {
                stat_disp <- if (is.infinite(ov_stat)) 999.0 else ov_stat
                ich_disp  <- if (is.infinite(ov_ich)) extrapCap else ov_ich
                lim_batch_val <- if (!is.na(limitingBatch)) as.character(limitingBatch) else ""

                lim_reason <- if (ov_stat > extrapCap) {
                    paste0(.("Capped by ICH Q1E extrapolation limit"), " (", private$.formatNum(extrapCap, 1), " ", .("months"), ")")
                } else if (length(bRows) > 1) {
                    paste0(.("Shortest batch"), ": ", lim_batch_val)
                } else {
                    .("Statistical intersection")
                }
            }

            table$addRow(rowKey = "overall", values = list(
                batch = paste0("<b>", .("OVERALL / PROPOSED"), "</b>"),
                model = adoptedModelName,
                intercept = NA_real_,
                slope = NA_real_,
                statShelf = stat_disp,
                ichShelf = ich_disp,
                limiting = lim_reason
            ))

            # Dynamic notes for shelfTable
            if (!is.na(ov_stat) && ov_stat > extrapCap) {
                table$setNote("extrap_active", jmvcore::format(.("<b>Extrapolation Cap Active</b>: Statistical intersection ({stat} months) exceeds the allowable ICH Q1E extrapolation limit ({cap} months). Permitted shelf life is capped at {cap} months."), stat = private$.formatNum(ov_stat, 1), cap = private$.formatNum(extrapCap, 1)))
            } else {
                table$setNote("extrap_active", NULL)
            }

            if (length(bRows) > 1 && !is.na(limitingBatch) && !is.na(shelfRes$batchRows[[limitingBatch]]$statShelf)) {
                table$setNote("limiting_batch_note", jmvcore::format(.("<b>Limiting Batch</b>: Overall shelf life is determined by {batch} ({months} months), which represents the earliest intersection among all batches."), batch = limitingBatch, months = private$.formatNum(shelfRes$batchRows[[limitingBatch]]$statShelf, 1)))
            } else {
                table$setNote("limiting_batch_note", NULL)
            }
        },

        .preparePlotStates = function(
            dat, batchLevels, selectedModelCode, fit_full, fit_cs, fit_cl, chosenFit,
            mse_full, df_full, mse_cs, df_cs, mse_cl, df_cl,
            shelfResults, specType, lsl_val, usl_val, lsl_trans, usl_trans, trans,
            depName, timeName, batchName, alphaPool, confLevel, p_slopes, f_slopes
        ) {
            ov_ich   <- shelfResults$overallIchShelf
            ov_stat  <- shelfResults$overallStatShelf
            t_dur    <- shelfResults$t_dur
            extrapCap <- shelfResults$extrapCap
            extrapActive <- shelfResults$extrapActive
            b_lim    <- shelfResults$limitingBatch
            lim_stat <- if (!is.na(b_lim)) shelfResults$batchRows[[b_lim]]$statShelf else NA_real_
            k        <- length(batchLevels)

            t_max_plot <- max(t_dur * 1.25, if (!is.na(ov_ich) && !is.infinite(ov_ich)) ov_ich * 1.25 else 36, 12)
            t_grid <- seq(0, t_max_plot, length.out = 150)
            alpha  <- 1 - confLevel
            is_two_sided <- (specType == "both")

            # Precalculate predictions and bounds for stabPlot and facetPlot
            pred_list <- list()
            for (b in batchLevels) {
                sub_d <- dat[dat$Batch == b, , drop = FALSE]
                n_i   <- nrow(sub_d)
                t_bar_i <- mean(sub_d$Time)
                S_tt_i  <- sum((sub_d$Time - t_bar_i)^2)

                if (selectedModelCode == 1) {
                    if (S_tt_i > 0) {
                        fit_b <- stats::lm(Y_trans ~ Time, data = sub_d)
                        cf_b  <- stats::coef(fit_b)
                        y_mean <- cf_b[1] + cf_b[2] * t_grid
                    } else {
                        y_mean <- rep(mean(sub_d$Y_trans), length(t_grid))
                    }
                    if (!is.na(mse_full) && mse_full > 0 && df_full > 0 && S_tt_i > 0) {
                        se_mean <- sqrt(mse_full * (1 / n_i + (t_grid - t_bar_i)^2 / S_tt_i))
                    } else {
                        se_mean <- rep(NA_real_, length(t_grid))
                    }
                    df_b_val <- df_full
                } else if (selectedModelCode == 2) {
                    new_df <- data.frame(Batch = factor(b, levels = batchLevels), Time = t_grid)
                    pred_res <- stats::predict(fit_cs, newdata = new_df, se.fit = TRUE)
                    y_mean <- pred_res$fit
                    se_mean <- pred_res$se.fit
                    df_b_val <- df_cs
                } else {
                    new_df <- data.frame(Time = t_grid)
                    pred_res <- stats::predict(fit_cl, newdata = new_df, se.fit = TRUE)
                    y_mean <- pred_res$fit
                    se_mean <- pred_res$se.fit
                    df_b_val <- df_cl
                }

                tcrit_plot <- if (is_two_sided) stats::qt(1 - alpha / 2, df = df_b_val) else stats::qt(1 - alpha, df = df_b_val)

                if (specType == "lower") {
                    b_lower <- y_mean - tcrit_plot * se_mean
                    b_upper <- y_mean
                } else if (specType == "upper") {
                    b_lower <- y_mean
                    b_upper <- y_mean + tcrit_plot * se_mean
                } else {
                    b_lower <- y_mean - tcrit_plot * se_mean
                    b_upper <- y_mean + tcrit_plot * se_mean
                }

                if (trans == "log") {
                    y_disp <- exp(y_mean)
                    l_disp <- exp(b_lower)
                    u_disp <- exp(b_upper)
                } else {
                    y_disp <- y_mean
                    l_disp <- b_lower
                    u_disp <- b_upper
                }

                pred_list[[b]] <- data.frame(
                    Time = t_grid,
                    Batch = factor(b, levels = batchLevels),
                    Fit = y_disp,
                    Lower = l_disp,
                    Upper = u_disp
                )
            }
            pred_df <- do.call(rbind, pred_list)
            obs_df  <- data.frame(Time = dat$Time, Y = dat$Y, Batch = dat$Batch)

            # 1. stabPlotState
            stabPlotState <- list(
                obs_df = obs_df,
                pred_df = pred_df,
                specType = specType,
                lsl = lsl_val,
                usl = usl_val,
                ov_ich = ov_ich,
                ov_stat = ov_stat,
                limitingBatch = b_lim,
                limiting_stat_shelf = lim_stat,
                extrapCap = extrapCap,
                extrapActive = extrapActive,
                modelCode = selectedModelCode,
                timeName = timeName,
                depName = depName,
                batchName = batchName,
                batchLevels = batchLevels
            )
            self$results$stabPlot$setState(stabPlotState)

            # 2. shelfPlotState
            shelf_batch_list <- list()
            for (b in names(shelfResults$batchRows)) {
                it <- shelfResults$batchRows[[b]]
                s_stat_val <- if (is.na(it$statShelf)) NA_real_ else if (is.infinite(it$statShelf)) 999.0 else it$statShelf
                s_ich_val  <- if (is.na(it$statShelf)) NA_real_ else if (is.infinite(it$statShelf)) extrapCap else min(it$statShelf, extrapCap)
                is_lim_b   <- (!is.na(b_lim) && b == b_lim && length(shelfResults$batchRows) > 1)

                shelf_batch_list[[b]] <- data.frame(
                    Batch = b,
                    StatShelf = s_stat_val,
                    IchShelf  = s_ich_val,
                    Limiting  = is_lim_b,
                    stringsAsFactors = FALSE
                )
            }
            shelf_batch_df <- do.call(rbind, shelf_batch_list)

            shelfPlotState <- list(
                shelf_batch_df = shelf_batch_df,
                ov_stat = ov_stat,
                ov_ich = ov_ich,
                limitingBatch = b_lim,
                extrapCap = extrapCap,
                extrapActive = extrapActive,
                t_dur = t_dur,
                batchLevels = batchLevels,
                batchName = batchName,
                timeName = timeName
            )
            self$results$shelfPlot$setState(shelfPlotState)

            # 3. facetPlotState
            facetPlotState <- list(
                obs_df = obs_df,
                pred_df = pred_df,
                specType = specType,
                lsl = lsl_val,
                usl = usl_val,
                batchLevels = batchLevels,
                depName = depName,
                timeName = timeName,
                batchName = batchName
            )
            self$results$facetPlot$setState(facetPlotState)

            # 4. ratePlotState
            batch_rate_list <- list()
            for (b in batchLevels) {
                sub_d <- dat[dat$Batch == b, , drop = FALSE]
                if (length(unique(sub_d$Time)) >= 2) {
                    fit_b <- stats::lm(Y_trans ~ Time, data = sub_d)
                    sm_b  <- summary(fit_b)$coefficients
                    if ("Time" %in% rownames(sm_b)) {
                        b_slope <- sm_b["Time", "Estimate"]
                        b_se    <- sm_b["Time", "Std. Error"]
                        df_b    <- fit_b$df.residual
                        tcrit_b <- if (df_b > 0) stats::qt(0.975, df = df_b) else 1.96
                        b_low   <- b_slope - tcrit_b * b_se
                        b_upp   <- b_slope + tcrit_b * b_se
                    } else {
                        b_slope <- NA_real_; b_low <- NA_real_; b_upp <- NA_real_
                    }
                } else {
                    b_slope <- NA_real_; b_low <- NA_real_; b_upp <- NA_real_
                }
                batch_rate_list[[b]] <- data.frame(
                    Batch = b,
                    Slope = b_slope,
                    Lower = b_low,
                    Upper = b_upp,
                    Type = .("Individual Batch"),
                    stringsAsFactors = FALSE
                )
            }
            df_rates <- do.call(rbind, batch_rate_list)

            if (k >= 2 && "Time" %in% names(stats::coef(fit_cs))) {
                sm_cs    <- summary(fit_cs)$coefficients
                cs_slope <- sm_cs["Time", "Estimate"]
                cs_se    <- sm_cs["Time", "Std. Error"]
                df_cs_r  <- fit_cs$df.residual
                tcrit_cs <- if (df_cs_r > 0) stats::qt(0.975, df = df_cs_r) else 1.96
                cs_low   <- cs_slope - tcrit_cs * cs_se
                cs_upp   <- cs_slope + tcrit_cs * cs_se
            } else if (k == 1 && "Time" %in% names(stats::coef(fit_cl))) {
                sm_cl    <- summary(fit_cl)$coefficients
                cs_slope <- sm_cl["Time", "Estimate"]
                cs_se    <- sm_cl["Time", "Std. Error"]
                df_cl_r  <- fit_cl$df.residual
                tcrit_cs <- if (df_cl_r > 0) stats::qt(0.975, df = df_cl_r) else 1.96
                cs_low   <- cs_slope - tcrit_cs * cs_se
                cs_upp   <- cs_slope + tcrit_cs * cs_se
            } else {
                cs_slope <- NA_real_; cs_low <- NA_real_; cs_upp <- NA_real_
            }

            ratePlotState <- list(
                df_rates = df_rates,
                cs_slope = cs_slope,
                cs_low = cs_low,
                cs_upp = cs_upp,
                p_slopes = p_slopes,
                f_slopes = f_slopes,
                alphaPool = alphaPool,
                k = k,
                depName = depName,
                timeName = timeName,
                batchName = batchName,
                batchLevels = batchLevels
            )
            self$results$ratePlot$setState(ratePlotState)

            # 5. predPlotState (Prediction intervals and SRL)
            b_target <- if (!is.na(b_lim)) b_lim else batchLevels[1]
            t_star   <- if (!is.na(ov_ich) && !is.infinite(ov_ich) && ov_ich > 0) ov_ich else t_dur

            if (selectedModelCode == 1) {
                sub_lim <- dat[dat$Batch == b_target, , drop = FALSE]
                fit_lim <- stats::lm(Y_trans ~ Time, data = sub_lim)
                pred_lim <- stats::predict(fit_lim, newdata = data.frame(Time = t_grid), se.fit = TRUE)
                slope_lim <- stats::coef(fit_lim)["Time"]
                pred_star <- stats::predict(fit_lim, newdata = data.frame(Time = t_star), se.fit = TRUE)
                mse_pi  <- mse_full
                df_pi   <- df_full
            } else if (selectedModelCode == 2) {
                new_grid <- data.frame(Batch = factor(b_target, levels = batchLevels), Time = t_grid)
                pred_lim <- stats::predict(fit_cs, newdata = new_grid, se.fit = TRUE)
                slope_lim <- stats::coef(fit_cs)["Time"]
                pred_star <- stats::predict(fit_cs, newdata = data.frame(Batch = factor(b_target, levels = batchLevels), Time = t_star), se.fit = TRUE)
                mse_pi  <- mse_cs
                df_pi   <- df_cs
            } else {
                pred_lim <- stats::predict(fit_cl, newdata = data.frame(Time = t_grid), se.fit = TRUE)
                slope_lim <- stats::coef(fit_cl)["Time"]
                pred_star <- stats::predict(fit_cl, newdata = data.frame(Time = t_star), se.fit = TRUE)
                mse_pi  <- mse_cl
                df_pi   <- df_cl
            }

            tcrit_pi <- if (is_two_sided) {
                if (df_pi > 0) stats::qt(1 - (1 - confLevel) / 2, df = df_pi) else 1.96
            } else {
                if (df_pi > 0) stats::qt(confLevel, df = df_pi) else 1.645
            }

            y_fit_pi <- pred_lim$fit
            se_mean_pi <- pred_lim$se.fit
            se_pred_pi <- if (!is.na(mse_pi) && mse_pi > 0) sqrt(mse_pi + se_mean_pi^2) else se_mean_pi

            ci_low_pi <- y_fit_pi - tcrit_pi * se_mean_pi
            ci_upp_pi <- y_fit_pi + tcrit_pi * se_mean_pi
            pi_low_pi <- y_fit_pi - tcrit_pi * se_pred_pi
            pi_upp_pi <- y_fit_pi + tcrit_pi * se_pred_pi

            if (trans == "log") {
                y_fit_pi  <- exp(y_fit_pi)
                ci_low_pi <- exp(ci_low_pi)
                ci_upp_pi <- exp(ci_upp_pi)
                pi_low_pi <- exp(pi_low_pi)
                pi_upp_pi <- exp(pi_upp_pi)
            }

            df_env <- data.frame(
                Time = t_grid,
                Fit = y_fit_pi,
                CI_Low = ci_low_pi,
                CI_Upp = ci_upp_pi,
                PI_Low = pi_low_pi,
                PI_Upp = pi_upp_pi
            )

            # SRL calculation on transformed scale
            se_star_pi <- if (!is.na(mse_pi) && mse_pi > 0) sqrt(mse_pi + pred_star$se.fit^2) else pred_star$se.fit
            margin_srl <- tcrit_pi * se_star_pi
            srl_lower <- NULL
            srl_upper <- NULL

            if (specType %in% c("lower", "both") && !is.null(lsl_val) && is.finite(lsl_trans)) {
                srl_l_t <- lsl_trans - slope_lim * t_star + margin_srl
                srl_lower <- if (trans == "log") exp(srl_l_t) else srl_l_t
            }
            if (specType %in% c("upper", "both") && !is.null(usl_val) && is.finite(usl_trans)) {
                srl_u_t <- usl_trans - slope_lim * t_star - margin_srl
                srl_upper <- if (trans == "log") exp(srl_u_t) else srl_u_t
            }

            predPlotState <- list(
                obs_df = obs_df,
                df_env = df_env,
                srl_lower = srl_lower,
                srl_upper = srl_upper,
                specType = specType,
                lsl = lsl_val,
                usl = usl_val,
                ov_ich = ov_ich,
                t_star = t_star,
                b_lim = b_target,
                depName = depName,
                timeName = timeName,
                batchName = batchName
            )
            self$results$predPlot$setState(predPlotState)

            # 6. diagPlotState
            diag_fitted <- stats::fitted(chosenFit)
            diag_resid  <- stats::residuals(chosenFit)
            diag_rstandard <- tryCatch(stats::rstandard(chosenFit), error = function(e) diag_resid / stats::sd(diag_resid))
            df_diag <- data.frame(
                Fitted = diag_fitted,
                Residuals = diag_resid,
                StdResiduals = diag_rstandard,
                SqrtAbsStdRes = sqrt(abs(diag_rstandard)),
                Time = dat$Time,
                Batch = dat$Batch
            )

            diagPlotState <- list(
                df_diag = df_diag,
                timeName = timeName,
                depName = depName,
                batchName = batchName
            )
            self$results$diagPlot$setState(diagPlotState)
        },
        .populateCoefTable = function(fit) {
            table <- self$results$coefTable
            table$deleteRows()
            sm <- summary(fit)
            cf <- sm$coefficients
            ci <- tryCatch(stats::confint(fit), error = function(e) matrix(NA_real_, nrow = nrow(cf), ncol = 2))

            terms <- rownames(cf)
            for (i in seq_along(terms)) {
                t_name <- terms[i]
                clean_name <- gsub("Batch(?=(batch|lot))", "", t_name, ignore.case = TRUE, perl = TRUE)
                clean_name <- gsub("Batch([0-9]+)", "Batch \\1", clean_name)
                
                est_val <- if (is.nan(cf[i, 1])) NA_real_ else cf[i, 1]
                se_val  <- if (is.nan(cf[i, 2])) NA_real_ else cf[i, 2]
                t_val   <- if (is.nan(cf[i, 3])) NA_real_ else cf[i, 3]
                p_val   <- if (is.nan(cf[i, 4])) NA_real_ else cf[i, 4]
                ci_low  <- if (!is.null(ci) && nrow(ci) >= i && is.nan(ci[i, 1])) NA_real_ else if (!is.null(ci) && nrow(ci) >= i) ci[i, 1] else NA_real_
                ci_upp  <- if (!is.null(ci) && nrow(ci) >= i && is.nan(ci[i, 2])) NA_real_ else if (!is.null(ci) && nrow(ci) >= i) ci[i, 2] else NA_real_

                table$addRow(rowKey = t_name, values = list(
                    term = clean_name,
                    est = est_val,
                    se = se_val,
                    t = t_val,
                    p = p_val,
                    lower = ci_low,
                    upper = ci_upp
                ))
            }
        },

        .populateDiagTable = function(fit, dat) {
            table <- self$results$diagTable
            table$deleteRows()
            sm <- summary(fit)
            res <- stats::residuals(fit)

            # R2 and Adj R2
            r2 <- if (is.nan(sm$r.squared)) NA_real_ else sm$r.squared
            adj_r2 <- if (is.nan(sm$adj.r.squared)) NA_real_ else sm$adj.r.squared
            sigma_val <- if (is.nan(sm$sigma)) NA_real_ else sm$sigma
            df_res <- sm$df[2]

            # F statistic for R2
            f_stat <- sm$fstatistic
            r2_df <- if (!is.null(f_stat) && length(f_stat) >= 1 && !is.na(f_stat[1]) && !is.nan(f_stat[1])) {
                paste0("F = ", private$.formatNum(f_stat[1], 2))
            } else {
                ""
            }
            r2_p <- if (!is.null(f_stat) && length(f_stat) == 3 && !is.na(f_stat[1]) && !is.nan(f_stat[1]) && f_stat[1] >= 0 && f_stat[2] > 0 && f_stat[3] > 0) {
                p_calc <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
                if (is.nan(p_calc)) NA_real_ else p_calc
            } else {
                NA_real_
            }

            # Shapiro-Wilk test for normality
            sw_val <- NA_real_
            sw_p   <- NA_real_
            if (length(res) >= 3 && length(res) <= 5000 && stats::sd(res) > 0) {
                tryCatch({
                    sw <- stats::shapiro.test(res)
                    sw_val <- if (is.nan(sw$statistic)) NA_real_ else sw$statistic
                    sw_p   <- if (is.nan(sw$p.value)) NA_real_ else sw$p.value
                }, error = function(e) {})
            }

            # Breusch-Pagan / Homoscedasticity check (rank correlation between |e| and Time)
            abs_res <- abs(res)
            cor_val <- NA_real_
            cor_p   <- NA_real_
            if (length(res) >= 3 && stats::sd(abs_res) > 0 && stats::sd(dat$Time) > 0) {
                tryCatch({
                    cor_res <- stats::cor.test(abs_res, dat$Time, method = "spearman", exact = FALSE)
                    cor_val <- if (is.nan(cor_res$estimate)) NA_real_ else cor_res$estimate
                    cor_p   <- if (is.nan(cor_res$p.value)) NA_real_ else cor_res$p.value
                }, error = function(e) {})
            }

            # Durbin-Watson autocorrelation approximation
            diff_res <- diff(res)
            ss_res   <- sum(res^2)
            dw_stat  <- if (ss_res > 0) sum(diff_res^2) / ss_res else NA_real_
            if (is.nan(dw_stat)) dw_stat <- NA_real_

            table$addRow(rowKey = "r2", values = list(
                metric = .("Coefficient of Determination (R²)"),
                value = r2,
                df = r2_df,
                p = r2_p,
                note = .("Model explained variance")
            ))

            table$addRow(rowKey = "adj_r2", values = list(
                metric = .("Adjusted R²"),
                value = adj_r2,
                df = "",
                p = NA_real_,
                note = .("Penalized for parameter count")
            ))

            table$addRow(rowKey = "sigma", values = list(
                metric = .("Residual Standard Error (s)"),
                value = sigma_val,
                df = paste0("df = ", df_res),
                p = NA_real_,
                note = .("Residual variability estimate")
            ))

            table$addRow(rowKey = "sw", values = list(
                metric = .("Normality of Residuals (Shapiro-Wilk)"),
                value = sw_val,
                df = paste0("W, N = ", length(res)),
                p = sw_p,
                note = if (!is.na(sw_p) && sw_p >= 0.05) {
                    .("Normality assumption met (p >= 0.05)")
                } else if (!is.na(sw_p)) {
                    .("Possible non-normality (p < 0.05)")
                } else {
                    .("Cannot evaluate normality (insufficient variance)")
                }
            ))

            table$addRow(rowKey = "cor", values = list(
                metric = .("Homoscedasticity (|e| vs Time correlation)"),
                value = cor_val,
                df = "rho",
                p = cor_p,
                note = if (!is.na(cor_p) && cor_p >= 0.05) {
                    .("Constant variance met (p >= 0.05)")
                } else if (!is.na(cor_p)) {
                    .("Variance may change over time")
                } else {
                    .("Cannot evaluate homoscedasticity")
                }
            ))

            table$addRow(rowKey = "dw", values = list(
                metric = .("Autocorrelation (Durbin-Watson d)"),
                value = dw_stat,
                df = "d ~ 2",
                p = NA_real_,
                note = if (!is.na(dw_stat) && dw_stat >= 1.5 && dw_stat <= 2.5) {
                    .("No significant autocorrelation")
                } else if (!is.na(dw_stat)) {
                    .("Possible residual autocorrelation")
                } else {
                    .("Undefined (zero residual variance)")
                }
            ))
        },

        .stabPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            pred_df <- state$pred_df
            obs_df  <- state$obs_df
            specType <- state$specType
            lsl <- state$lsl
            usl <- state$usl
            ov_ich <- state$ov_ich
            modelCode <- state$modelCode
            timeName <- state$timeName
            depName <- state$depName
            batchName <- state$batchName

            p <- ggplot2::ggplot(obs_df, ggplot2::aes(x = Time, y = Y, color = Batch)) +
                ggplot2::geom_point(size = 2.5, alpha = 0.85) +
                ggplot2::geom_line(data = pred_df, ggplot2::aes(x = Time, y = Fit, color = Batch), linewidth = 1.0) +
                ggplot2::labs(
                    title = paste0(.("Stability Profile & Shelf Life Estimation"), " (ICH Q1E)"),
                    subtitle = paste0(
                        .("Adopted:"), " ",
                        if (modelCode == 1) .("Separate slopes & intercepts") else if (modelCode == 2) .("Common slope & separate intercepts") else .("Common slope & intercept (Pooled)"),
                        if (!is.na(ov_ich)) paste0("  |  ", .("Permitted Shelf Life:"), " ", private$.formatNum(ov_ich, 1), " ", .("months")) else ""
                    ),
                    x = timeName,
                    y = depName,
                    color = batchName,
                    fill = batchName
                )

            # Confidence bands
            if (specType == "lower") {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df, ggplot2::aes(x = Time, ymin = Lower, ymax = Fit, fill = Batch),
                    alpha = 0.15, linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE
                )
            } else if (specType == "upper") {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df, ggplot2::aes(x = Time, ymin = Fit, ymax = Upper, fill = Batch),
                    alpha = 0.15, linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE
                )
            } else {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df, ggplot2::aes(x = Time, ymin = Lower, ymax = Upper, fill = Batch),
                    alpha = 0.15, linetype = "dashed", linewidth = 0.4, inherit.aes = FALSE
                )
            }

            # Specification limits
            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = lsl, linetype = "solid", color = "#C0392B", linewidth = 0.9
                ) + ggplot2::annotate(
                    "text", x = 0, y = lsl, label = paste0("LSL = ", lsl),
                    hjust = -0.05, vjust = -0.5, size = 3.3, color = "#C0392B", fontface = "bold"
                )
            }
            if (specType %in% c("upper", "both") && !is.null(usl)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = usl, linetype = "solid", color = "#C0392B", linewidth = 0.9
                ) + ggplot2::annotate(
                    "text", x = 0, y = usl, label = paste0("USL = ", usl),
                    hjust = -0.05, vjust = 1.3, size = 3.3, color = "#C0392B", fontface = "bold"
                )
            }

            # Extrapolation cap vertical line
            if (isTRUE(state$extrapActive) && !is.na(state$extrapCap) && !is.infinite(state$extrapCap)) {
                p <- p + ggplot2::geom_vline(
                    xintercept = state$extrapCap, linetype = "dotdash", color = "#7D3C98", linewidth = 0.8
                ) + ggplot2::annotate(
                    "label", x = state$extrapCap, y = Inf,
                    label = paste0(.("Extrapolation Limit:"), " ", private$.formatNum(state$extrapCap, 1), " ", .("mo")),
                    vjust = 1.3, hjust = 1.05, fill = "white", color = "#7D3C98", size = 3.2,
                    label.padding = ggplot2::unit(0.18, "lines"), label.r = ggplot2::unit(0.15, "lines")
                )
            }

            # Shelf life vertical line
            if (!is.na(ov_ich) && !is.infinite(ov_ich) && ov_ich > 0) {
                p <- p + ggplot2::geom_vline(
                    xintercept = ov_ich, linetype = "longdash", color = "#27AE60", linewidth = 1.0
                ) + ggplot2::annotate(
                    "label", x = ov_ich, y = -Inf,
                    label = paste0(.("Shelf Life"), ": ", private$.formatNum(ov_ich, 1), " ", .("months")),
                    vjust = -0.4, hjust = 1.05, fill = "white", color = "#27AE60", fontface = "bold", size = 3.6,
                    label.padding = ggplot2::unit(0.2, "lines"), label.r = ggplot2::unit(0.15, "lines")
                )
            }

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::theme(
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10.5, margin = ggplot2::margin(b = 10))
            )

            print(p)
            TRUE
        },

        .shelfPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            batch_df    <- state$shelf_batch_df
            ov_ich      <- state$ov_ich
            ov_stat     <- state$ov_stat
            t_dur       <- state$t_dur
            extrapCap   <- state$extrapCap
            extrapActive <- isTRUE(state$extrapActive)
            b_lim       <- state$limitingBatch
            batchName   <- state$batchName
            batchLevels <- state$batchLevels

            if (is.null(batch_df) || nrow(batch_df) == 0) return(FALSE)

            batch_df$Batch <- factor(batch_df$Batch, levels = rev(batchLevels))

            lim_label   <- .("Limiting batch")
            other_label <- .("Other batches")
            batch_df$Status <- factor(
                ifelse(batch_df$Limiting, lim_label, other_label),
                levels = c(lim_label, other_label)
            )

            valid_df <- batch_df[!is.na(batch_df$StatShelf), , drop = FALSE]
            na_df    <- batch_df[is.na(batch_df$StatShelf), , drop = FALSE]

            val_shelf <- valid_df$StatShelf[valid_df$StatShelf < 900]
            max_stat  <- if (length(val_shelf) > 0) max(val_shelf, na.rm = TRUE) else t_dur
            max_ref   <- max(c(max_stat, t_dur, if (!is.na(extrapCap) && !is.infinite(extrapCap)) extrapCap else 0, if (!is.na(ov_ich) && !is.infinite(ov_ich)) ov_ich else 0), na.rm = TRUE)
            max_x     <- max_ref * 1.30

            p <- ggplot2::ggplot(batch_df, ggplot2::aes(y = Batch))

            if (nrow(valid_df) > 0) {
                plot_valid <- valid_df
                plot_valid$PlotShelf <- ifelse(plot_valid$StatShelf >= 900, max_x * 0.92, plot_valid$StatShelf)
                plot_valid$LabelText <- ifelse(
                    plot_valid$StatShelf >= 900,
                    paste0("> ", private$.formatNum(max_stat, 0), " ", .("months")),
                    paste0(private$.formatNum(plot_valid$StatShelf, 1), " ", .("months"))
                )

                p <- p + ggplot2::geom_segment(
                    data = plot_valid,
                    ggplot2::aes(x = 0, xend = PlotShelf, y = Batch, yend = Batch),
                    linewidth = 1.2, color = "#2C3E50"
                ) +
                ggplot2::geom_point(
                    data = plot_valid,
                    ggplot2::aes(x = PlotShelf, color = Status),
                    size = 4.5
                ) +
                ggplot2::geom_text(
                    data = plot_valid,
                    ggplot2::aes(x = PlotShelf, label = LabelText),
                    hjust = -0.2, size = 3.5, fontface = "bold", color = "#2C3E50"
                )
            }

            if (nrow(na_df) > 0) {
                p <- p + ggplot2::geom_point(
                    data = na_df,
                    ggplot2::aes(x = 0),
                    color = "#95A5A6", size = 4.0, shape = 4, stroke = 1.5
                ) +
                ggplot2::geom_text(
                    data = na_df,
                    ggplot2::aes(x = 0, label = paste0("  [", .("Undetermined"), "]")),
                    hjust = 0, size = 3.3, fontface = "italic", color = "#7F8C8D"
                )
            }

            # Reference line 1: Observed duration
            p <- p + ggplot2::geom_vline(xintercept = t_dur, linetype = "dotted", color = "#7F8C8D", linewidth = 0.9) +
                ggplot2::annotate(
                    "text", x = t_dur, y = Inf,
                    label = paste0(.("Observed"), ": ", private$.formatNum(t_dur, 1), " ", .("months")),
                    vjust = 1.5, hjust = 1.05, color = "#7F8C8D", size = 3.2
                )

            # Reference line 2: Extrapolation Cap (when extrapCap > t_dur and finite)
            if (!is.na(extrapCap) && !is.infinite(extrapCap) && extrapCap > t_dur) {
                p <- p + ggplot2::geom_vline(xintercept = extrapCap, linetype = "dashed", color = "#2980B9", linewidth = 0.9) +
                    ggplot2::annotate(
                        "text", x = extrapCap, y = Inf,
                        label = paste0(.("Extrapolation Cap"), ": ", private$.formatNum(extrapCap, 1), " ", .("months")),
                        vjust = 1.5, hjust = -0.05, color = "#2980B9", size = 3.2
                    )
            }

            # Reference line 3: Permitted Shelf Life
            if (!is.na(ov_ich) && !is.infinite(ov_ich) && ov_ich > 0) {
                p <- p + ggplot2::geom_vline(xintercept = ov_ich, linetype = "solid", color = "#27AE60", linewidth = 1.1) +
                    ggplot2::annotate(
                        "text", x = ov_ich, y = -Inf,
                        label = paste0(.("Permitted Shelf Life"), ": ", private$.formatNum(ov_ich, 1), " ", .("months")),
                        vjust = -0.8, hjust = -0.05, color = "#27AE60", fontface = "bold", size = 3.5
                    )
            }

            p <- p + ggplot2::coord_cartesian(xlim = c(0, max_x), clip = "off")

            lim_info <- if (extrapActive) {
                .("Capped by extrapolation rule")
            } else if (!is.null(b_lim) && !is.na(b_lim)) {
                paste0(.("Limiting Batch:"), " ", b_lim)
            } else ""

            sub_text <- if (!is.na(ov_ich)) {
                paste0(
                    .("Permitted Shelf Life:"), " ", private$.formatNum(ov_ich, 1), " ", .("months"),
                    if (length(batchLevels) > 1 && nzchar(lim_info)) paste0("  |  ", lim_info) else ""
                )
            } else ""

            p <- p + ggplot2::labs(
                title = paste0(.("Shelf Life Comparison & Extrapolation Horizon"), " (ICH Q1E)"),
                subtitle = sub_text,
                x = .("Shelf Life (months)"),
                y = batchName,
                color = ""
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::scale_color_manual(
                name = "",
                values = stats::setNames(c("#C0392B", "#1B365D"), c(lim_label, other_label)),
                drop = FALSE
            )
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10.5, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank(),
                plot.margin = ggplot2::margin(t = 8, r = 25, b = 8, l = 8)
            )

            print(p)
            TRUE
        },

        .facetPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            obs_df   <- state$obs_df
            pred_df  <- state$pred_df
            specType <- state$specType
            lsl      <- state$lsl
            usl      <- state$usl
            depName  <- state$depName
            timeName <- state$timeName

            p <- ggplot2::ggplot(obs_df, ggplot2::aes(x = Time, y = Y)) +
                ggplot2::geom_point(color = "#2C3E50", size = 2.2, alpha = 0.8) +
                ggplot2::geom_line(data = pred_df, ggplot2::aes(x = Time, y = Fit), color = "#2980B9", linewidth = 0.95)

            if (specType == "lower") {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df, ggplot2::aes(x = Time, ymin = Lower, ymax = Fit),
                    fill = "#2980B9", alpha = 0.18, linetype = "dashed", linewidth = 0.35, inherit.aes = FALSE
                )
            } else if (specType == "upper") {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df, ggplot2::aes(x = Time, ymin = Fit, ymax = Upper),
                    fill = "#2980B9", alpha = 0.18, linetype = "dashed", linewidth = 0.35, inherit.aes = FALSE
                )
            } else {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df, ggplot2::aes(x = Time, ymin = Lower, ymax = Upper),
                    fill = "#2980B9", alpha = 0.18, linetype = "dashed", linewidth = 0.35, inherit.aes = FALSE
                )
            }

            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                p <- p + ggplot2::geom_hline(yintercept = lsl, linetype = "solid", color = "#C0392B", linewidth = 0.75)
            }
            if (specType %in% c("upper", "both") && !is.null(usl)) {
                p <- p + ggplot2::geom_hline(yintercept = usl, linetype = "solid", color = "#C0392B", linewidth = 0.75)
            }

            p <- p + ggplot2::facet_wrap(~ Batch, scales = "fixed") +
                ggplot2::labs(
                    title = paste0(.("Individual Batch Stability Trajectories"), " (ICH Q1E)"),
                    subtitle = .("Observed data, regression fits, and 95% confidence bands per batch"),
                    x = timeName,
                    y = depName
                )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 11)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 12.5),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10, margin = ggplot2::margin(b = 8)),
                strip.background = ggplot2::element_rect(fill = "#EAECEE", color = "#BDC3C7"),
                strip.text = ggplot2::element_text(face = "bold", size = 10)
            )

            print(p)
            TRUE
        },

        .ratePlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            df_rates  <- state$df_rates
            cs_slope  <- state$cs_slope
            cs_low    <- state$cs_low
            cs_upp    <- state$cs_upp
            p_slopes  <- state$p_slopes
            f_slopes  <- state$f_slopes
            alphaPool <- state$alphaPool
            k         <- state$k
            depName   <- state$depName
            timeName  <- state$timeName
            batchName <- state$batchName

            # Combine individual rates with pooled slope if k >= 2
            if (k >= 2 && !is.na(cs_slope)) {
                row_cs <- data.frame(
                    Batch = .("Pooled Slope"),
                    Slope = cs_slope,
                    Lower = cs_low,
                    Upper = cs_upp,
                    Type = .("Pooled slope"),
                    stringsAsFactors = FALSE
                )
                plot_data <- rbind(df_rates, row_cs)
                plot_data$Batch <- factor(plot_data$Batch, levels = rev(c(state$batchLevels, .("Pooled Slope"))))
            } else {
                plot_data <- df_rates
                plot_data$Batch <- factor(plot_data$Batch, levels = rev(state$batchLevels))
            }

            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Slope, y = Batch, color = Type)) +
                ggplot2::geom_point(size = 3.5) +
                ggplot2::geom_errorbarh(ggplot2::aes(xmin = Lower, xmax = Upper), height = 0.25, linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#7F8C8D")

            if (k >= 2 && !is.na(cs_slope)) {
                p <- p + ggplot2::geom_vline(xintercept = cs_slope, linetype = "dotted", color = "#8E44AD", linewidth = 0.7)
            }

            subtitle_text <- if (k < 2) {
                .("Single batch degradation rate with 95% confidence interval")
            } else if (!is.na(p_slopes)) {
                paste0(
                    .("Slopes ANCOVA Test:"), " F = ", private$.formatNum(f_slopes, 2),
                    ", p = ", private$.formatNum(p_slopes, 4),
                    " (", if (p_slopes >= alphaPool) .("Slopes are poolable") else .("Slopes differ significantly"), ")"
                )
            } else {
                .("Degradation rates across evaluated batches")
            }

            p <- p + ggplot2::labs(
                title = paste0(.("Degradation Rates / Slopes Comparison"), " (95% CI)"),
                subtitle = subtitle_text,
                x = paste0(.("Rate of Change"), " (", depName, " / ", timeName, ")"),
                y = batchName,
                color = .("Type")
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center"
            )

            print(p)
            TRUE
        },

        .predPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            obs_df    <- state$obs_df
            df_env    <- state$df_env
            srl_lower <- state$srl_lower
            srl_upper <- state$srl_upper
            specType  <- state$specType
            lsl       <- state$lsl
            usl       <- state$usl
            ov_ich    <- state$ov_ich
            t_star    <- state$t_star
            b_lim     <- state$b_lim
            depName   <- state$depName
            timeName  <- state$timeName
            batchName <- state$batchName

            p <- ggplot2::ggplot(obs_df, ggplot2::aes(x = Time, y = Y)) +
                ggplot2::geom_point(ggplot2::aes(color = Batch), size = 2.2, alpha = 0.7)

            # Prediction interval ribbon (wider)
            if (specType == "lower") {
                p <- p + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = PI_Low, ymax = Fit),
                    fill = "#85C1E9", alpha = 0.25, inherit.aes = FALSE
                ) + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = CI_Low, ymax = Fit),
                    fill = "#2E86C1", alpha = 0.35, inherit.aes = FALSE
                )
            } else if (specType == "upper") {
                p <- p + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = Fit, ymax = PI_Upp),
                    fill = "#85C1E9", alpha = 0.25, inherit.aes = FALSE
                ) + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = Fit, ymax = CI_Upp),
                    fill = "#2E86C1", alpha = 0.35, inherit.aes = FALSE
                )
            } else {
                p <- p + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = PI_Low, ymax = PI_Upp),
                    fill = "#85C1E9", alpha = 0.25, inherit.aes = FALSE
                ) + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = CI_Low, ymax = CI_Upp),
                    fill = "#2E86C1", alpha = 0.35, inherit.aes = FALSE
                )
            }

            p <- p + ggplot2::geom_line(data = df_env, ggplot2::aes(x = Time, y = Fit), color = "#1B4F72", linewidth = 1.0)

            # Specification limit lines
            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                p <- p + ggplot2::geom_hline(yintercept = lsl, linetype = "solid", color = "#C0392B", linewidth = 0.85)
            }
            if (specType %in% c("upper", "both") && !is.null(usl)) {
                p <- p + ggplot2::geom_hline(yintercept = usl, linetype = "solid", color = "#C0392B", linewidth = 0.85)
            }

            # Draw SRL lines
            if (!is.null(srl_lower) && is.finite(srl_lower)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = srl_lower, linetype = "dashed", color = "#E67E22", linewidth = 0.85
                ) + ggplot2::annotate(
                    "text", x = 0, y = srl_lower,
                    label = paste0(.("Lower SRL:"), " ", private$.formatNum(srl_lower, 2)),
                    hjust = -0.05, vjust = -0.5, size = 3.2, color = "#D35400"
                )
            }
            if (!is.null(srl_upper) && is.finite(srl_upper)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = srl_upper, linetype = "dashed", color = "#E67E22", linewidth = 0.85
                ) + ggplot2::annotate(
                    "text", x = 0, y = srl_upper,
                    label = paste0(.("Upper SRL:"), " ", private$.formatNum(srl_upper, 2)),
                    hjust = -0.05, vjust = 1.3, size = 3.2, color = "#D35400"
                )
            }

            # Mark Permitted Shelf Life
            if (!is.na(ov_ich) && !is.infinite(ov_ich) && ov_ich > 0) {
                p <- p + ggplot2::geom_vline(
                    xintercept = ov_ich, linetype = "longdash", color = "#27AE60", linewidth = 1.0
                ) + ggplot2::annotate(
                    "label", x = ov_ich, y = -Inf,
                    label = paste0(.("Shelf Life"), ": ", private$.formatNum(ov_ich, 1), " ", .("months")),
                    vjust = -0.4, hjust = 1.05, fill = "white", color = "#27AE60", fontface = "bold", size = 3.6,
                    label.padding = ggplot2::unit(0.2, "lines"), label.r = ggplot2::unit(0.15, "lines")
                )
            }

            srl_sub <- c()
            if (!is.null(srl_lower) && is.finite(srl_lower)) srl_sub <- c(srl_sub, paste0(.("Lower SRL:"), " ", private$.formatNum(srl_lower, 1)))
            if (!is.null(srl_upper) && is.finite(srl_upper)) srl_sub <- c(srl_sub, paste0(.("Upper SRL:"), " ", private$.formatNum(srl_upper, 1)))
            srl_text <- if (length(srl_sub) > 0) paste0("  |  ", paste(srl_sub, collapse = ", ")) else ""

            p <- p + ggplot2::labs(
                title = paste0(.("Prediction Intervals & Shelf-Life Release Limits (SRL)"), " (ICH Q1E)"),
                subtitle = paste0(
                    .("Limiting Batch:"), " ", b_lim,
                    if (!is.na(ov_ich)) paste0("  |  ", .("Permitted Shelf Life:"), " ", private$.formatNum(ov_ich, 1), " ", .("months")) else "",
                    srl_text
                ),
                x = timeName,
                y = depName,
                color = batchName
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10.5, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank()
            )

            print(p)
            TRUE
        },

        .diagPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$df_diag)) return(FALSE)

            df_diag  <- state$df_diag
            timeName <- if (!is.null(state$timeName)) state$timeName else "Time"
            depName  <- if (!is.null(state$depName)) state$depName else "Residuals"

            p1 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = Fitted, y = Residuals)) +
                ggplot2::geom_point(color = "#2C3E50", alpha = 0.8, size = 2) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C") +
                ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#2980B9", linewidth = 0.8) +
                ggplot2::labs(title = .("Residuals vs Fitted"), x = .("Fitted values"), y = .("Residuals"))

            p2 <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = StdResiduals)) +
                ggplot2::stat_qq(color = "#2C3E50", alpha = 0.8, size = 2) +
                ggplot2::stat_qq_line(color = "#E74C3C", linewidth = 0.8) +
                ggplot2::labs(title = .("Normal Q-Q"), x = .("Theoretical Quantiles"), y = .("Standardized Residuals"))

            p3 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = Time, y = Residuals)) +
                ggplot2::geom_point(color = "#2C3E50", alpha = 0.8, size = 2) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C") +
                ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#27AE60", linewidth = 0.8) +
                ggplot2::labs(title = .("Residuals vs Time"), x = timeName, y = .("Residuals"))

            p4 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = Fitted, y = SqrtAbsStdRes)) +
                ggplot2::geom_point(color = "#2C3E50", alpha = 0.8, size = 2) +
                ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#8E44AD", linewidth = 0.8) +
                ggplot2::labs(title = .("Scale-Location"), x = .("Fitted values"), y = paste0("\u221a|", .("Standardized Residuals"), "|"))

            apply_theme <- function(p) {
                if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 10)
                p + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5))
            }

            p1 <- apply_theme(p1)
            p2 <- apply_theme(p2)
            p3 <- apply_theme(p3)
            p4 <- apply_theme(p4)

            grid_plot <- tryCatch({
                cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
            }, error = function(e) {
                tryCatch({
                    patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
                }, error = function(e2) {
                    p1
                })
            })

            print(grid_plot)
            TRUE
        },

        .populateMultiShelfTable = function(evaluatedAttrs, governingAttrName, minStatShelf, minIchShelf, governingBatch) {
            table <- self$results$multiShelfTable
            table$deleteRows()

            for (attrName in names(evaluatedAttrs)) {
                res <- evaluatedAttrs[[attrName]]
                aDef <- res$attrDef
                specText <- if (aDef$specType == "lower") {
                    paste0("LSL \u2265 ", private$.formatNum(aDef$lsl, 2))
                } else if (aDef$specType == "upper") {
                    paste0("USL \u2264 ", private$.formatNum(aDef$usl, 2))
                } else {
                    paste0(private$.formatNum(aDef$lsl, 2), " \u2013 ", private$.formatNum(aDef$usl, 2))
                }

                stat_s <- res$shelfResults$overallStatShelf
                ich_s  <- res$shelfResults$overallIchShelf
                b_lim  <- res$shelfResults$limitingBatch

                is_gov <- (attrName == governingAttrName)
                statusText <- if (is_gov) {
                    paste0("\u2605 ", .("Worst-Case Driver (Governing)"))
                } else {
                    .("Compliant")
                }

                table$addRow(rowKey = attrName, values = list(
                    attr = aDef$name,
                    spec = specText,
                    model = res$adoptedModelName,
                    limitingBatch = if (!is.na(b_lim)) paste0(.("Batch"), " ", b_lim) else "",
                    statShelf = stat_s,
                    ichShelf = ich_s,
                    status = statusText
                ))
            }

            table$addRow(rowKey = "_overall_", values = list(
                attr = paste0("<b>", .("Overall Product Shelf Life"), "</b>"),
                spec = .("All CQAs Met"),
                model = .("Worst-Case Min"),
                limitingBatch = if (!is.na(governingBatch)) paste0(.("Batch"), " ", governingBatch) else "",
                statShelf = minStatShelf,
                ichShelf = minIchShelf,
                status = paste0("<b>", .("Governing Expiry Date"), "</b>")
            ))
        },

        .populateMultiBatchTable = function(evaluatedAttrs, batchLevels, minStatShelf) {
            table <- self$results$multiBatchTable
            table$deleteRows()

            attrNames <- names(evaluatedAttrs)
            n_attrs <- length(attrNames)

            for (b in batchLevels) {
                bShelves <- numeric(n_attrs)
                for (j in seq_len(n_attrs)) {
                    aName <- attrNames[j]
                    bShelves[j] <- evaluatedAttrs[[aName]]$shelfResults$batchRows[[b]]$statShelf
                }
                valid_b <- bShelves[!is.na(bShelves)]
                worst_b <- if (length(valid_b) > 0) min(valid_b) else NA_real_
                lim_idx <- if (!is.na(worst_b)) which(bShelves == worst_b)[1] else NA_integer_
                lim_attr_name <- if (!is.na(lim_idx)) attrNames[lim_idx] else ""

                vals <- list(
                    batch = paste0(.("Batch"), " ", b),
                    attr1 = if (n_attrs >= 1) bShelves[1] else NA_real_,
                    attr2 = if (n_attrs >= 2) bShelves[2] else NA_real_,
                    attr3 = if (n_attrs >= 3) bShelves[3] else NA_real_,
                    attr4 = if (n_attrs >= 4) bShelves[4] else NA_real_,
                    attr5 = if (n_attrs >= 5) bShelves[5] else NA_real_,
                    worstShelf = worst_b,
                    limitingAttr = lim_attr_name
                )
                table$addRow(rowKey = b, values = vals)
            }

            globalAttrShelves <- sapply(evaluatedAttrs, function(x) x$shelfResults$overallStatShelf)
            lim_global_idx <- which(globalAttrShelves == minStatShelf)[1]
            lim_global_attr <- if (!is.na(lim_global_idx)) attrNames[lim_global_idx] else ""

            table$addRow(rowKey = "_product_min_", values = list(
                batch = paste0("<b>", .("Product Minimum"), "</b>"),
                attr1 = if (n_attrs >= 1) globalAttrShelves[1] else NA_real_,
                attr2 = if (n_attrs >= 2) globalAttrShelves[2] else NA_real_,
                attr3 = if (n_attrs >= 3) globalAttrShelves[3] else NA_real_,
                attr4 = if (n_attrs >= 4) globalAttrShelves[4] else NA_real_,
                attr5 = if (n_attrs >= 5) globalAttrShelves[5] else NA_real_,
                worstShelf = minStatShelf,
                limitingAttr = paste0("<b>", lim_global_attr, "</b>")
            ))
        },

        .multiShelfPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$evaluatedAttrs)) return(FALSE)

            evaluatedAttrs    <- state$evaluatedAttrs
            governingAttrName <- state$governingAttrName
            minIchShelf       <- state$minIchShelf
            minStatShelf      <- state$minStatShelf
            governingBatch    <- state$governingBatch
            t_dur             <- state$t_dur
            extrapCap         <- state$extrapCap
            extrapActive      <- isTRUE(state$extrapActive)

            lim_label   <- .("Governing worst-case")
            other_label <- .("Other quality attributes")

            plotRows <- list()
            for (attrName in names(evaluatedAttrs)) {
                res_a  <- evaluatedAttrs[[attrName]]
                s_ich  <- res_a$shelfResults$overallIchShelf
                s_stat <- res_a$shelfResults$overallStatShelf
                b_lim  <- res_a$shelfResults$limitingBatch
                is_gov <- (attrName == governingAttrName)

                val <- if (!is.na(s_stat) && !is.infinite(s_stat)) s_stat else if (!is.na(s_ich) && !is.infinite(s_ich)) s_ich else NA

                label_suffix <- if (!is.null(b_lim) && !is.na(b_lim) && nzchar(b_lim)) paste0(" (", .("Batch"), " ", b_lim, ")") else ""
                attr_display <- paste0(attrName, label_suffix)

                plotRows[[length(plotRows) + 1]] <- data.frame(
                    Attribute   = attrName,
                    AttrDisplay = attr_display,
                    StatShelf   = val,
                    IchShelf    = s_ich,
                    IsGoverning = is_gov,
                    Status      = factor(ifelse(is_gov, lim_label, other_label), levels = c(lim_label, other_label)),
                    stringsAsFactors = FALSE
                )
            }
            df_plot <- do.call(rbind, plotRows)
            df_plot$AttrDisplay <- factor(df_plot$AttrDisplay, levels = rev(df_plot$AttrDisplay))

            valid_df <- df_plot[!is.na(df_plot$StatShelf), , drop = FALSE]
            na_df    <- df_plot[is.na(df_plot$StatShelf), , drop = FALSE]

            val_shelf <- valid_df$StatShelf[valid_df$StatShelf < 900]
            max_stat  <- if (length(val_shelf) > 0) max(val_shelf, na.rm = TRUE) else t_dur
            max_ref   <- max(c(max_stat, t_dur, if (!is.null(extrapCap) && !is.na(extrapCap) && !is.infinite(extrapCap)) extrapCap else 0, if (!is.null(minIchShelf) && !is.na(minIchShelf) && !is.infinite(minIchShelf)) minIchShelf else 0), na.rm = TRUE)
            max_x     <- max_ref * 1.32

            p <- ggplot2::ggplot(df_plot, ggplot2::aes(y = AttrDisplay))

            if (nrow(valid_df) > 0) {
                plot_valid <- valid_df
                plot_valid$PlotShelf <- ifelse(plot_valid$StatShelf >= 900, max_x * 0.90, plot_valid$StatShelf)
                plot_valid$LabelText <- ifelse(
                    plot_valid$StatShelf >= 900,
                    paste0("> ", private$.formatNum(max_stat, 0), " ", .("months")),
                    paste0(private$.formatNum(plot_valid$StatShelf, 1), " ", .("months"))
                )

                p <- p + ggplot2::geom_segment(
                    data = plot_valid,
                    ggplot2::aes(x = 0, xend = PlotShelf, y = AttrDisplay, yend = AttrDisplay),
                    linewidth = 1.2, color = "#2C3E50"
                ) +
                ggplot2::geom_point(
                    data = plot_valid,
                    ggplot2::aes(x = PlotShelf, color = Status),
                    size = 4.5
                ) +
                ggplot2::geom_text(
                    data = plot_valid,
                    ggplot2::aes(x = PlotShelf, label = LabelText),
                    hjust = -0.2, size = 3.5, fontface = "bold", color = "#2C3E50"
                )
            }

            if (nrow(na_df) > 0) {
                p <- p + ggplot2::geom_point(
                    data = na_df,
                    ggplot2::aes(x = 0),
                    color = "#95A5A6", size = 4.0, shape = 4, stroke = 1.5
                ) +
                ggplot2::geom_text(
                    data = na_df,
                    ggplot2::aes(x = 0, label = paste0("  [", .("Undetermined"), "]")),
                    hjust = 0, size = 3.3, fontface = "italic", color = "#7F8C8D"
                )
            }

            # Reference line 1: Observed duration
            if (!is.null(t_dur) && !is.na(t_dur) && t_dur > 0) {
                p <- p + ggplot2::geom_vline(xintercept = t_dur, linetype = "dotted", color = "#7F8C8D", linewidth = 0.9) +
                    ggplot2::annotate(
                        "text", x = t_dur, y = Inf,
                        label = paste0(.("Observed"), ": ", private$.formatNum(t_dur, 1), " ", .("months")),
                        vjust = 1.5, hjust = 1.05, color = "#7F8C8D", size = 3.2
                    )
            }

            # Reference line 2: Extrapolation Cap (when extrapCap > t_dur and finite)
            if (!is.null(extrapCap) && !is.na(extrapCap) && !is.infinite(extrapCap) && extrapCap > t_dur) {
                p <- p + ggplot2::geom_vline(xintercept = extrapCap, linetype = "dashed", color = "#2980B9", linewidth = 0.9) +
                    ggplot2::annotate(
                        "text", x = extrapCap, y = Inf,
                        label = paste0(.("Extrapolation Cap"), ": ", private$.formatNum(extrapCap, 1), " ", .("months")),
                        vjust = 1.5, hjust = -0.05, color = "#2980B9", size = 3.2
                    )
            }

            # Reference line 3: Overall Permitted Shelf Life
            ov_shelf <- if (!is.null(minIchShelf) && !is.na(minIchShelf) && !is.infinite(minIchShelf)) minIchShelf else minStatShelf
            if (!is.null(ov_shelf) && !is.na(ov_shelf) && !is.infinite(ov_shelf) && ov_shelf > 0) {
                p <- p + ggplot2::geom_vline(xintercept = ov_shelf, linetype = "solid", color = "#27AE60", linewidth = 1.1) +
                    ggplot2::annotate(
                        "text", x = ov_shelf, y = -Inf,
                        label = paste0(.("Overall Permitted Shelf Life"), ": ", private$.formatNum(ov_shelf, 1), " ", .("months")),
                        vjust = -0.8, hjust = -0.05, color = "#27AE60", fontface = "bold", size = 3.5
                    )
            }

            p <- p + ggplot2::coord_cartesian(xlim = c(0, max_x), clip = "off")

            govBatchText <- if (!is.null(governingBatch) && !is.na(governingBatch) && nzchar(governingBatch)) {
                paste0(" (", .("Batch"), " ", governingBatch, ")")
            } else ""

            sub_text <- if (!is.null(ov_shelf) && !is.na(ov_shelf)) {
                paste0(
                    .("Overall Permitted Shelf Life:"), " ", private$.formatNum(ov_shelf, 1), " ", .("months"),
                    "  |  ", .("Limiting CQA:"), " ", governingAttrName, govBatchText
                )
            } else ""

            p <- p + ggplot2::labs(
                title = .("Multi-Attribute Shelf Life Comparison & Horizon Plot"),
                subtitle = sub_text,
                x = .("Shelf Life (months)"),
                y = .("Quality Attribute"),
                color = ""
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::scale_color_manual(
                name = "",
                values = stats::setNames(c("#C0392B", "#1B365D"), c(lim_label, other_label)),
                drop = FALSE
            )
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10.5, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank(),
                plot.margin = ggplot2::margin(t = 8, r = 40, b = 8, l = 8)
            )

            print(p)
            TRUE
        },

        .runArrhenius = function(
            governingRes, data, timeName, batchName, tempVarName,
            arrhEa, arrhTempAcc, arrhRhAcc, arrhTempLong, arrhRhLong, arrhB, showArrhSens,
            evalIsLong = FALSE
        ) {
            chosenFit <- governingRes$chosenFit
            cf <- stats::coef(chosenFit)
            specType <- governingRes$attrDef$specType

            if (governingRes$selectedModelCode == 1) {
                b_lim <- governingRes$shelfResults$limitingBatch
                sub_b <- governingRes$dat[governingRes$dat$Batch == b_lim, , drop = FALSE]
                if (nrow(sub_b) >= 2 && length(unique(sub_b$Time)) >= 2) {
                    fit_b <- stats::lm(Y_trans ~ Time, data = sub_b)
                    raw_slp <- stats::coef(fit_b)["Time"]
                } else {
                    raw_slp <- cf["Time"]
                }
            } else {
                raw_slp <- cf["Time"]
            }

            if (specType %in% c("lower", "both")) {
                k_eval <- -raw_slp
            } else {
                k_eval <- raw_slp
            }
            if (is.na(k_eval) || k_eval <= 0) {
                k_eval <- abs(raw_slp)
            }
            if (is.na(k_eval) || k_eval <= 0) {
                k_eval <- 0.001
            }

            base_shelf <- governingRes$shelfResults$overallStatShelf
            if (is.na(base_shelf) || is.infinite(base_shelf)) {
                base_shelf <- 12.0
            }

            R_gas <- 8.314462618

            calcAF = function(t_tgt, rh_tgt, ea_use = arrhEa) {
                inv_diff <- (1 / (t_tgt + 273.15)) - (1 / (arrhTempAcc + 273.15))
                rh_diff  <- arrhRhAcc - rh_tgt
                af_val   <- exp((ea_use * 1000 / R_gas) * inv_diff + arrhB * rh_diff)
                return(af_val)
            }

            af_long_nominal <- calcAF(arrhTempLong, arrhRhLong)

            # Determine k_acc and accelerated shelf-life baseline
            if (evalIsLong) {
                k_acc <- k_eval * af_long_nominal
                shelf_acc <- base_shelf / af_long_nominal
            } else {
                k_acc <- k_eval
                shelf_acc <- base_shelf
            }

            emp_pts <- NULL

            # Multi-temperature empirical regression if tempVar supplied
            if (length(tempVarName) > 0 && nzchar(tempVarName) && tempVarName %in% colnames(data)) {
                sub_t <- jmvcore::naOmit(data[, c(governingRes$attrDef$name, timeName, tempVarName)])
                colnames(sub_t) <- c("Y", "Time", "Temp")
                sub_t$Y <- as.numeric(sub_t$Y)
                sub_t$Time <- as.numeric(sub_t$Time)
                sub_t$Temp <- as.numeric(sub_t$Temp)

                u_temps <- sort(unique(sub_t$Temp))
                if (length(u_temps) >= 2) {
                    k_vals <- numeric(length(u_temps))
                    inv_T_vals <- numeric(length(u_temps))
                    for (i in seq_along(u_temps)) {
                        d_ti <- sub_t[sub_t$Temp == u_temps[i], ]
                        if (nrow(d_ti) >= 2 && length(unique(d_ti$Time)) >= 2) {
                            f_ti <- stats::lm(Y ~ Time, data = d_ti)
                            s_ti <- stats::coef(f_ti)["Time"]
                            ki <- if (specType %in% c("lower", "both")) -s_ti else s_ti
                            if (is.na(ki) || ki <= 0) ki <- abs(s_ti)
                            k_vals[i] <- max(ki, 1e-6)
                        } else {
                            k_vals[i] <- max(k_acc, 1e-6)
                        }
                        inv_T_vals[i] <- 1 / (u_temps[i] + 273.15)
                    }

                    df_reg <- data.frame(ln_k = log(k_vals), inv_T = inv_T_vals)
                    fit_reg <- stats::lm(ln_k ~ inv_T, data = df_reg)
                    cf_reg <- stats::coef(fit_reg)
                    se_reg <- summary(fit_reg)$coefficients

                    beta_1 <- cf_reg["inv_T"]
                    beta_0 <- cf_reg["(Intercept)"]
                    ea_fitted_kJ <- -beta_1 * R_gas / 1000

                    regTable <- self$results$arrhRegTable
                    regTable$deleteRows()
                    regTable$addRow(rowKey = "Ea", values = list(
                        param = .("Activation Energy (Ea, kJ/mol)"),
                        est = ea_fitted_kJ,
                        se = if (nrow(se_reg) >= 2) se_reg[2, 2] * R_gas / 1000 else NA_real_,
                        t = if (nrow(se_reg) >= 2) se_reg[2, 3] else NA_real_,
                        p = if (nrow(se_reg) >= 2) se_reg[2, 4] else NA_real_,
                        ci = if (nrow(se_reg) >= 2) paste0(
                            private$.formatNum(ea_fitted_kJ - 1.96 * se_reg[2, 2] * R_gas / 1000, 1),
                            " \u2013 ",
                            private$.formatNum(ea_fitted_kJ + 1.96 * se_reg[2, 2] * R_gas / 1000, 1)
                        ) else ""
                    ))
                    regTable$addRow(rowKey = "lnA", values = list(
                        param = .("Pre-exponential factor (ln A)"),
                        est = beta_0,
                        se = if (nrow(se_reg) >= 1) se_reg[1, 2] else NA_real_,
                        t = if (nrow(se_reg) >= 1) se_reg[1, 3] else NA_real_,
                        p = if (nrow(se_reg) >= 1) se_reg[1, 4] else NA_real_,
                        ci = ""
                    ))
                    r2_val <- summary(fit_reg)$r.squared
                    regTable$addRow(rowKey = "R2", values = list(
                        param = .("Coefficient of Determination (R²)"),
                        est = r2_val,
                        se = NA_real_, t = NA_real_, p = NA_real_, ci = ""
                    ))

                    if (ea_fitted_kJ > 10 && ea_fitted_kJ < 300) {
                        arrhEa <- ea_fitted_kJ
                    }

                    if (arrhTempAcc %in% u_temps) {
                        k_acc <- k_vals[which(u_temps == arrhTempAcc)]
                    }
                    emp_pts <- list(
                        InvT = 1000 / (u_temps + 273.15),
                        ln_k = log(k_vals),
                        TempC = u_temps
                    )
                }
            }

            zones <- list(
                list(name = .("Long-term Storage (Zone II)"), temp = arrhTempLong, rh = arrhRhLong, note = .("Target Registration Condition")),
                list(name = .("Intermediate Condition (Zone IVa)"), temp = 30.0, rh = 65.0, note = .("Tropical Climates")),
                list(name = .("Hot & Humid Condition (Zone IVb)"), temp = 30.0, rh = 75.0, note = .("Extreme Climates")),
                list(name = .("Refrigerated Storage (Cold Chain)"), temp = 5.0, rh = 0.0, note = .("2°C – 8°C Protected")),
                list(name = .("Accelerated Condition (Reference)"), temp = arrhTempAcc, rh = arrhRhAcc, note = .("Empirical Baseline (Study Condition)"))
            )

            arrhTable <- self$results$arrhTable
            arrhTable$deleteRows()

            for (idx in seq_along(zones)) {
                zn <- zones[[idx]]
                af_i <- calcAF(zn$temp, zn$rh, ea_use = arrhEa)
                rate_i <- k_acc / af_i
                pred_sl_i <- shelf_acc * af_i

                arrhTable$addRow(rowKey = paste0("z_", idx), values = list(
                    zone = zn$name,
                    temp = zn$temp,
                    rh = zn$rh,
                    af = af_i,
                    rate = rate_i,
                    predShelf = pred_sl_i,
                    extrapNote = zn$note
                ))
            }

            if (showArrhSens) {
                sensTable <- self$results$arrhSensTable
                sensTable$deleteRows()

                ea_values <- c(50.0, 60.0, 70.0, 80.0, 83.14, 90.0, 100.0, 110.0, 120.0)
                inv_diff_25 <- (1 / (arrhTempLong + 273.15)) - (1 / (arrhTempAcc + 273.15))
                rh_diff_25  <- arrhRhAcc - arrhRhLong

                for (idx in seq_along(ea_values)) {
                    ea_val <- ea_values[idx]
                    af_25  <- exp((ea_val * 1000 / R_gas) * inv_diff_25 + arrhB * rh_diff_25)
                    pred_25 <- shelf_acc * af_25

                    risk_text <- if (ea_val <= 60) {
                        .("Highly Conservative (Low thermal sensitivity)")
                    } else if (ea_val < 80) {
                        .("Moderately Conservative")
                    } else if (abs(ea_val - 83.14) < 1.0) {
                        .("Consensus Benchmark (20 kcal/mol)")
                    } else if (ea_val <= 100) {
                        .("Standard Activation Energy")
                    } else {
                        .("Optimistic (High thermal sensitivity)")
                    }

                    sensTable$addRow(rowKey = paste0("ea_", idx), values = list(
                        ea = ea_val,
                        af = af_25,
                        predShelf = pred_25,
                        assessment = risk_text
                    ))
                }
            }

            y0_val <- if (!is.null(governingRes$dat) && !is.null(governingRes$dat$Y_orig)) {
                mean(governingRes$dat$Y_orig[governingRes$dat$Time == min(governingRes$dat$Time)], na.rm = TRUE)
            } else 100.0

            arrhState <- list(
                k_acc = k_acc,
                base_shelf = base_shelf,
                shelf_acc = shelf_acc,
                y0_val = y0_val,
                arrhEa = arrhEa,
                arrhTempAcc = arrhTempAcc,
                arrhRhAcc = arrhRhAcc,
                arrhTempLong = arrhTempLong,
                arrhRhLong = arrhRhLong,
                arrhB = arrhB,
                governingAttrName = governingRes$attrDef$name,
                specType = specType,
                lsl_val = governingRes$attrDef$lsl,
                usl_val = governingRes$attrDef$usl,
                emp_pts = emp_pts
            )
            self$results$arrhPlot$setState(arrhState)
            self$results$arrhTrajPlot$setState(arrhState)
        },

        .arrhPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$arrhEa)) return(FALSE)

            Ea <- state$arrhEa
            k_acc <- state$k_acc
            T_acc <- state$arrhTempAcc
            R_gas <- 8.314462618

            t_c_seq <- seq(0, 50, by = 1)
            t_k_seq <- t_c_seq + 273.15
            inv_T   <- 1000 / t_k_seq

            inv_T_acc <- 1000 / (T_acc + 273.15)
            ln_k_seq  <- log(k_acc) - (Ea * 1000 / R_gas) * (1 / t_k_seq - 1 / (T_acc + 273.15))

            df_line <- data.frame(InvT = inv_T, ln_k = ln_k_seq, TempC = t_c_seq)

            key_pts <- data.frame(
                TempC = c(40, 30, 25, 5),
                Condition = c(.("40°C Accelerated"), .("30°C Intermediate"), .("25°C Long-term"), .("5°C Refrigerated")),
                Color = c("#E74C3C", "#E67E22", "#2980B9", "#27AE60"),
                stringsAsFactors = FALSE
            )
            key_pts$InvT <- 1000 / (key_pts$TempC + 273.15)
            key_pts$ln_k <- log(k_acc) - (Ea * 1000 / R_gas) * (1 / (key_pts$TempC + 273.15) - 1 / (T_acc + 273.15))

            p <- ggplot2::ggplot(df_line, ggplot2::aes(x = InvT, y = ln_k)) +
                ggplot2::geom_line(color = "#1B365D", linewidth = 1.1)

            if (!is.null(state$emp_pts) && length(state$emp_pts$InvT) > 0) {
                emp_df <- data.frame(
                    InvT = state$emp_pts$InvT,
                    ln_k = state$emp_pts$ln_k,
                    TempC = state$emp_pts$TempC
                )
                p <- p + ggplot2::geom_point(
                    data = emp_df,
                    ggplot2::aes(x = InvT, y = ln_k),
                    shape = 1, size = 6, stroke = 1.3, color = "#2C3E50"
                )
            }

            p <- p +
                ggplot2::geom_point(data = key_pts, ggplot2::aes(x = InvT, y = ln_k, color = Condition), size = 4) +
                ggplot2::geom_text(
                    data = key_pts,
                    ggplot2::aes(x = InvT, y = ln_k, label = paste0(TempC, "°C")),
                    vjust = -0.9, fontface = "bold", size = 3.5, color = "#2C3E50"
                ) +
                ggplot2::scale_color_manual(values = stats::setNames(key_pts$Color, key_pts$Condition)) +
                ggplot2::labs(
                    title = .("Arrhenius Degradation Kinetics Plot"),
                    subtitle = paste0(
                        .("Activation Energy (Ea):"), " ", private$.formatNum(Ea, 2), " kJ/mol",
                        "  |  ", .("Baseline rate (40°C):"), " ", private$.formatNum(k_acc, 4), " / ", .("month")
                    ),
                    x = paste0("1000 / T (1/K)  [", .("Inverse Absolute Temperature"), "]"),
                    y = paste0("ln(k)  [", .("Log Degradation Rate"), "]"),
                    color = ""
                )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10.5, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                panel.grid.minor = ggplot2::element_blank()
            )

            print(p)
            TRUE
        },

        .arrhTrajPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$arrhEa)) return(FALSE)

            Ea <- state$arrhEa
            k_acc <- state$k_acc
            T_acc <- state$arrhTempAcc
            RH_acc <- state$arrhRhAcc
            B_rh <- state$arrhB
            base_shelf <- state$base_shelf
            shelf_acc <- if (!is.null(state$shelf_acc)) state$shelf_acc else base_shelf
            specType <- state$specType
            lsl_val <- state$lsl_val
            usl_val <- state$usl_val
            attrName <- state$governingAttrName
            R_gas <- 8.314462618

            conds <- list(
                list(label = .("40°C / 75% RH (Accelerated)"), temp = 40, rh = 75, color = "#E74C3C"),
                list(label = .("30°C / 65% RH (Intermediate)"), temp = 30, rh = 65, color = "#E67E22"),
                list(label = .("25°C / 60% RH (Long-term)"), temp = 25, rh = 60, color = "#2980B9"),
                list(label = .("5°C Refrigerated"), temp = 5, rh = 0, color = "#27AE60")
            )

            is_lower <- specType %in% c("lower", "both")
            y0 <- if (!is.null(state$y0_val) && !is.na(state$y0_val)) {
                state$y0_val
            } else if (is_lower) {
                if (!is.null(lsl_val) && !is.na(lsl_val)) lsl_val + k_acc * shelf_acc else 100
            } else {
                if (!is.null(usl_val) && !is.na(usl_val)) max(0, usl_val - k_acc * shelf_acc) else 0
            }

            max_time <- max(shelf_acc * 6, base_shelf * 1.5, 48)
            time_grid <- seq(0, max_time, length.out = 100)

            traj_list <- list()
            for (c_item in conds) {
                inv_diff <- (1 / (c_item$temp + 273.15)) - (1 / (T_acc + 273.15))
                rh_diff  <- RH_acc - c_item$rh
                af <- exp((Ea * 1000 / R_gas) * inv_diff + B_rh * rh_diff)
                k_i <- k_acc / af
                y_traj <- if (is_lower) y0 - k_i * time_grid else y0 + k_i * time_grid

                traj_list[[length(traj_list) + 1]] <- data.frame(
                    Time = time_grid,
                    Concentration = y_traj,
                    Condition = c_item$label,
                    stringsAsFactors = FALSE
                )
            }
            df_traj <- do.call(rbind, traj_list)

            cond_colors <- stats::setNames(sapply(conds, function(c) c$color), sapply(conds, function(c) c$label))

            p <- ggplot2::ggplot(df_traj, ggplot2::aes(x = Time, y = Concentration, color = Condition)) +
                ggplot2::geom_line(linewidth = 1.1) +
                ggplot2::scale_color_manual(values = cond_colors)

            if (is_lower && !is.null(lsl_val) && !is.na(lsl_val)) {
                p <- p + ggplot2::geom_hline(yintercept = lsl_val, linetype = "dashed", color = "#C0392B", linewidth = 1)
            }
            if (!is_lower && !is.null(usl_val) && !is.na(usl_val)) {
                p <- p + ggplot2::geom_hline(yintercept = usl_val, linetype = "dashed", color = "#C0392B", linewidth = 1)
            }

            p <- p + ggplot2::labs(
                title = .("Projected Degradation Trajectories Across Storage Temperatures"),
                subtitle = paste0(
                    .("Predicted via Arrhenius kinetics for attribute:"), " '", attrName, "'",
                    "  |  Ea = ", private$.formatNum(Ea, 1), " kJ/mol"
                ),
                x = paste0(.("Storage Time"), " (", .("months"), ")"),
                y = paste0(attrName, " (", .("Response Level"), ")"),
                color = ""
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10.5, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                panel.grid.minor = ggplot2::element_blank()
            )

            print(p)
            TRUE
        }
    )
)
