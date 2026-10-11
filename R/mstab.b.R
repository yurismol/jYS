# This file is a generated template, your changes will not be overwritten

mSTABClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mSTABClass",
    inherit = mSTABBase,
    private = list(

        .formatNum = function(x, digits = 1) {
            if (is.null(x) || length(x) == 0) return("")
            is_ru <- tryCatch({
                .("months") == "мес." ||
                grepl("ru", Sys.getenv("LANG"), ignore.case = TRUE) ||
                grepl("Russian", Sys.getlocale(), ignore.case = TRUE)
            }, error = function(e) FALSE)
            format_one <- function(v) {
                if (is.na(v) || is.infinite(v)) return("")
                v_rnd <- round(v, digits)
                s <- as.character(v_rnd)
                if (is_ru) gsub("\\.", ",", s) else s
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
                .("<b>ICH Q1E Appendix A Extrapolation</b>: Permitted shelf life may extend beyond real-time data if accelerated stability demonstrates little or no change. Under favorable conditions, maximum extrapolation is capped at 2x observed duration (up to Y + 12 months), or 1.5x observed duration (up to Y + 6 months) if accelerated data show change. Extrapolation is never permitted beyond the statistical intersection.")
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
        },

        .run = function() {
            depName   <- self$options$dep
            timeName  <- self$options$time
            batchName <- self$options$batch

            if (is.null(depName) || is.null(timeName) || is.null(batchName)) {
                self$results$instructions$setVisible(TRUE)
                return()
            }

            self$results$instructions$setVisible(FALSE)

            # Extract data
            raw_data <- self$data
            if (is.null(raw_data) || nrow(raw_data) == 0)
                return()

            dat <- data.frame(
                Y     = jmvcore::toNumeric(raw_data[[depName]]),
                Time  = jmvcore::toNumeric(raw_data[[timeName]]),
                Batch = as.factor(raw_data[[batchName]]),
                stringsAsFactors = FALSE
            )

            # Complete cases
            valid_idx <- stats::complete.cases(dat$Y, dat$Time, dat$Batch)
            dat <- dat[valid_idx, , drop = FALSE]

            if (nrow(dat) < 4) {
                jmvcore::reject(.("Too few valid observations for stability analysis (at least 4 observations required)."))
                return()
            }

            # Drop unused levels
            dat$Batch <- droplevels(dat$Batch)
            batchLevels <- levels(dat$Batch)
            k <- length(batchLevels)

            if (k < 1) {
                jmvcore::reject(.("No valid batch levels found."))
                return()
            }

            # Handle transformation
            trans <- self$options$trans
            if (trans == "log") {
                if (any(dat$Y <= 0)) {
                    jmvcore::reject(.("Log transformation requires strictly positive values of the response variable."))
                    return()
                }
                dat$Y_trans <- log(dat$Y)
            } else {
                dat$Y_trans <- dat$Y
            }

            # Specification limits
            specType <- self$options$specType
            lsl_val  <- self$options$lsl
            usl_val  <- self$options$usl

            if (trans == "log") {
                lsl_trans <- if (!is.null(lsl_val) && lsl_val > 0) log(lsl_val) else -Inf
                usl_trans <- if (!is.null(usl_val) && usl_val > 0) log(usl_val) else Inf
            } else {
                lsl_trans <- if (!is.null(lsl_val)) lsl_val else -Inf
                usl_trans <- if (!is.null(usl_val)) usl_val else Inf
            }

            # ANCOVA models
            # Model 1: Separate slopes & intercepts: Y ~ Batch * Time
            # Model 2: Common slope & separate intercepts: Y ~ Batch + Time
            # Model 3: Common slope & common intercept: Y ~ Time
            fit_full <- stats::lm(Y_trans ~ Batch * Time, data = dat)
            fit_cs   <- stats::lm(Y_trans ~ Batch + Time, data = dat)
            fit_cl   <- stats::lm(Y_trans ~ Time, data = dat)

            # RSS and degrees of freedom
            rss_full <- sum(stats::residuals(fit_full)^2)
            df_full  <- stats::df.residual(fit_full)
            mse_full <- if (df_full > 0) rss_full / df_full else NA_real_

            rss_cs   <- sum(stats::residuals(fit_cs)^2)
            df_cs    <- stats::df.residual(fit_cs)
            mse_cs   <- if (df_cs > 0) rss_cs / df_cs else NA_real_

            rss_cl   <- sum(stats::residuals(fit_cl)^2)
            df_cl    <- stats::df.residual(fit_cl)
            mse_cl   <- if (df_cl > 0) rss_cl / df_cl else NA_real_

            alphaPool <- self$options$alphaPool

            # Step 1: Slopes test
            if (k >= 2 && df_full > 0 && !is.na(mse_full) && mse_full > 0) {
                ss_slopes <- max(0, rss_cs - rss_full)
                df_slopes <- k - 1
                ms_slopes <- ss_slopes / df_slopes
                f_slopes  <- ms_slopes / mse_full
                p_slopes  <- 1 - stats::pf(f_slopes, df_slopes, df_full)
            } else {
                ss_slopes <- if (k >= 2) max(0, rss_cs - rss_full) else 0
                df_slopes <- if (k >= 2) k - 1 else 0
                ms_slopes <- if (df_slopes > 0) ss_slopes / df_slopes else 0
                f_slopes  <- NA_real_
                p_slopes  <- if (k < 2) 1.0 else NA_real_
            }

            # Step 2: Intercepts test
            if (k >= 2 && df_cs > 0 && !is.na(mse_cs) && mse_cs > 0) {
                ss_ints <- max(0, rss_cl - rss_cs)
                df_ints <- k - 1
                ms_ints <- ss_ints / df_ints
                f_ints  <- ms_ints / mse_cs
                p_ints  <- 1 - stats::pf(f_ints, df_ints, df_cs)
            } else {
                ss_ints <- if (k >= 2) max(0, rss_cl - rss_cs) else 0
                df_ints <- if (k >= 2) k - 1 else 0
                ms_ints <- if (df_ints > 0) ss_ints / df_ints else 0
                f_ints  <- NA_real_
                p_ints  <- if (k < 2) 1.0 else NA_real_
            }

            # Model choice
            modelChoice <- self$options$modelChoice
            selectedModelCode <- 3
            adoptedModelName  <- .("Common slope & common intercept (Model 3)")

            if (modelChoice == "sep_slopes_sep_int") {
                selectedModelCode <- 1
                adoptedModelName  <- .("Separate slopes & separate intercepts (Model 1)")
            } else if (modelChoice == "common_slope_sep_int") {
                selectedModelCode <- 2
                adoptedModelName  <- .("Common slope & separate intercepts (Model 2)")
            } else if (modelChoice == "common_slope_common_int") {
                selectedModelCode <- 3
                adoptedModelName  <- .("Common slope & common intercept (Model 3)")
            } else {
                # Automatic decision tree according to ICH Q1E Appendix B
                if (k >= 2 && !is.na(p_slopes) && p_slopes <= alphaPool) {
                    selectedModelCode <- 1
                    adoptedModelName  <- .("Separate slopes & separate intercepts (Model 1)")
                } else if (k >= 2 && !is.na(p_ints) && p_ints <= alphaPool) {
                    selectedModelCode <- 2
                    adoptedModelName  <- .("Common slope & separate intercepts (Model 2)")
                } else if (k >= 2 && !is.na(p_slopes) && !is.na(p_ints)) {
                    selectedModelCode <- 3
                    adoptedModelName  <- .("Common slope & common intercept (Model 3)")
                } else {
                    selectedModelCode <- 1
                    adoptedModelName  <- .("Separate slopes & separate intercepts (Model 1)")
                }
            }

            # Populate ANCOVA Table
            private$.populatePoolTable(
                k, df_slopes, ss_slopes, ms_slopes, f_slopes, p_slopes,
                df_ints, ss_ints, ms_ints, f_ints, p_ints,
                df_full, rss_full, mse_full, alphaPool, selectedModelCode
            )

            # Shelf life calculation
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
                confLevel = self$options$confLevel,
                trans = trans
            )

            # Store computed details for plotting
            plotState <- list(
                dat = dat,
                batchLevels = batchLevels,
                selectedModelCode = selectedModelCode,
                fit_full = fit_full,
                fit_cs = fit_cs,
                fit_cl = fit_cl,
                shelfResults = shelfResults,
                specType = specType,
                lsl = lsl_val,
                usl = usl_val,
                trans = trans,
                depName = depName,
                timeName = timeName,
                batchName = batchName,
                alphaPool = self$options$alphaPool,
                confLevel = self$options$confLevel
            )
            self$results$stabPlot$setState(plotState)
            self$results$shelfPlot$setState(plotState)
            self$results$facetPlot$setState(plotState)
            self$results$ratePlot$setState(plotState)
            self$results$predPlot$setState(plotState)

            # Store for diagnostic plot
            chosenFit <- if (selectedModelCode == 1) fit_full else if (selectedModelCode == 2) fit_cs else fit_cl
            self$results$diagPlot$setState(list(
                fit = chosenFit,
                dat = dat,
                timeName = timeName
            ))

            # Populate Shelf Life Table
            private$.populateShelfTable(shelfResults, adoptedModelName, dat)

            # Populate Coefficients Table
            private$.populateCoefTable(chosenFit)

            # Populate Diagnostics Table
            private$.populateDiagTable(chosenFit, dat)
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
            model_summary <- if (selectedModelCode == 1) {
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
            # Critical t value for 95% one-sided (or two-sided) bound
            alpha <- 1 - confLevel
            resList <- list()

            # Time range
            t_max_obs <- max(dat$Time, na.rm = TRUE)

            # Function to calculate confidence bound for a given model and batch at time t
            # Note: For Model 1 (separate slopes), user specification requires:
            # "Where slopes differ, use the pooled residual mean square error across batches."
            # df for t-statistic is df_full (pooled degrees of freedom).
            calcBound <- function(batch_i, t_eval, modelCode, boundType) {
                if (modelCode == 1) {
                    # Model 1: Separate slopes and separate intercepts with pooled MSE
                    sub_dat <- dat[dat$Batch == batch_i, , drop = FALSE]
                    n_i <- nrow(sub_dat)
                    t_bar_i <- mean(sub_dat$Time)
                    S_tt_i <- sum((sub_dat$Time - t_bar_i)^2)

                    # Fit batch-specific coefficients
                    fit_i <- stats::lm(Y_trans ~ Time, data = sub_dat)
                    coef_i <- stats::coef(fit_i)
                    y_hat <- coef_i[1] + coef_i[2] * t_eval

                    # SE using pooled MSE from full model
                    if (is.na(mse_full) || mse_full <= 0 || df_full <= 0 || S_tt_i <= 0) {
                        se_pred <- rep(NA_real_, length(t_eval))
                    } else {
                        se_pred <- sqrt(mse_full * (1 / n_i + (t_eval - t_bar_i)^2 / S_tt_i))
                    }
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

                if (boundType == "lower") {
                    t_crit <- stats::qt(1 - alpha, df = df_val)
                    return(y_hat - t_crit * se_pred)
                } else if (boundType == "upper") {
                    t_crit <- stats::qt(1 - alpha, df = df_val)
                    return(y_hat + t_crit * se_pred)
                } else {
                    # Two-sided 95%
                    t_crit <- stats::qt(1 - alpha / 2, df = df_val)
                    return(list(
                        lower = y_hat - t_crit * se_pred,
                        upper = y_hat + t_crit * se_pred
                    ))
                }
            }

            # Solver to find earliest time t >= 0 where bound crosses specification limit
            findCrossing <- function(batch_i, modelCode) {
                # Test crossing for lower bound
                cross_lower <- NA_real_
                cross_upper <- NA_real_

                if (specType %in% c("lower", "both")) {
                    b_0 <- calcBound(batch_i, 0, modelCode, "lower")
                    if (!is.na(b_0) && b_0 < lsl_trans) {
                        cross_lower <- 0.0 # Out of specification at time zero
                    } else {
                        # Search for crossing in [0, 240] months
                        f_root_lower <- function(t) calcBound(batch_i, t, modelCode, "lower") - lsl_trans
                        b_far <- calcBound(batch_i, 240, modelCode, "lower")
                        if (!is.na(b_far) && b_far <= lsl_trans) {
                            tryCatch({
                                r_res <- stats::uniroot(f_root_lower, interval = c(0, 240), tol = 1e-4)
                                cross_lower <- r_res$root
                            }, error = function(e) {
                                cross_lower <- NA_real_
                            })
                        } else {
                            cross_lower <- Inf # No crossing within 240 months
                        }
                    }
                }

                if (specType %in% c("upper", "both")) {
                    b_0 <- calcBound(batch_i, 0, modelCode, "upper")
                    if (!is.na(b_0) && b_0 > usl_trans) {
                        cross_upper <- 0.0
                    } else {
                        f_root_upper <- function(t) calcBound(batch_i, t, modelCode, "upper") - usl_trans
                        b_far <- calcBound(batch_i, 240, modelCode, "upper")
                        if (!is.na(b_far) && b_far >= usl_trans) {
                            tryCatch({
                                r_res <- stats::uniroot(f_root_upper, interval = c(0, 240), tol = 1e-4)
                                cross_upper <- r_res$root
                            }, error = function(e) {
                                cross_upper <- NA_real_
                            })
                        } else {
                            cross_upper <- Inf
                        }
                    }
                }

                # Earliest crossing
                cand <- c(cross_lower, cross_upper)
                cand <- cand[!is.na(cand)]
                if (length(cand) == 0) return(NA_real_)
                return(min(cand))
            }

            # Calculate for each batch
            batchRows <- list()
            for (b in batchLevels) {
                # Coefficients for display
                sub_dat <- dat[dat$Batch == b, , drop = FALSE]
                if (selectedModelCode == 1) {
                    fit_b <- stats::lm(Y_trans ~ Time, data = sub_dat)
                    cf <- stats::coef(fit_b)
                    int_val  <- if (trans == "log") exp(cf[1]) else cf[1]
                    slp_val  <- cf[2]
                } else if (selectedModelCode == 2) {
                    cf_cs <- stats::coef(fit_cs)
                    # Baseline intercept + batch offset
                    batch_term <- paste0("Batch", b)
                    offset <- if (batch_term %in% names(cf_cs)) cf_cs[[batch_term]] else 0
                    int_raw <- cf_cs[1] + offset
                    int_val  <- if (trans == "log") exp(int_raw) else int_raw
                    slp_val  <- cf_cs["Time"]
                } else {
                    cf_cl <- stats::coef(fit_cl)
                    int_val  <- if (trans == "log") exp(cf_cl[1]) else cf_cl[1]
                    slp_val  <- cf_cl["Time"]
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

            if (accel == "change_observed") {
                extrapCap <- t_dur
            } else if (accel == "no_change") {
                if (t_dur >= 12) {
                    extrapCap <- min(2 * t_dur, t_dur + 12)
                } else {
                    extrapCap <- min(2 * t_dur, t_dur + 6)
                }
            } else {
                # None / not evaluated
                extrapCap <- t_dur
            }

            # Identify limiting batch and overall shelf life
            all_stat_shelves <- sapply(batchRows, function(x) x$statShelf)
            min_shelf <- min(all_stat_shelves, na.rm = TRUE)
            min_batch_idx <- which(all_stat_shelves == min_shelf)
            limiting_batch_name <- names(batchRows)[min_batch_idx[1]]

            overall_stat_shelf <- min_shelf
            overall_ich_shelf  <- if (is.infinite(overall_stat_shelf)) extrapCap else min(overall_stat_shelf, extrapCap)

            return(list(
                batchRows = batchRows,
                overallStatShelf = overall_stat_shelf,
                overallIchShelf = overall_ich_shelf,
                limitingBatch = limiting_batch_name,
                extrapCap = extrapCap,
                extrapActive = isTRUE(overall_stat_shelf > extrapCap),
                t_dur = t_dur,
                calcBound = calcBound
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
                s_ich  <- if (is.infinite(s_stat)) extrapCap else min(s_stat, extrapCap)

                is_limit <- (b == limitingBatch && length(bRows) > 1)
                lim_text <- if (is_limit) .("Limiting batch") else .("Conforming")

                stat_display <- if (is.infinite(s_stat)) 999.0 else s_stat
                ich_display  <- if (is.infinite(s_ich)) extrapCap else s_ich
                batch_val    <- as.character(b)

                table$addRow(rowKey = b, values = list(
                    batch = batch_val,
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
            stat_disp <- if (is.infinite(ov_stat)) 999.0 else ov_stat
            ich_disp  <- if (is.infinite(ov_ich)) extrapCap else ov_ich
            lim_batch_val <- if (!is.null(limitingBatch)) as.character(limitingBatch) else ""

            lim_reason <- if (ov_stat > extrapCap) {
                paste0(.("Capped by ICH Q1E extrapolation limit"), " (", private$.formatNum(extrapCap, 1), " ", .("months"), ")")
            } else if (length(bRows) > 1) {
                paste0(.("Shortest batch"), ": ", lim_batch_val)
            } else {
                .("Statistical intersection")
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
            if (ov_stat > extrapCap) {
                table$setNote("extrap_active", jmvcore::format(.("<b>Extrapolation Cap Active</b>: Statistical intersection ({stat} months) exceeds the allowable ICH Q1E extrapolation limit ({cap} months). Permitted shelf life is capped at {cap} months."), stat = private$.formatNum(ov_stat, 1), cap = private$.formatNum(extrapCap, 1)))
            } else {
                table$setNote("extrap_active", NULL)
            }

            if (length(bRows) > 1 && !is.null(limitingBatch)) {
                table$setNote("limiting_batch_note", jmvcore::format(.("<b>Limiting Batch</b>: Overall shelf life is determined by {batch} ({months} months), which represents the earliest intersection among all batches."), batch = lim_batch_val, months = private$.formatNum(shelfRes$batchRows[[limitingBatch]]$statShelf, 1)))
            } else {
                table$setNote("limiting_batch_note", NULL)
            }
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
                clean_name <- gsub("Batch(?=(batch|серия|партия|lot))", "", t_name, ignore.case = TRUE, perl = TRUE)
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

            dat <- state$dat
            batchLevels <- state$batchLevels
            modelCode   <- state$selectedModelCode
            shelfRes    <- state$shelfResults
            specType    <- state$specType
            lsl         <- state$lsl
            usl         <- state$usl
            trans       <- state$trans
            depName     <- state$depName
            timeName    <- state$timeName
            batchName   <- state$batchName

            calcBound   <- shelfRes$calcBound
            ov_ich      <- shelfRes$overallIchShelf
            t_dur       <- shelfRes$t_dur

            # Determine plot time range: from 0 up to max(t_dur, ov_ich * 1.25, 36)
            t_max_plot <- max(t_dur * 1.25, if (!is.infinite(ov_ich)) ov_ich * 1.25 else 36, 12)
            t_grid <- seq(0, t_max_plot, length.out = 150)

            # Build prediction dataframe for lines and ribbons
            pred_list <- list()
            for (b in batchLevels) {
                if (modelCode == 1) {
                    sub_d <- dat[dat$Batch == b, , drop = FALSE]
                    fit_b <- stats::lm(Y_trans ~ Time, data = sub_d)
                    y_mean <- stats::predict(fit_b, newdata = data.frame(Time = t_grid))
                } else if (modelCode == 2) {
                    y_mean <- stats::predict(state$fit_cs, newdata = data.frame(Batch = factor(b, levels = batchLevels), Time = t_grid))
                } else {
                    y_mean <- stats::predict(state$fit_cl, newdata = data.frame(Time = t_grid))
                }

                # Confidence bounds
                if (specType == "lower") {
                    b_lower <- calcBound(b, t_grid, modelCode, "lower")
                    b_upper <- y_mean
                } else if (specType == "upper") {
                    b_lower <- y_mean
                    b_upper <- calcBound(b, t_grid, modelCode, "upper")
                } else {
                    bounds <- calcBound(b, t_grid, modelCode, "both")
                    b_lower <- bounds$lower
                    b_upper <- bounds$upper
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

            # Base ggplot
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = Time, y = Y, color = Batch)) +
                ggplot2::geom_point(size = 2.5, alpha = 0.85) +
                ggplot2::geom_line(data = pred_df, ggplot2::aes(x = Time, y = Fit, color = Batch), linewidth = 1.0) +
                ggplot2::labs(
                    title = paste0(.("Stability Profile & Shelf Life Estimation"), " (ICH Q1E)"),
                    subtitle = paste0(
                        .("Adopted:"), " ",
                        if (modelCode == 1) .("Separate slopes & intercepts") else if (modelCode == 2) .("Common slope & separate intercepts") else .("Common slope & intercept (Pooled)"),
                        "  |  ", .("Permitted Shelf Life:"), " ", private$.formatNum(ov_ich, 1), " ", .("months")
                    ),
                    x = timeName,
                    y = depName,
                    color = batchName,
                    fill = batchName
                )

            if (!is.null(ggtheme)) {
                p <- p + ggtheme
            } else {
                p <- p + ggplot2::theme_bw(base_size = 12)
            }

            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 14),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 11, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank()
            )

            # Add confidence ribbon
            if (specType == "lower") {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df,
                    ggplot2::aes(x = Time, ymin = Lower, ymax = Fit, fill = Batch),
                    alpha = 0.18, color = NA, inherit.aes = FALSE, show.legend = FALSE
                )
            } else if (specType == "upper") {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df,
                    ggplot2::aes(x = Time, ymin = Fit, ymax = Upper, fill = Batch),
                    alpha = 0.18, color = NA, inherit.aes = FALSE, show.legend = FALSE
                )
            } else {
                p <- p + ggplot2::geom_ribbon(
                    data = pred_df,
                    ggplot2::aes(x = Time, ymin = Lower, ymax = Upper, fill = Batch),
                    alpha = 0.18, color = NA, inherit.aes = FALSE, show.legend = FALSE
                )
            }

            # Specification limit horizontal lines
            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = lsl,
                    linetype = "dashed",
                    color = "#C0392B",
                    linewidth = 0.9
                ) + ggplot2::annotate(
                    "text", x = 1, y = lsl, label = paste0("LSL = ", private$.formatNum(lsl, 1)),
                    vjust = -0.5, hjust = 0, color = "#C0392B", fontface = "bold", size = 3.8
                )
            }

            if (specType %in% c("upper", "both") && !is.null(usl)) {
                p <- p + ggplot2::geom_hline(
                    yintercept = usl,
                    linetype = "dashed",
                    color = "#C0392B",
                    linewidth = 0.9
                ) + ggplot2::annotate(
                    "text", x = 1, y = usl, label = paste0("USL = ", private$.formatNum(usl, 1)),
                    vjust = 1.3, hjust = 0, color = "#C0392B", fontface = "bold", size = 3.8
                )
            }

            # Mark Observed study duration (Real-time data boundary)
            p <- p + ggplot2::geom_vline(
                xintercept = t_dur,
                linetype = "dotted",
                color = "#7F8C8D",
                linewidth = 0.8
            ) + ggplot2::annotate(
                "text", x = t_dur, y = Inf, label = paste0(.("Observed"), ": ", private$.formatNum(t_dur, 1), " ", .("months")),
                vjust = 1.5, hjust = 1.05, color = "#7F8C8D", size = 3.4
            )

            # Mark Permitted Shelf Life
            if (!is.infinite(ov_ich) && ov_ich > 0) {
                p <- p + ggplot2::geom_vline(
                    xintercept = ov_ich,
                    linetype = "longdash",
                    color = "#27AE60",
                    linewidth = 1.0
                ) + ggplot2::annotate(
                    "label", x = ov_ich, y = -Inf,
                    label = paste0(.("Shelf Life"), ": ", private$.formatNum(ov_ich, 1), " ", .("months")),
                    vjust = -0.4, hjust = 1.05,
                    fill = "white", color = "#27AE60", fontface = "bold", size = 3.6,
                    label.padding = ggplot2::unit(0.2, "lines"), label.r = ggplot2::unit(0.15, "lines")
                )
            }

            p <- p + ggplot2::theme(
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center"
            )

            print(p)
            TRUE
        },

        .shelfPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            batchLevels <- state$batchLevels
            shelfRes    <- state$shelfResults
            ov_ich      <- shelfRes$overallIchShelf
            t_dur       <- shelfRes$t_dur
            extrapCap   <- shelfRes$extrapCap
            batchName   <- state$batchName

            batchRows <- shelfRes$batchRows
            shelves <- sapply(batchLevels, function(b) {
                s <- batchRows[[b]]$statShelf
                if (is.null(s) || is.na(s)) 0 else s
            })

            is_lim <- sapply(batchLevels, function(b) {
                if (!is.null(shelfRes$limitingBatch) && b == shelfRes$limitingBatch) TRUE else FALSE
            })

            df_shelf <- data.frame(
                Batch = factor(batchLevels, levels = rev(batchLevels)),
                ShelfLife = as.numeric(shelves),
                Limiting = factor(ifelse(is_lim, .("Limiting batch"), .("Other batches")), levels = c(.("Limiting batch"), .("Other batches")))
            )

            p <- ggplot2::ggplot(df_shelf, ggplot2::aes(x = ShelfLife, y = Batch)) +
                ggplot2::geom_segment(ggplot2::aes(x = 0, xend = ShelfLife, y = Batch, yend = Batch), linewidth = 1.2, color = "#2C3E50") +
                ggplot2::geom_point(ggplot2::aes(color = Limiting), size = 4.5) +
                ggplot2::geom_text(
                    ggplot2::aes(label = paste0(private$.formatNum(ShelfLife, 1), " ", .("months"))),
                    hjust = -0.2, size = 3.5, fontface = "bold", color = "#2C3E50"
                )

            # Reference line: Observed duration
            p <- p + ggplot2::geom_vline(xintercept = t_dur, linetype = "dotted", color = "#7F8C8D", linewidth = 0.9) +
                ggplot2::annotate(
                    "text", x = t_dur, y = Inf, label = paste0(.("Observed"), ": ", private$.formatNum(t_dur, 1), " ", .("months")),
                    vjust = 1.5, hjust = 1.05, color = "#7F8C8D", size = 3.2
                )

            # Reference line: Extrapolation Cap
            if (!is.infinite(extrapCap) && extrapCap > 0) {
                p <- p + ggplot2::geom_vline(xintercept = extrapCap, linetype = "dashed", color = "#2980B9", linewidth = 0.9) +
                    ggplot2::annotate(
                        "text", x = extrapCap, y = Inf, label = paste0(.("Extrapolation Cap"), ": ", private$.formatNum(extrapCap, 1), " ", .("months")),
                        vjust = 1.5, hjust = -0.05, color = "#2980B9", size = 3.2
                    )
            }

            # Reference line: Permitted Shelf Life
            if (!is.infinite(ov_ich) && ov_ich > 0) {
                p <- p + ggplot2::geom_vline(xintercept = ov_ich, linetype = "solid", color = "#27AE60", linewidth = 1.1) +
                    ggplot2::annotate(
                        "text", x = ov_ich, y = -Inf, label = paste0(.("Permitted Shelf Life"), ": ", private$.formatNum(ov_ich, 1), " ", .("months")),
                        vjust = -0.8, hjust = -0.05, color = "#27AE60", fontface = "bold", size = 3.5
                    )
            }

            max_x <- max(c(df_shelf$ShelfLife, t_dur, if (!is.infinite(extrapCap)) extrapCap else 0, if (!is.infinite(ov_ich)) ov_ich else 0), na.rm = TRUE)
            p <- p + ggplot2::coord_cartesian(xlim = c(0, max_x * 1.25))

            p <- p + ggplot2::labs(
                title = paste0(.("Shelf Life Comparison & Extrapolation Horizon"), " (ICH Q1E)"),
                subtitle = paste0(
                    .("Permitted Shelf Life:"), " ", private$.formatNum(ov_ich, 1), " ", .("months"),
                    "  |  ",
                    if (isTRUE(shelfRes$extrapActive)) .("Capped by extrapolation rule") else paste0(.("Limiting Batch:"), " ", shelfRes$limitingBatch)
                ),
                x = .("Shelf Life (months)"),
                y = batchName,
                color = ""
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::scale_color_manual(
                name = "",
                values = stats::setNames(c("#C0392B", "#1B365D"), c(.("Limiting batch"), .("Other batches")))
            )
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 11, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank()
            )

            print(p)
            TRUE
        },

        .facetPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            dat <- state$dat
            batchLevels <- state$batchLevels
            modelCode   <- state$selectedModelCode
            shelfRes    <- state$shelfResults
            specType    <- state$specType
            lsl         <- state$lsl
            usl         <- state$usl
            trans       <- state$trans
            depName     <- state$depName
            timeName    <- state$timeName
            batchName   <- state$batchName

            calcBound   <- shelfRes$calcBound
            t_dur       <- shelfRes$t_dur
            ov_ich      <- shelfRes$overallIchShelf

            t_max_plot <- max(t_dur * 1.25, if (!is.infinite(ov_ich)) ov_ich * 1.25 else 36, 12)
            t_grid <- seq(0, t_max_plot, length.out = 150)

            pred_list <- list()
            for (b in batchLevels) {
                if (modelCode == 1) {
                    sub_d <- dat[dat$Batch == b, , drop = FALSE]
                    fit_b <- stats::lm(Y_trans ~ Time, data = sub_d)
                    y_mean <- stats::predict(fit_b, newdata = data.frame(Time = t_grid))
                } else if (modelCode == 2) {
                    y_mean <- stats::predict(state$fit_cs, newdata = data.frame(Batch = factor(b, levels = batchLevels), Time = t_grid))
                } else {
                    y_mean <- stats::predict(state$fit_cl, newdata = data.frame(Time = t_grid))
                }

                if (specType == "lower") {
                    b_lower <- calcBound(b, t_grid, modelCode, "lower")
                    b_upper <- y_mean
                } else if (specType == "upper") {
                    b_lower <- y_mean
                    b_upper <- calcBound(b, t_grid, modelCode, "upper")
                } else {
                    bounds <- calcBound(b, t_grid, modelCode, "both")
                    b_lower <- bounds$lower
                    b_upper <- bounds$upper
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

            p <- ggplot2::ggplot(dat, ggplot2::aes(x = Time, y = Y)) +
                ggplot2::geom_point(color = "#1B365D", size = 2, alpha = 0.8) +
                ggplot2::geom_line(data = pred_df, ggplot2::aes(x = Time, y = Fit), color = "#2980B9", linewidth = 0.9, inherit.aes = FALSE)

            if (specType == "lower") {
                p <- p + ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = Time, ymin = Lower, ymax = Fit), alpha = 0.2, fill = "#2980B9", inherit.aes = FALSE)
            } else if (specType == "upper") {
                p <- p + ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = Time, ymin = Fit, ymax = Upper), alpha = 0.2, fill = "#2980B9", inherit.aes = FALSE)
            } else {
                p <- p + ggplot2::geom_ribbon(data = pred_df, ggplot2::aes(x = Time, ymin = Lower, ymax = Upper), alpha = 0.2, fill = "#2980B9", inherit.aes = FALSE)
            }

            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                p <- p + ggplot2::geom_hline(yintercept = lsl, linetype = "dashed", color = "#C0392B", linewidth = 0.8)
            }
            if (specType %in% c("upper", "both") && !is.null(usl)) {
                p <- p + ggplot2::geom_hline(yintercept = usl, linetype = "dashed", color = "#C0392B", linewidth = 0.8)
            }

            batch_shelf_df <- data.frame(
                Batch = factor(batchLevels, levels = batchLevels),
                StatShelf = sapply(batchLevels, function(b) shelfRes$batchRows[[b]]$statShelf)
            )
            batch_shelf_df <- batch_shelf_df[!is.infinite(batch_shelf_df$StatShelf) & batch_shelf_df$StatShelf > 0, ]
            if (nrow(batch_shelf_df) > 0) {
                p <- p + ggplot2::geom_vline(data = batch_shelf_df, ggplot2::aes(xintercept = StatShelf), linetype = "longdash", color = "#27AE60", linewidth = 0.8, inherit.aes = FALSE)
            }

            p <- p + ggplot2::facet_wrap(~ Batch, labeller = ggplot2::label_both)

            p <- p + ggplot2::labs(
                title = paste0(.("Individual Batch Trajectories"), " (ICH Q1E)"),
                subtitle = paste0(
                    .("Adopted:"), " ",
                    if (modelCode == 1) .("Separate slopes & intercepts") else if (modelCode == 2) .("Common slope & separate intercepts") else .("Common slope & intercept (Pooled)")
                ),
                x = timeName,
                y = depName
            )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 11)
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 10, margin = ggplot2::margin(b = 8)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank()
            )

            print(p)
            TRUE
        },

        .ratePlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            dat <- state$dat
            batchLevels <- state$batchLevels
            alphaPool   <- state$alphaPool
            depName     <- state$depName
            timeName    <- state$timeName
            batchName   <- state$batchName

            # Calculate individual batch slopes with 95% CIs
            batch_rate_list <- list()
            for (b in batchLevels) {
                sub_d <- dat[dat$Batch == b, , drop = FALSE]
                fit_b <- stats::lm(Y_trans ~ Time, data = sub_d)
                sm_b  <- summary(fit_b)$coefficients
                b_slope <- sm_b["Time", "Estimate"]
                b_se    <- sm_b["Time", "Std. Error"]
                df_b    <- fit_b$df.residual
                tcrit_b <- if (df_b > 0) stats::qt(0.975, df = df_b) else 1.96
                batch_rate_list[[b]] <- data.frame(
                    Batch = b,
                    Slope = b_slope,
                    Lower = b_slope - tcrit_b * b_se,
                    Upper = b_slope + tcrit_b * b_se,
                    Type = .("Individual Batch"),
                    stringsAsFactors = FALSE
                )
            }
            df_rates <- do.call(rbind, batch_rate_list)

            # Common slope estimate from CS model
            fit_cs <- stats::lm(Y_trans ~ Batch + Time, data = dat)
            sm_cs  <- summary(fit_cs)$coefficients
            cs_slope <- sm_cs["Time", "Estimate"]
            cs_se    <- sm_cs["Time", "Std. Error"]
            df_cs    <- fit_cs$df.residual
            tcrit_cs <- if (df_cs > 0) stats::qt(0.975, df = df_cs) else 1.96

            pooled_row <- data.frame(
                Batch = .("Pooled Slope"),
                Slope = cs_slope,
                Lower = cs_slope - tcrit_cs * cs_se,
                Upper = cs_slope + tcrit_cs * cs_se,
                Type = .("Pooled Slope"),
                stringsAsFactors = FALSE
            )
            df_rates <- rbind(df_rates, pooled_row)
            df_rates$Batch <- factor(df_rates$Batch, levels = rev(c(batchLevels, .("Pooled Slope"))))
            df_rates$Type <- factor(df_rates$Type, levels = c(.("Individual Batch"), .("Pooled Slope")))

            # Equality of slopes F-test
            fit_sep <- stats::lm(Y_trans ~ Batch * Time, data = dat)
            an_slopes <- stats::anova(fit_cs, fit_sep)
            f_slope <- an_slopes$F[2]
            p_slope <- an_slopes[["Pr(>F)"]][2]

            p <- ggplot2::ggplot(df_rates, ggplot2::aes(x = Slope, y = Batch, color = Type)) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#7F8C8D") +
                ggplot2::geom_vline(xintercept = cs_slope, linetype = "dotted", color = "#2980B9", linewidth = 0.8) +
                ggplot2::geom_errorbar(ggplot2::aes(xmin = Lower, xmax = Upper), width = 0.25, linewidth = 0.9) +
                ggplot2::geom_point(size = 3.5) +
                ggplot2::labs(
                    title = paste0(.("Batch Degradation Rates & ANCOVA Slope Comparison"), " (ICH Q1E)"),
                    subtitle = paste0(
                        .("Equality of Slopes:"), " F = ", private$.formatNum(f_slope, 2),
                        ", p = ", private$.formatNum(p_slope, 4),
                        " (", if (!is.na(p_slope) && p_slope >= alphaPool) .("Slopes are poolable") else .("Slopes differ significantly"), ")"
                    ),
                    x = paste0(.("Degradation Rate"), " (", depName, " / ", timeName, ")"),
                    y = batchName,
                    color = ""
                )

            if (!is.null(ggtheme)) p <- p + ggtheme else p <- p + ggplot2::theme_bw(base_size = 12)
            p <- p + ggplot2::scale_color_manual(
                name = "",
                values = stats::setNames(c("#1B365D", "#27AE60"), c(.("Individual Batch"), .("Pooled Slope")))
            )
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", color = "#1B365D", size = 13),
                plot.subtitle = ggplot2::element_text(color = "#2C3E50", size = 11, margin = ggplot2::margin(b = 10)),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.box = "horizontal",
                legend.justification = "center",
                panel.grid.minor = ggplot2::element_blank()
            )

            print(p)
            TRUE
        },

        .predPlot = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            dat <- state$dat
            batchLevels <- state$batchLevels
            modelCode   <- state$selectedModelCode
            shelfRes    <- state$shelfResults
            specType    <- state$specType
            lsl         <- state$lsl
            usl         <- state$usl
            trans       <- state$trans
            depName     <- state$depName
            timeName    <- state$timeName
            batchName   <- state$batchName
            confLevel   <- state$confLevel

            ov_ich <- shelfRes$overallIchShelf
            t_dur  <- shelfRes$t_dur
            b_lim  <- if (!is.null(shelfRes$limitingBatch)) shelfRes$limitingBatch else batchLevels[1]

            t_max_plot <- max(t_dur * 1.25, if (!is.infinite(ov_ich)) ov_ich * 1.25 else 36, 12)
            t_grid <- seq(0, t_max_plot, length.out = 150)

            # Fit for limiting batch
            if (modelCode == 1) {
                fit_lim <- stats::lm(Y_trans ~ Time, data = dat[dat$Batch == b_lim, , drop = FALSE])
                pred_lim <- stats::predict(fit_lim, newdata = data.frame(Time = t_grid), se.fit = TRUE)
                mse_lim <- summary(fit_lim)$sigma^2
                df_lim  <- fit_lim$df.residual
                slope_lim <- stats::coef(fit_lim)["Time"]
                pred_star <- stats::predict(fit_lim, newdata = data.frame(Time = if (!is.infinite(ov_ich) && ov_ich > 0) ov_ich else t_dur), se.fit = TRUE)
            } else if (modelCode == 2) {
                pred_lim <- stats::predict(state$fit_cs, newdata = data.frame(Batch = factor(b_lim, levels = batchLevels), Time = t_grid), se.fit = TRUE)
                mse_lim <- summary(state$fit_cs)$sigma^2
                df_lim  <- state$fit_cs$df.residual
                slope_lim <- stats::coef(state$fit_cs)["Time"]
                pred_star <- stats::predict(state$fit_cs, newdata = data.frame(Batch = factor(b_lim, levels = batchLevels), Time = if (!is.infinite(ov_ich) && ov_ich > 0) ov_ich else t_dur), se.fit = TRUE)
            } else {
                pred_lim <- stats::predict(state$fit_cl, newdata = data.frame(Time = t_grid), se.fit = TRUE)
                mse_lim <- summary(state$fit_cl)$sigma^2
                df_lim  <- state$fit_cl$df.residual
                slope_lim <- stats::coef(state$fit_cl)["Time"]
                pred_star <- stats::predict(state$fit_cl, newdata = data.frame(Time = if (!is.infinite(ov_ich) && ov_ich > 0) ov_ich else t_dur), se.fit = TRUE)
            }

            tcrit_ci <- if (df_lim > 0) stats::qt(confLevel, df = df_lim) else 1.645
            y_fit   <- pred_lim$fit
            se_mean <- pred_lim$se.fit
            se_pred <- sqrt(mse_lim + se_mean^2)

            ci_low <- y_fit - tcrit_ci * se_mean
            ci_upp <- y_fit + tcrit_ci * se_mean
            pi_low <- y_fit - tcrit_ci * se_pred
            pi_upp <- y_fit + tcrit_ci * se_pred

            if (trans == "log") {
                y_fit  <- exp(y_fit)
                ci_low <- exp(ci_low)
                ci_upp <- exp(ci_upp)
                pi_low <- exp(pi_low)
                pi_upp <- exp(pi_upp)
            }

            df_env <- data.frame(
                Time = t_grid,
                Fit = y_fit,
                CI_Low = ci_low,
                CI_Upp = ci_upp,
                PI_Low = pi_low,
                PI_Upp = pi_upp
            )

            # Calculate SRL (Release Limit)
            t_star <- if (!is.infinite(ov_ich) && ov_ich > 0) ov_ich else t_dur
            se_pred_star <- sqrt(mse_lim + pred_star$se.fit^2)
            margin_pi <- tcrit_ci * se_pred_star
            srl_val <- NULL
            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                srl_val <- lsl - slope_lim * t_star + margin_pi
            } else if (specType == "upper" && !is.null(usl)) {
                srl_val <- usl - slope_lim * t_star - margin_pi
            }

            p <- ggplot2::ggplot(dat, ggplot2::aes(x = Time, y = Y)) +
                ggplot2::geom_point(ggplot2::aes(color = Batch), size = 2.2, alpha = 0.7)

            # Prediction interval ribbon (wider, outer)
            if (specType == "lower") {
                p <- p + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = PI_Low, ymax = Fit),
                    fill = "#85C1E9", alpha = 0.25, inherit.aes = FALSE
                ) + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = CI_Low, ymax = Fit),
                    fill = "#2980B9", alpha = 0.35, inherit.aes = FALSE
                )
            } else if (specType == "upper") {
                p <- p + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = Fit, ymax = PI_Upp),
                    fill = "#85C1E9", alpha = 0.25, inherit.aes = FALSE
                ) + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = Fit, ymax = CI_Upp),
                    fill = "#2980B9", alpha = 0.35, inherit.aes = FALSE
                )
            } else {
                p <- p + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = PI_Low, ymax = PI_Upp),
                    fill = "#85C1E9", alpha = 0.25, inherit.aes = FALSE
                ) + ggplot2::geom_ribbon(
                    data = df_env, ggplot2::aes(x = Time, ymin = CI_Low, ymax = CI_Upp),
                    fill = "#2980B9", alpha = 0.35, inherit.aes = FALSE
                )
            }

            p <- p + ggplot2::geom_line(data = df_env, ggplot2::aes(x = Time, y = Fit), color = "#1B365D", linewidth = 1.0)

            # Specification Limit Line
            if (specType %in% c("lower", "both") && !is.null(lsl)) {
                p <- p + ggplot2::geom_hline(yintercept = lsl, linetype = "dashed", color = "#C0392B", linewidth = 0.9) +
                    ggplot2::annotate("text", x = 1, y = lsl, label = paste0("LSL = ", private$.formatNum(lsl, 1)), vjust = -0.5, hjust = 0, color = "#C0392B", fontface = "bold", size = 3.6)
            }
            if (specType %in% c("upper", "both") && !is.null(usl)) {
                p <- p + ggplot2::geom_hline(yintercept = usl, linetype = "dashed", color = "#C0392B", linewidth = 0.9) +
                    ggplot2::annotate("text", x = 1, y = usl, label = paste0("USL = ", private$.formatNum(usl, 1)), vjust = 1.3, hjust = 0, color = "#C0392B", fontface = "bold", size = 3.6)
            }

            # SRL horizontal marker from t=0 to t*
            if (!is.null(srl_val) && is.finite(srl_val)) {
                p <- p + ggplot2::geom_segment(
                    ggplot2::aes(x = 0, xend = t_star, y = srl_val, yend = srl_val),
                    linetype = "dotdash", color = "#8E44AD", linewidth = 0.9, inherit.aes = FALSE
                ) + ggplot2::annotate(
                    "text", x = 0.5, y = srl_val, label = paste0(.("Release Limit (SRL)"), " = ", private$.formatNum(srl_val, 1)),
                    vjust = -0.5, hjust = 0, color = "#8E44AD", fontface = "bold", size = 3.6
                )
            }

            # Shelf life vertical line
            if (!is.infinite(ov_ich) && ov_ich > 0) {
                p <- p + ggplot2::geom_vline(xintercept = ov_ich, linetype = "longdash", color = "#27AE60", linewidth = 1.0) +
                    ggplot2::annotate(
                        "label", x = ov_ich, y = -Inf,
                        label = paste0(.("Shelf Life"), ": ", private$.formatNum(ov_ich, 1), " ", .("months")),
                        vjust = -0.4, hjust = 1.05,
                        fill = "white", color = "#27AE60", fontface = "bold", size = 3.6,
                        label.padding = ggplot2::unit(0.2, "lines"), label.r = ggplot2::unit(0.15, "lines")
                    )
            }

            p <- p + ggplot2::labs(
                title = paste0(.("Prediction Intervals & Shelf-Life Release Limits (SRL)"), " (ICH Q1E)"),
                subtitle = paste0(
                    .("Limiting Batch:"), " ", b_lim,
                    "  |  ", .("Permitted Shelf Life:"), " ", private$.formatNum(ov_ich, 1), " ", .("months"),
                    if (!is.null(srl_val) && is.finite(srl_val)) paste0("  |  ", .("Calculated SRL:"), " ", private$.formatNum(srl_val, 1)) else ""
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
            if (is.null(state) || is.null(state$fit)) return(FALSE)

            fit <- state$fit
            dat <- state$dat
            timeName <- if (!is.null(state$timeName)) state$timeName else "Time"

            res <- stats::residuals(fit)
            fitted_vals <- stats::fitted(fit)
            std_res <- stats::rstandard(fit)
            sqrt_abs_std_res <- sqrt(abs(std_res))

            df_diag <- data.frame(
                Fitted = fitted_vals,
                Residuals = res,
                StdResiduals = std_res,
                SqrtAbsStdRes = sqrt_abs_std_res,
                Time = if (!is.null(dat) && "Time" %in% names(dat)) dat$Time else seq_along(res),
                Batch = if (!is.null(dat) && "Batch" %in% names(dat)) dat$Batch else factor(rep("1", length(res)))
            )

            use_theme <- if (!is.null(ggtheme)) ggtheme else ggplot2::theme_bw(base_size = 10)

            apply_theme <- function(p_sub, show_legend = FALSE) {
                p_sub <- p_sub + use_theme + ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 11, color = "#1B365D"),
                    legend.position = if (show_legend) "bottom" else "none",
                    legend.direction = "horizontal",
                    legend.box = "horizontal",
                    legend.justification = "center"
                )
                p_sub
            }

            # 1. Residuals vs Fitted
            p1 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = Fitted, y = Residuals)) +
                ggplot2::geom_point(color = "#2C3E50", size = 2, alpha = 0.8) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B") +
                ggplot2::geom_smooth(method = "loess", formula = y ~ x, color = "#2980B9", se = FALSE, linewidth = 0.8) +
                ggplot2::labs(title = .("Residuals vs Fitted"), x = .("Fitted values"), y = .("Residuals"))
            p1 <- apply_theme(p1)

            # 2. Normal Q-Q
            p2 <- ggplot2::ggplot(df_diag, ggplot2::aes(sample = StdResiduals)) +
                ggplot2::stat_qq(color = "#2C3E50", size = 2, alpha = 0.8) +
                ggplot2::stat_qq_line(color = "#C0392B", linetype = "dashed", linewidth = 0.8) +
                ggplot2::labs(title = .("Normal Q-Q"), x = .("Theoretical Quantiles"), y = .("Standardized Residuals"))
            p2 <- apply_theme(p2)

            # 3. Residuals vs Time
            batchName <- if (!is.null(state$batchName)) state$batchName else .("Batch")
            p3 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = Time, y = Residuals, color = Batch)) +
                ggplot2::geom_point(size = 2, alpha = 0.8) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#C0392B") +
                ggplot2::labs(title = .("Residuals vs Time"), x = timeName, y = .("Residuals"), color = batchName)
            p3 <- apply_theme(p3, show_legend = TRUE)

            # 4. Scale-Location
            p4 <- ggplot2::ggplot(df_diag, ggplot2::aes(x = Fitted, y = SqrtAbsStdRes)) +
                ggplot2::geom_point(color = "#2C3E50", size = 2, alpha = 0.8) +
                ggplot2::geom_smooth(method = "loess", formula = y ~ x, color = "#2980B9", se = FALSE, linewidth = 0.8) +
                ggplot2::labs(title = .("Scale-Location"), x = .("Fitted values"), y = expression(sqrt("|Standardized Residuals|")))
            p4 <- apply_theme(p4)

            # Multiplot layout
            grid::grid.newpage()
            grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
            print(p1, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
            print(p2, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
            print(p3, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
            print(p4, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
            TRUE
        }
    )
)
