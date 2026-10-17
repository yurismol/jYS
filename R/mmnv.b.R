# This file is a generated template, your changes will not be overwritten

mMNVClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMNVClass",
    inherit = mMNVBase,
    private = list(
        .state = list(),

        .init = function() {
            # Provide HTML instructions explaining MANOVA approach vs univariate ANOVA
            instructions <- self$results$instructions
            instructions$setContent(paste0(
                "<h3>", .("MANOVA & Repeated Measures: Methodological Guide"), "</h3>",
                "<p>",
                .("The Multivariate Analysis of Variance (MANOVA) approach to repeated measures designs"),
                " (O'Brien & Kaiser, 1985) ",
                .("treats repeated measurements as a multivariate vector of dependent variables. Unlike classical univariate RM-ANOVA, the MANOVA approach makes"),
                " <b>", .("NO assumption of sphericity"), "</b> ",
                .("(compound symmetry) on the error variance-covariance matrix."),
                "</p>",
                "<ul>",
                "<li><b>", .("When to prefer MANOVA:"), "</b> ",
                .("When sample size per group exceeds the number of repeated measures by at least 10 to 15 (N - g >= k + 10) and the sphericity assumption is violated (Mauchly's p < .05, epsilon < .75)."),
                "</li>",
                "<li><b>", .("When to prefer Univariate ANOVA:"), "</b> ",
                .("When sample size is very small (N < k), where MANOVA cannot be computed because error degrees of freedom are insufficient."),
                "</li>",
                "<li><b>", .("Multivariate Test Criteria:"), "</b> ",
                .("Pillai's Trace is the most robust statistic against violations of homogeneity of covariance matrices. Wilks' Lambda is the standard likelihood-ratio criterion reported in psychological and medical literature."),
                "</li>",
                "</ul>"
            ))

            private$.initNotes()
        },

        .initNotes = function() {
            # 1. multivariateTable notes
            mvTable <- self$results$multivariateTable
            mvTable$setNote(
                "pillai_note",
                .("<b>Pillai's Trace</b>: Most robust statistic against departures from multivariate normality and homogeneity of covariance matrices (Olson, 1974; O'Brien & Kaiser, 1985). Recommended as primary test criterion.")
            )
            mvTable$setNote(
                "wilks_note",
                .("<b>Wilks' Lambda</b>: Classic likelihood-ratio criterion (U-statistic). Values closer to 0 represent stronger multivariate effects.")
            )
            mvTable$setNote(
                "hl_note",
                .("<b>Hotelling-Lawley Trace</b>: Sum of eigenvalues of HE^-1. Directly reflects multidimensional variance explained by hypothesis relative to error.")
            )
            mvTable$setNote(
                "roy_note",
                .("<b>Roy's Largest Root</b>: Based on the maximum eigenvalue. Most powerful when the effect is concentrated along a single dimension, but sensitive to departures from assumptions.")
            )
            mvTable$setNote(
                "f_approx",
                .("<b>F-Approximation</b>: Multidimensional test criteria are converted to exact or approximate F-statistics using Rao's or Pillai's approximations.")
            )

            # 2. univariateTable notes
            uniTable <- self$results$univariateTable
            uniTable$setNote(
                "uni_intro",
                .("<b>Univariate Repeated Measures ANOVA</b>: Tests within-subjects effects assuming sphericity. When Mauchly's test is violated (p < .05), uncorrected F-tests inflate Type I error; epsilon-adjusted degrees of freedom and p-values must be used instead.")
            )
            uniTable$setNote(
                "gg_note",
                .("<b>Greenhouse-Geisser (ε_GG)</b>: Conservative epsilon adjustment applied to degrees of freedom when sphericity is violated (Greenhouse & Geisser, 1959). Recommended when true epsilon < 0.75.")
            )
            uniTable$setNote(
                "hf_note",
                .("<b>Huynh-Feldt (ε_HF)</b>: Less conservative epsilon adjustment suitable when sample size is moderate and true epsilon >= 0.75 (Huynh & Feldt, 1976).")
            )
            uniTable$setNote(
                "manova_pref",
                .("<b>Methodological Recommendation</b>: In repeated measures designs with sphericity violations (p < .05, ε < .75), the MANOVA approach (Multivariate Table) is preferred over uncorrected Univariate ANOVA (O'Brien & Kaiser, 1985).")
            )

            # 3. sphericityTable notes
            sphTable <- self$results$sphericityTable
            sphTable$setNote(
                "mauchly_rule",
                .("<b>Mauchly's Sphericity Test</b>: Tests H0 that the covariance matrix of orthonormal contrasts has equal variances (compound symmetry). A significant result (p < .05) indicates violation of sphericity.")
            )
            sphTable$setNote(
                "eps_bounds",
                .("<b>Epsilon Bounds</b>: Epsilon ranges from 1 / (k - 1) (lower bound, severe violation) to 1.0 (perfect sphericity). Values below 0.75 strongly favor the multivariate MANOVA approach.")
            )

            # 4. profileTable notes
            profTable <- self$results$profileTable
            profTable$setNote(
                "step1_parallel",
                .("<b>1. Parallelism (Group × Time Interaction)</b>: Tests whether response trajectories over time are parallel across groups. If rejected (p < .05), groups change differently over time, demonstrating differential treatment efficacy.")
            )
            profTable$setNote(
                "step2_levels",
                .("<b>2. Equal Levels (Coincidence / Between-Group Main Effect)</b>: Tests whether overall mean responses across all time points are identical across groups (between-subjects main effect).")
            )
            profTable$setNote(
                "step3_flatness",
                .("<b>3. Flatness (Within-Subject Change over Time)</b>: Tests whether mean response levels remain constant across time points regardless of group (within-subjects main effect).")
            )

            # 5. contrastsTable notes
            cntTable <- self$results$contrastsTable
            cntTable$setNote(
                "poly_rule",
                .("<b>Orthogonal Polynomial Contrasts</b>: Decompose the within-subject factor into orthogonal trends: Linear (constant rate of change), Quadratic (curvature/plateau), and Cubic (inflection points).")
            )
            cntTable$setNote(
                "trend_interp",
                .("<b>Trend Interpretation</b>: Evaluates the functional mathematical trajectory of the response across repeated measurement intervals.")
            )

            # 6. assumptionsTable notes
            assTable <- self$results$assumptionsTable
            assTable$setNote(
                "boxm_rule",
                .("<b>Box's M Test</b>: Tests equality of covariance matrices across between-subjects groups using Box's exact F-approximation (Box, 1949). Highly sensitive to departures from normality; alpha = .001 is conventionally used.")
            )
            assTable$setNote(
                "mardia_rule",
                .("<b>Mardia's Multivariate Normality</b>: Computes multivariate skewness (b1,p; asymptotic chi-square) and multivariate kurtosis (b2,p; asymptotic standard normal z) (Mardia, 1970). Values p >= .05 indicate conformity with multivariate normality.")
            )
            assTable$setNote(
                "mahal_rule",
                .("<b>Multivariate Outliers (Mahalanobis D²)</b>: Evaluates squared distance from group centroid against the chi-square distribution (df = p, alpha = .001). Observations exceeding critical threshold warrant inspection.")
            )

            # 7. meansTable notes
            mnsTable <- self$results$meansTable
            mnsTable$setNote(
                "means_rule",
                .("<b>Marginal Means & Error Bounds</b>: Displays estimated marginal means, standard errors (SE), standard deviations (SD), and 95% confidence intervals for each factor combination across repeated measures cells.")
            )
        },

        .run = function() {
            dat <- self$data
            mode <- self$options$mode

            if (mode == "rm") {
                private$.runRepeatedMeasures(dat)
            } else {
                private$.runFactorialManova(dat)
            }
        },

        .runRepeatedMeasures = function(dat) {
            rmCells <- self$options$rmCells
            if (is.null(rmCells) || length(rmCells) < 2) {
                return()
            }

            k <- length(rmCells)
            rmFactorName <- self$options$rmFactorName
            if (is.null(rmFactorName) || rmFactorName == "") {
                rmFactorName <- "Time"
            }

            between <- self$options$between
            covs <- self$options$covs

            # Selected columns
            all_cols <- c(rmCells, between, covs)
            dat_clean <- stats::na.omit(dat[, all_cols, drop = FALSE])
            n_obs <- nrow(dat_clean)

            if (n_obs < 3) {
                jmvcore::reject(.("Too few complete observations (minimum 3 required)."))
                return()
            }

            # Check numeric type for rmCells and covs
            for (col in rmCells) {
                dat_clean[[col]] <- jmvcore::toNumeric(dat_clean[[col]])
            }
            if (!is.null(covs) && length(covs) > 0) {
                for (col in covs) {
                    dat_clean[[col]] <- jmvcore::toNumeric(dat_clean[[col]])
                }
            }
            if (!is.null(between) && length(between) > 0) {
                for (col in between) {
                    dat_clean[[col]] <- as.factor(dat_clean[[col]])
                    stats::contrasts(dat_clean[[col]]) <- stats::contr.sum(nlevels(dat_clean[[col]]))
                }
            }

            # Construct formula
            rhs_terms <- c()
            if (!is.null(between) && length(between) > 0) {
                rhs_terms <- c(rhs_terms, paste(between, collapse = " * "))
            }
            if (!is.null(covs) && length(covs) > 0) {
                rhs_terms <- c(rhs_terms, paste(covs, collapse = " + "))
            }

            rhs_formula <- if (length(rhs_terms) > 0) paste(rhs_terms, collapse = " + ") else "1"
            lhs_formula <- paste0("cbind(", paste(paste0("`", rmCells, "`"), collapse = ", "), ")")
            full_formula <- stats::as.formula(paste(lhs_formula, "~", rhs_formula))

            fit <- stats::lm(full_formula, data = dat_clean)

            # Within-subjects factor specification
            idata <- data.frame(setNames(list(factor(1:k, labels = rmCells)), rmFactorName))
            idesign <- stats::as.formula(paste0("~ `", rmFactorName, "`"))

            # Set contrast scheme
            contrastType <- self$options$contrastType
            if (contrastType == "polynomial") {
                stats::contrasts(idata[[rmFactorName]]) <- stats::contr.poly(k)
            } else if (contrastType == "helmert") {
                stats::contrasts(idata[[rmFactorName]]) <- stats::contr.helmert(k)
            } else if (contrastType == "treatment") {
                stats::contrasts(idata[[rmFactorName]]) <- stats::contr.treatment(k)
            } else {
                stats::contrasts(idata[[rmFactorName]]) <- stats::contr.poly(k)
            }

            # Call car::Anova
            av <- tryCatch(
                car::Anova(fit, idata = idata, idesign = idesign, type = "III"),
                error = function(e) {
                    jmvcore::reject(paste0(.("Error evaluating MANOVA model"), ": ", e$message))
                }
            )

            av_sum <- summary(av, multivariate = TRUE)

            # 1. Fill Multivariate Table
            private$.fillMultivariateTable(av_sum, is_rm = TRUE, rmFactorName = rmFactorName)

            # 2. Fill Univariate Table
            if (self$options$showUnivariate) {
                private$.fillUnivariateTable(av_sum)
            }

            # 3. Fill Sphericity Table
            if (self$options$showSphericity) {
                private$.fillSphericityTable(av_sum, k = k)
            }

            # 4. Fill Profile Analysis Table
            if (self$options$showProfileAnalysis) {
                private$.fillProfileTable(av_sum, between = between, rmFactorName = rmFactorName)
            }

            # 5. Fill Contrasts Table
            if (self$options$showContrastsTable) {
                private$.fillContrastsTable(dat_clean, rmCells, rmFactorName, between)
            }

            # 6. Fill Assumptions Table
            if (self$options$showAssumptionChecks) {
                private$.fillAssumptionsTable(dat_clean, rmCells, between)
            }

            # 7. Fill Means Table
            if (self$options$showMeansTable) {
                private$.fillMeansTable(dat_clean, rmCells, between)
            }

            # Save state for plotting
            private$.state <- list(
                dat_clean = dat_clean,
                rmCells = rmCells,
                rmFactorName = rmFactorName,
                between = between,
                k = k
            )
        },

        .runFactorialManova = function(dat) {
            deps <- self$options$deps
            if (is.null(deps) || length(deps) < 2) {
                return()
            }

            between <- self$options$between
            covs <- self$options$covs

            if ((is.null(between) || length(between) == 0) && (is.null(covs) || length(covs) == 0)) {
                return()
            }

            all_cols <- c(deps, between, covs)
            dat_clean <- stats::na.omit(dat[, all_cols, drop = FALSE])
            n_obs <- nrow(dat_clean)

            if (n_obs < 4) {
                jmvcore::reject(.("Too few complete observations (minimum 4 required)."))
                return()
            }

            for (col in deps) {
                dat_clean[[col]] <- jmvcore::toNumeric(dat_clean[[col]])
            }
            if (!is.null(covs) && length(covs) > 0) {
                for (col in covs) {
                    dat_clean[[col]] <- jmvcore::toNumeric(dat_clean[[col]])
                }
            }
            if (!is.null(between) && length(between) > 0) {
                for (col in between) {
                    dat_clean[[col]] <- as.factor(dat_clean[[col]])
                    stats::contrasts(dat_clean[[col]]) <- stats::contr.sum(nlevels(dat_clean[[col]]))
                }
            }

            rhs_terms <- c()
            if (!is.null(between) && length(between) > 0) {
                rhs_terms <- c(rhs_terms, paste(between, collapse = " * "))
            }
            if (!is.null(covs) && length(covs) > 0) {
                rhs_terms <- c(rhs_terms, paste(covs, collapse = " + "))
            }

            rhs_formula <- paste(rhs_terms, collapse = " + ")
            lhs_formula <- paste0("cbind(", paste(paste0("`", deps, "`"), collapse = ", "), ")")
            full_formula <- stats::as.formula(paste(lhs_formula, "~", rhs_formula))

            fit <- stats::lm(full_formula, data = dat_clean)
            av <- car::Anova(fit, type = "III")
            av_sum <- summary(av)

            private$.fillMultivariateTable(av_sum, is_rm = FALSE)

            if (self$options$showAssumptionChecks) {
                private$.fillAssumptionsTable(dat_clean, deps, between)
            }

            if (self$options$showMeansTable) {
                private$.fillMeansTable(dat_clean, deps, between)
            }

            private$.state <- list(
                dat_clean = dat_clean,
                deps = deps,
                between = between,
                p = length(deps)
            )
        },

        .fillMultivariateTable = function(av_sum, is_rm = TRUE, rmFactorName = "Time") {
            table <- self$results$multivariateTable
            table$deleteRows()

            m_tests <- av_sum$multivariate.tests

            if (is.null(m_tests) || length(m_tests) == 0) return()

            test_configs <- list(
                list(id = "Pillai", name = .("Pillai's Trace"), opt = self$options$testPillai, fn = car:::Pillai),
                list(id = "Wilks", name = .("Wilks' Lambda"), opt = self$options$testWilks, fn = car:::Wilks),
                list(id = "Hotelling-Lawley", name = .("Hotelling-Lawley Trace"), opt = self$options$testHotelling, fn = car:::HL),
                list(id = "Roy", name = .("Roy's Largest Root"), opt = self$options$testRoy, fn = car:::Roy)
            )

            term_names <- names(m_tests)
            for (term in term_names) {
                x <- m_tests[[term]]
                if (is.null(x$SSPH) || is.null(x$SSPE)) next
                if (!is.null(x$singular) && x$singular) next

                SSPE_qr <- qr(x$SSPE)
                eigs <- tryCatch(
                    Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values),
                    error = function(e) NULL
                )
                if (is.null(eigs)) next

                clean_term <- gsub("`", "", term)

                for (cfg in test_configs) {
                    if (!cfg$opt) next

                    stat_res <- tryCatch(
                        cfg$fn(eigs, x$df, x$df.residual),
                        error = function(e) NULL
                    )
                    if (is.null(stat_res)) next

                    stat_val <- as.numeric(stat_res[1])
                    f_val    <- as.numeric(stat_res[2])
                    df1_val  <- as.integer(round(stat_res[3]))
                    df2_val  <- as.numeric(stat_res[4])
                    p_val    <- stats::pf(f_val, df1_val, df2_val, lower.tail = FALSE)

                    eta_p <- if (!is.na(f_val) && !is.na(df1_val) && !is.na(df2_val) && (f_val * df1_val + df2_val) > 0) {
                        (f_val * df1_val) / (f_val * df1_val + df2_val)
                    } else {
                        NA
                    }

                    table$addRow(rowKey = paste(clean_term, cfg$id, sep = "_"), values = list(
                        term = clean_term,
                        test = cfg$name,
                        stat = stat_val,
                        f = f_val,
                        df1 = df1_val,
                        df2 = df2_val,
                        p = p_val,
                        etaSqP = eta_p
                    ))
                }
            }

            if (is_rm && table$rowCount > 0) {
                table$addFootnote(
                    rowNo = 1,
                    col = "test",
                    note = .("Multivariate tests make no sphericity assumption on the covariance matrix (O'Brien & Kaiser, 1985). Pillai's trace is robust to heterogeneity of covariance matrices.")
                )
            }
        },

        .fillUnivariateTable = function(av_sum) {
            table <- self$results$univariateTable
            table$deleteRows()

            univ <- av_sum$univariate.tests
            adjust <- av_sum$pval.adjustments

            if (is.null(univ) || !is.matrix(univ)) return()

            univ_terms <- rownames(univ)
            for (term in univ_terms) {
                clean_term <- gsub("`", "", term)
                ss_val <- as.numeric(univ[term, "Sum Sq"])
                df_val <- as.numeric(univ[term, "num Df"])
                error_ss <- as.numeric(univ[term, "Error SS"])
                den_df <- as.numeric(univ[term, "den Df"])
                f_val <- as.numeric(univ[term, "F value"])
                p_val <- as.numeric(univ[term, "Pr(>F)"])

                ms_val <- if (!is.na(ss_val) && df_val > 0) ss_val / df_val else NA
                eta_p <- if (!is.na(ss_val) && !is.na(error_ss) && (ss_val + error_ss) > 0) {
                    ss_val / (ss_val + error_ss)
                } else {
                    NA
                }

                # Row 1: Sphericity Assumed
                table$addRow(rowKey = paste(clean_term, "sphericity", sep = "_"), values = list(
                    term = clean_term,
                    correction = .("Sphericity Assumed"),
                    ss = ss_val,
                    df = df_val,
                    ms = ms_val,
                    f = f_val,
                    p = p_val,
                    etaSqP = eta_p
                ))

                # Check if epsilon adjustments exist for this term
                if (!is.null(adjust) && term %in% rownames(adjust)) {
                    gg_eps <- as.numeric(adjust[term, "GG eps"])
                    gg_p   <- as.numeric(adjust[term, "Pr(>F[GG])"])
                    hf_eps <- as.numeric(adjust[term, "HF eps"])
                    hf_p   <- as.numeric(adjust[term, "Pr(>F[HF])"])

                    if (!is.na(gg_eps)) {
                        table$addRow(rowKey = paste(clean_term, "gg", sep = "_"), values = list(
                            term = clean_term,
                            correction = .("Greenhouse-Geisser"),
                            ss = ss_val,
                            df = df_val * gg_eps,
                            ms = ss_val / (df_val * gg_eps),
                            f = f_val,
                            p = gg_p,
                            etaSqP = eta_p
                        ))
                    }

                    if (!is.na(hf_eps)) {
                        # Huynh-Feldt can exceed 1.0; standard cap is 1.0
                        hf_eps_capped <- min(1.0, hf_eps)
                        table$addRow(rowKey = paste(clean_term, "hf", sep = "_"), values = list(
                            term = clean_term,
                            correction = .("Huynh-Feldt"),
                            ss = ss_val,
                            df = df_val * hf_eps_capped,
                            ms = ss_val / (df_val * hf_eps_capped),
                            f = f_val,
                            p = hf_p,
                            etaSqP = eta_p
                        ))
                    }
                }
            }
        },

        .fillSphericityTable = function(av_sum, k) {
            table <- self$results$sphericityTable
            table$deleteRows()

            sph <- av_sum$sphericity.tests
            adjust <- av_sum$pval.adjustments

            if (is.null(sph) || !is.matrix(sph)) return()

            terms <- rownames(sph)
            for (term in terms) {
                clean_term <- gsub("`", "", term)
                w_val <- if ("Test statistic" %in% colnames(sph)) as.numeric(sph[term, "Test statistic"]) else as.numeric(sph[term, 1])
                p_val <- if ("p-value" %in% colnames(sph)) as.numeric(sph[term, "p-value"]) else as.numeric(sph[term, 2])

                gg_eps <- NA
                hf_eps <- NA
                if (!is.null(adjust) && term %in% rownames(adjust)) {
                    gg_eps <- as.numeric(adjust[term, "GG eps"])
                    hf_eps <- as.numeric(adjust[term, "HF eps"])
                }

                lower_eps <- 1 / (k - 1)
                df_sph <- (k * (k - 1) / 2) - 1

                table$addRow(rowKey = clean_term, values = list(
                    term = clean_term,
                    mauchlyW = w_val,
                    chisq = if (!is.na(w_val) && w_val > 0) -log(w_val) * (nrow(self$data) - 1) else NA,
                    df = as.integer(df_sph),
                    p = p_val,
                    gg = gg_eps,
                    hf = hf_eps,
                    lower = lower_eps
                ))
            }

            if (table$rowCount > 0) {
                table$addFootnote(
                    rowNo = 1,
                    col = "p",
                    note = .("A significant p-value (p < .05) indicates violation of sphericity, demonstrating why the MANOVA approach is required.")
                )
            }
        },

        .fillProfileTable = function(av_sum, between, rmFactorName) {
            table <- self$results$profileTable
            table$deleteRows()

            m_tests <- av_sum$multivariate.tests
            if (is.null(m_tests)) return()

            has_between <- (!is.null(between) && length(between) > 0)

            # Hypothesis 1: Parallelism (Time:Between interaction)
            if (has_between) {
                matched_term <- names(m_tests)[grep(paste0(rmFactorName, ".*", between[1]), names(m_tests))]
                if (length(matched_term) > 0) {
                    x <- m_tests[[matched_term[1]]]
                    SSPE_qr <- qr(x$SSPE)
                    eigs <- Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values)
                    wlk <- car:::Wilks(eigs, x$df, x$df.residual)
                    w_stat <- as.numeric(wlk[1])
                    f_val  <- as.numeric(wlk[2])
                    df1    <- as.integer(round(wlk[3]))
                    df2    <- as.numeric(wlk[4])
                    p_val  <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

                    conclusion <- if (p_val < 0.05) {
                        .("Profiles are NOT parallel (groups change differently over time; interaction is significant).")
                    } else {
                        .("Profiles are parallel (no significant difference in trajectory over time between groups).")
                    }

                    table$addRow(rowKey = "parallelism", values = list(
                        hypothesis = .("1. Parallelism (Time \u00D7 Group Interaction)"),
                        test = .("Wilks' Lambda"),
                        stat = w_stat,
                        f = f_val,
                        df1 = df1,
                        df2 = df2,
                        p = p_val,
                        conclusion = conclusion
                    ))
                }

                # Hypothesis 2: Equal Levels / Coincidence (Between-groups main effect)
                matched_b <- names(m_tests)[grep(paste0("^`?", between[1], "`?$"), names(m_tests))]
                if (length(matched_b) > 0) {
                    x <- m_tests[[matched_b[1]]]
                    SSPE_qr <- qr(x$SSPE)
                    eigs <- Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values)
                    pil <- car:::Pillai(eigs, x$df, x$df.residual)
                    p_stat <- as.numeric(pil[1])
                    f_val  <- as.numeric(pil[2])
                    df1    <- as.integer(round(pil[3]))
                    df2    <- as.numeric(pil[4])
                    p_val  <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

                    conclusion <- if (p_val < 0.05) {
                        .("Profiles do NOT coincide (significant overall elevation difference between groups).")
                    } else {
                        .("Profiles coincide (no significant overall elevation difference between groups).")
                    }

                    table$addRow(rowKey = "coincidence", values = list(
                        hypothesis = .("2. Equal Levels (Coincidence / Between-Group Main Effect)"),
                        test = .("Pillai / Univariate"),
                        stat = p_stat,
                        f = f_val,
                        df1 = df1,
                        df2 = df2,
                        p = p_val,
                        conclusion = conclusion
                    ))
                }
            }

            # Hypothesis 3: Flatness (Within-subjects time main effect)
            matched_time <- names(m_tests)[grep(paste0("^`?", rmFactorName, "`?$"), names(m_tests))]
            if (length(matched_time) > 0) {
                x <- m_tests[[matched_time[1]]]
                SSPE_qr <- qr(x$SSPE)
                eigs <- Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values)
                wlk <- car:::Wilks(eigs, x$df, x$df.residual)
                w_stat <- as.numeric(wlk[1])
                f_val  <- as.numeric(wlk[2])
                df1    <- as.integer(round(wlk[3]))
                df2    <- as.numeric(wlk[4])
                p_val  <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

                conclusion <- if (p_val < 0.05) {
                    .("Profiles are NOT flat (significant change across repeated measures over time).")
                } else {
                    .("Profiles are flat (no significant change across repeated measures over time).")
                }

                table$addRow(rowKey = "flatness", values = list(
                    hypothesis = .("3. Flatness (Within-Subjects Change over Time)"),
                    test = .("Wilks' Lambda"),
                    stat = w_stat,
                    f = f_val,
                    df1 = df1,
                    df2 = df2,
                    p = p_val,
                    conclusion = conclusion
                ))
            }
        },

        .fillContrastsTable = function(dat_clean, rmCells, rmFactorName, between) {
            table <- self$results$contrastsTable
            table$deleteRows()

            k <- length(rmCells)
            if (k < 2) return()

            Y <- as.matrix(dat_clean[, rmCells, drop = FALSE])
            poly_mat <- stats::contr.poly(k)
            c_names <- c(.("Linear"), .("Quadratic"), .("Cubic"), .("Quartic"), .("Quintic"))

            for (degree in 1:(k - 1)) {
                c_weight <- poly_mat[, degree]
                Y_c <- Y %*% c_weight
                c_label <- if (degree <= length(c_names)) c_names[degree] else paste0(.("Polynomial Degree"), " ", degree)

                # Test within-subjects contrast
                rhs <- if (!is.null(between) && length(between) > 0) paste(between, collapse = " * ") else "1"
                c_fit <- stats::lm(stats::as.formula(paste("Y_c ~", rhs)), data = dat_clean)
                aov_c <- stats::anova(c_fit)

                # Contrast main effect
                ss_intercept <- aov_c["(Intercept)", "Sum Sq"]
                df_intercept <- 1
                ms_intercept <- ss_intercept
                ss_res <- aov_c["Residuals", "Sum Sq"]
                df_res <- aov_c["Residuals", "Df"]
                ms_res <- ss_res / df_res
                f_val <- ms_intercept / ms_res
                p_val <- stats::pf(f_val, 1, df_res, lower.tail = FALSE)
                eta_p <- ss_intercept / (ss_intercept + ss_res)

                table$addRow(rowKey = paste0("poly_", degree), values = list(
                    contrast = paste0(rmFactorName, " (", c_label, ")"),
                    ss = ss_intercept,
                    df = 1,
                    ms = ms_intercept,
                    f = f_val,
                    p = p_val,
                    etaSqP = eta_p
                ))

                # Contrast x Between interaction
                if (!is.null(between) && length(between) > 0) {
                    for (b in between) {
                        if (b %in% rownames(aov_c)) {
                            ss_b <- aov_c[b, "Sum Sq"]
                            df_b <- aov_c[b, "Df"]
                            ms_b <- ss_b / df_b
                            f_b <- ms_b / ms_res
                            p_b <- stats::pf(f_b, df_b, df_res, lower.tail = FALSE)
                            eta_b <- ss_b / (ss_b + ss_res)

                            table$addRow(rowKey = paste0("poly_", degree, "_", b), values = list(
                                contrast = paste0(rmFactorName, " (", c_label, ") \u00D7 ", b),
                                ss = ss_b,
                                df = as.integer(df_b),
                                ms = ms_b,
                                f = f_b,
                                p = p_b,
                                etaSqP = eta_b
                            ))
                        }
                    }
                }
            }
        },

        .fillAssumptionsTable = function(dat_clean, vars, between) {
            table <- self$results$assumptionsTable
            table$deleteRows()

            Y <- as.matrix(dat_clean[, vars, drop = FALSE])
            n <- nrow(Y)
            p <- ncol(Y)

            # 1. Box's M Test
            if (!is.null(between) && length(between) > 0) {
                grp <- dat_clean[[between[1]]]
                box_res <- private$.calcBoxM(Y, grp)
                if (!is.null(box_res)) {
                    assess <- if (box_res$p < 0.001) {
                        .("Heterogeneous covariance matrices (p < .001). Rely primarily on Pillai's Trace.")
                    } else {
                        .("Homogeneity of covariance matrices supported (assumption met).")
                    }
                    table$addRow(rowKey = "box_m", values = list(
                        test = .("Box's M (Equality of Covariance Matrices)"),
                        stat = box_res$stat,
                        fOrChisq = box_res$f,
                        df1 = box_res$df1,
                        df2 = box_res$df2,
                        p = box_res$p,
                        assessment = assess
                    ))
                }
            }

            # 2. Mardia's Multivariate Normality
            mardia_res <- private$.calcMardia(Y)
            if (!is.null(mardia_res)) {
                # Skewness
                assess_skew <- if (mardia_res$skew$p < 0.05) .("Multivariate skewness significant.") else .("Multivariate skewness normal.")
                table$addRow(rowKey = "mardia_skew", values = list(
                    test = .("Mardia Multivariate Skewness"),
                    stat = mardia_res$skew$stat,
                    fOrChisq = mardia_res$skew$chisq,
                    df1 = mardia_res$skew$df,
                    df2 = NA,
                    p = mardia_res$skew$p,
                    assessment = assess_skew
                ))

                # Kurtosis
                assess_kurt <- if (mardia_res$kurt$p < 0.05) .("Multivariate kurtosis significant.") else .("Multivariate kurtosis normal.")
                table$addRow(rowKey = "mardia_kurt", values = list(
                    test = .("Mardia Multivariate Kurtosis"),
                    stat = mardia_res$kurt$stat,
                    fOrChisq = mardia_res$kurt$z,
                    df1 = NA,
                    df2 = NA,
                    p = mardia_res$kurt$p,
                    assessment = assess_kurt
                ))
            }

            # 3. Multivariate Outliers (Mahalanobis D^2)
            outlier_res <- private$.calcMahalanobis(Y)
            if (!is.null(outlier_res)) {
                assess_out <- if (outlier_res$n_outliers > 0) {
                    paste0(outlier_res$n_outliers, " ", .("observation(s) exceed critical Mahalanobis D\u00B2 (p < .001)."))
                } else {
                    .("No multivariate outliers detected at critical threshold (p < .001).")
                }
                table$addRow(rowKey = "mahalanobis", values = list(
                    test = .("Mahalanobis Distance (Multivariate Outliers)"),
                    stat = outlier_res$max_d2,
                    fOrChisq = outlier_res$crit,
                    df1 = p,
                    df2 = NA,
                    p = stats::pchisq(outlier_res$max_d2, df = p, lower.tail = FALSE),
                    assessment = assess_out
                ))
            }
        },

        .fillMeansTable = function(dat_clean, vars, between) {
            table <- self$results$meansTable
            table$deleteRows()

            has_between <- (!is.null(between) && length(between) > 0)

            if (has_between) {
                grp_col <- between[1]
                grp_levels <- levels(dat_clean[[grp_col]])

                for (grp in grp_levels) {
                    sub_dat <- dat_clean[dat_clean[[grp_col]] == grp, , drop = FALSE]
                    n_sub <- nrow(sub_dat)

                    for (v in vars) {
                        vals <- sub_dat[[v]]
                        m_val <- mean(vals, na.rm = TRUE)
                        s_val <- stats::sd(vals, na.rm = TRUE)
                        se_val <- s_val / sqrt(n_sub)
                        ci_lower <- m_val - 1.96 * se_val
                        ci_upper <- m_val + 1.96 * se_val

                        table$addRow(rowKey = paste(grp, v, sep = "_"), values = list(
                            factor = grp,
                            time = v,
                            n = as.integer(n_sub),
                            mean = m_val,
                            se = se_val,
                            sd = s_val,
                            ciLower = ci_lower,
                            ciUpper = ci_upper
                        ))
                    }
                }
            } else {
                n_all <- nrow(dat_clean)
                for (v in vars) {
                    vals <- dat_clean[[v]]
                    m_val <- mean(vals, na.rm = TRUE)
                    s_val <- stats::sd(vals, na.rm = TRUE)
                    se_val <- s_val / sqrt(n_all)
                    ci_lower <- m_val - 1.96 * se_val
                    ci_upper <- m_val + 1.96 * se_val

                    table$addRow(rowKey = v, values = list(
                        factor = .("Total"),
                        time = v,
                        n = as.integer(n_all),
                        mean = m_val,
                        se = se_val,
                        sd = s_val,
                        ciLower = ci_lower,
                        ciUpper = ci_upper
                    ))
                }
            }
        },

        .calcBoxM = function(Y, grp) {
            grp <- as.factor(grp)
            levels_g <- levels(grp)
            g <- length(levels_g)
            p <- ncol(Y)
            N <- nrow(Y)

            n_vec <- table(grp)
            if (any(n_vec <= p) || g < 2) return(NULL)

            det_vec <- numeric(g)
            pooled_cov <- matrix(0, p, p)

            for (i in seq_along(levels_g)) {
                Yi <- Y[grp == levels_g[i], , drop = FALSE]
                Si <- stats::cov(Yi)
                det_i <- det(Si)
                if (is.na(det_i) || det_i <= 0) return(NULL)
                det_vec[i] <- det_i
                pooled_cov <- pooled_cov + (n_vec[i] - 1) * Si
            }

            pooled_cov <- pooled_cov / (N - g)
            det_pooled <- det(pooled_cov)
            if (is.na(det_pooled) || det_pooled <= 0) return(NULL)

            M <- (N - g) * log(det_pooled) - sum((n_vec - 1) * log(det_vec))
            sum_inv_n <- sum(1 / (n_vec - 1))
            inv_N_g <- 1 / (N - g)
            c1 <- (sum_inv_n - inv_N_g) * (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (g - 1))
            df1 <- (g - 1) * p * (p + 1) / 2

            c2 <- (sum(1 / (n_vec - 1)^2) - 1 / (N - g)^2) * (p - 1) * (p + 2) / (6 * (g - 1))

            if ((c2 - c1^2) > 0) {
                df2 <- (df1 + 2) / (c2 - c1^2)
                a <- (1 - c1 - df1 / df2) / df1
                f_val <- M * a
            } else {
                df2 <- (df1 + 2) / (c1^2)
                f_val <- (M * (1 - c1)) / df1
            }

            p_val <- stats::pf(f_val, df1, df2, lower.tail = FALSE)
            list(stat = M, f = f_val, df1 = df1, df2 = df2, p = p_val)
        },

        .calcMardia = function(Y) {
            n <- nrow(Y)
            p <- ncol(Y)
            if (n <= p) return(NULL)

            Y_cen <- scale(Y, center = TRUE, scale = FALSE)
            S <- stats::cov(Y)
            S_inv <- tryCatch(solve(S), error = function(e) NULL)
            if (is.null(S_inv)) return(NULL)

            D <- Y_cen %*% S_inv %*% t(Y_cen)

            # Skewness
            b1p <- sum(D^3) / (n^2)
            chisq_skew <- n * b1p / 6
            df_skew <- p * (p + 1) * (p + 2) / 6
            p_skew <- stats::pchisq(chisq_skew, df = df_skew, lower.tail = FALSE)

            # Kurtosis
            d_diag <- diag(D)
            b2p <- sum(d_diag^2) / n
            expected_b2p <- p * (p + 2)
            var_b2p <- 8 * p * (p + 2) / n
            z_kurt <- (b2p - expected_b2p) / sqrt(var_b2p)
            p_kurt <- 2 * (1 - stats::pnorm(abs(z_kurt)))

            list(
                skew = list(stat = b1p, chisq = chisq_skew, df = df_skew, p = p_skew),
                kurt = list(stat = b2p, z = z_kurt, p = p_kurt)
            )
        },

        .calcMahalanobis = function(Y) {
            n <- nrow(Y)
            p <- ncol(Y)
            if (n <= p) return(NULL)

            S <- stats::cov(Y)
            S_inv <- tryCatch(solve(S), error = function(e) NULL)
            if (is.null(S_inv)) return(NULL)

            center <- colMeans(Y)
            d2 <- stats::mahalanobis(Y, center, S)
            crit_val <- stats::qchisq(0.999, df = p)
            n_outliers <- sum(d2 > crit_val)
            max_d2 <- max(d2)

            list(d2 = d2, crit = crit_val, max_d2 = max_d2, n_outliers = n_outliers)
        },

        .plotProfile = function(image, ...) {
            if (is.null(private$.state$dat_clean)) return(FALSE)

            dat <- private$.state$dat_clean
            rmCells <- private$.state$rmCells
            between <- private$.state$between
            plotError <- self$options$plotError

            if (is.null(rmCells) || length(rmCells) < 2) return(FALSE)

            # Reshape data to long format
            has_between <- (!is.null(between) && length(between) > 0)
            id_col <- "Subject_ID"
            dat[[id_col]] <- 1:nrow(dat)

            long_rows <- list()
            for (i in 1:nrow(dat)) {
                for (t_idx in seq_along(rmCells)) {
                    row_entry <- list(
                        id = dat[[id_col]][i],
                        time = rmCells[t_idx],
                        time_idx = t_idx,
                        val = dat[[rmCells[t_idx]]][i]
                    )
                    if (has_between) {
                        row_entry$group <- as.character(dat[[between[1]]][i])
                    } else {
                        row_entry$group <- "All"
                    }
                    long_rows[[length(long_rows) + 1]] <- row_entry
                }
            }

            long_df <- do.call(rbind, lapply(long_rows, as.data.frame))
            long_df$time <- factor(long_df$time, levels = rmCells)

            # Calculate summary stats per group and time
            summary_list <- list()
            groups <- unique(long_df$group)

            for (g in groups) {
                for (t_val in rmCells) {
                    sub_vals <- long_df$val[long_df$group == g & long_df$time == t_val]
                    m <- mean(sub_vals, na.rm = TRUE)
                    s <- stats::sd(sub_vals, na.rm = TRUE)
                    n_s <- length(sub_vals)
                    se <- s / sqrt(n_s)

                    summary_list[[length(summary_list) + 1]] <- data.frame(
                        group = g,
                        time = t_val,
                        mean = m,
                        se = se,
                        ci_lower = m - 1.96 * se,
                        ci_upper = m + 1.96 * se,
                        stringsAsFactors = FALSE
                    )
                }
            }

            sum_df <- do.call(rbind, summary_list)
            sum_df$time <- factor(sum_df$time, levels = rmCells)

            p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = time, y = mean, group = group, color = group)) +
                ggplot2::geom_line(size = 1.1) +
                ggplot2::geom_point(size = 3) +
                ggplot2::labs(
                    title = .("Repeated Measures Trajectory Profile"),
                    x = self$options$rmFactorName,
                    y = .("Mean Response"),
                    color = if (has_between) between[1] else ""
                ) +
                ggplot2::theme_minimal(base_size = 12) +
                ggplot2::theme(
                    panel.grid.minor = ggplot2::element_blank(),
                    legend.position = "top",
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
                )

            if (plotError == "ci") {
                p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, alpha = 0.8)
            } else if (plotError == "se") {
                p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = 0.15, alpha = 0.8)
            }

            print(p)
            TRUE
        },

        .plotHe = function(image, ...) {
            if (is.null(private$.state$dat_clean)) return(FALSE)

            dat <- private$.state$dat_clean
            rmCells <- private$.state$rmCells
            deps <- private$.state$deps
            vars <- if (!is.null(rmCells)) rmCells else deps

            if (is.null(vars) || length(vars) < 2) return(FALSE)

            Y <- as.matrix(dat[, vars[1:2], drop = FALSE])
            between <- private$.state$between

            df_plot <- as.data.frame(Y)
            colnames(df_plot) <- c("V1", "V2")

            if (!is.null(between) && length(between) > 0) {
                df_plot$Group <- dat[[between[1]]]
            } else {
                df_plot$Group <- factor("Total")
            }

            # 2D scatter with group ellipses representing Hypothesis/Error dispersion
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = V1, y = V2, color = Group)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::stat_ellipse(level = 0.68, linetype = "dashed", size = 0.9) +
                ggplot2::stat_ellipse(level = 0.95, size = 1.1) +
                ggplot2::labs(
                    title = .("Hypothesis-Error (HE) Multivariate Scatter & Ellipses"),
                    x = vars[1],
                    y = vars[2],
                    color = if (!is.null(between) && length(between) > 0) between[1] else ""
                ) +
                ggplot2::theme_minimal(base_size = 12) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                    legend.position = "top"
                )

            print(p)
            TRUE
        },

        .plotOutlier = function(image, ...) {
            if (is.null(private$.state$dat_clean)) return(FALSE)

            dat <- private$.state$dat_clean
            rmCells <- private$.state$rmCells
            deps <- private$.state$deps
            vars <- if (!is.null(rmCells)) rmCells else deps

            if (is.null(vars) || length(vars) < 2) return(FALSE)

            Y <- as.matrix(dat[, vars, drop = FALSE])
            res <- private$.calcMahalanobis(Y)
            if (is.null(res)) return(FALSE)

            df_out <- data.frame(
                Index = 1:length(res$d2),
                D2 = res$d2,
                Outlier = res$d2 > res$crit
            )

            p <- ggplot2::ggplot(df_out, ggplot2::aes(x = Index, y = D2, color = Outlier)) +
                ggplot2::geom_point(size = 2.5) +
                ggplot2::geom_hline(yintercept = res$crit, linetype = "dashed", color = "#D9534F", size = 1) +
                ggplot2::scale_color_manual(values = c("FALSE" = "#2C5E8A", "TRUE" = "#D9534F")) +
                ggplot2::labs(
                    title = .("Mahalanobis Distance D\u00B2 (Multivariate Outliers)"),
                    x = .("Observation Index"),
                    y = .("Mahalanobis Distance D\u00B2"),
                    subtitle = paste0(.("Critical threshold (p < .001)"), ": ", round(res$crit, 2))
                ) +
                ggplot2::theme_minimal(base_size = 12) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                    plot.subtitle = ggplot2::element_text(hjust = 0.5),
                    legend.position = "none"
                )

            print(p)
            TRUE
        }
    )
)
