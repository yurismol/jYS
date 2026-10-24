# This file is a generated template, your changes will not be overwritten

mMNVClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMNVClass",
    inherit = mMNVBase,
    private = list(

        .init = function() {
            instructions <- self$results$instructions
            rmCells <- self$options$rmCells
            isConfigured <- !is.null(rmCells) && length(rmCells) >= 2
            instructions$setVisible(!isConfigured)
            instructions$setContent(paste0(
                "<h3>", .("MANOVA & Repeated Measures: Methodological Guide"), "</h3>",
                "<p><b>", .("Getting started:"), "</b> ",
                .("Select at least 2 repeated measures variables (e.g., T1, T2, T3) into Repeated Measures Cells. Optionally specify a Within-Subjects Factor Name (e.g., Time), Between-Subjects Factors (e.g., Treatment Group), and Covariates."),
                "</p>",
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
            # 1. multivariateTable note
            mvTable <- self$results$multivariateTable
            mvTable$setNote(
                "mv_note",
                .("Multivariate tests evaluate within-subjects changes across time without assuming sphericity. Pillai's trace is recommended as the most robust criterion.")
            )

            # 2. univariateTable note
            uniTable <- self$results$univariateTable
            uniTable$setNote(
                "uni_note",
                .("Univariate ANOVA assumes sphericity. If Mauchly's test is significant (p < .05), report Greenhouse-Geisser (ε < .75) or Huynh-Feldt (ε ≥ .75) corrected p-values.")
            )

            # 3. sphericityTable note
            sphTable <- self$results$sphericityTable
            sphTable$setNote(
                "sph_note",
                .("Mauchly's test evaluates sphericity (equal variances of differences). Values of ε close to 1 indicate sphericity; ε < .75 indicates substantial departure.")
            )

            # 4. profileTable note
            profTable <- self$results$profileTable
            profTable$setNote(
                "prof_note",
                .("Profile analysis tests 3 key questions: Parallelism (different group trajectories), Coincidence (different overall levels), and Flatness (change over time).")
            )

            # 5. contrastsTable note
            cntTable <- self$results$contrastsTable
            cntTable$setNote(
                "cnt_note",
                .("Contrasts compare specific trajectories across time points (e.g., linear trends or step-by-step changes).")
            )

            # 6. assumptionsTable note
            assTable <- self$results$assumptionsTable
            assTable$setNote(
                "assump_note",
                .("Evaluates model assumptions on residuals: covariance matrix homogeneity (Box's M), multivariate normality (Mardia), and multivariate outliers (Mahalanobis D²).")
            )

            # 7. meansTable note
            mnsTable <- self$results$meansTable
            mnsTable$setNote(
                "means_note",
                .("Cell means and standard deviations with 95% Student's t confidence intervals. If covariates are present, model tests are evaluated at mean covariate values.")
            )
        },

        .run = function() {
            dat <- self$data
            rmCells <- self$options$rmCells

            # Quiet return when nothing is assigned
            if (is.null(rmCells) || length(rmCells) == 0) {
                return()
            }

            if (length(rmCells) < 2) {
                jmvcore::reject(.("Repeated Measures requires at least 2 variables / time points in Repeated Measures Cells."))
                return()
            }

            private$.runRepeatedMeasures(dat)
        },

                .runRepeatedMeasures = function(dat) {
            rmCells <- self$options$rmCells
            if (is.null(rmCells) || length(rmCells) < 2) {
                return()
            }

            k <- length(rmCells)
            rmFactorDisplay <- self$options$rmFactorName
            if (is.null(rmFactorDisplay) || rmFactorDisplay == "") {
                rmFactorDisplay <- "Time"
            }
            # Ensure syntactic R name for formula
            rmFactorFormula <- make.names(rmFactorDisplay)

            between <- self$options$between
            covs <- self$options$covs

            # Selected columns
            all_cols <- c(rmCells, between, covs)
            dat_clean <- stats::na.omit(dat[, all_cols, drop = FALSE])
            dat_clean <- droplevels(dat_clean)
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
                    vals <- jmvcore::toNumeric(dat_clean[[col]])
                    # Center covariates around their sample mean (standard Type III ANCOVA practice)
                    dat_clean[[col]] <- vals - mean(vals, na.rm = TRUE)
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
                rhs_terms <- c(rhs_terms, paste(paste0("`", between, "`"), collapse = " * "))
            }
            if (!is.null(covs) && length(covs) > 0) {
                rhs_terms <- c(rhs_terms, paste(paste0("`", covs, "`"), collapse = " + "))
            }

            rhs_formula <- if (length(rhs_terms) > 0) paste(rhs_terms, collapse = " + ") else "1"
            lhs_formula <- paste0("cbind(", paste(paste0("`", rmCells, "`"), collapse = ", "), ")")
            full_formula <- stats::as.formula(paste(lhs_formula, "~", rhs_formula))

            fit <- stats::lm(full_formula, data = dat_clean)

            # Within-subjects factor specification
            idata <- data.frame(setNames(list(factor(1:k, labels = rmCells)), rmFactorFormula))
            idesign <- stats::as.formula(paste0("~ `", rmFactorFormula, "`"))

            # Set contrast scheme
            contrastType <- self$options$contrastType
            if (contrastType == "polynomial") {
                c_mat <- stats::contr.poly(k)
            } else if (contrastType == "repeated") {
                c_mat <- matrix(0, nrow = k, ncol = k - 1)
                for (j in 1:(k - 1)) {
                    c_mat[j, j] <- -1
                    c_mat[j + 1, j] <- 1
                }
            } else if (contrastType == "treatment") {
                c_mat <- stats::contr.treatment(k)
            } else if (contrastType == "helmert") {
                c_mat <- stats::contr.helmert(k)
            } else if (contrastType == "difference") {
                c_mat <- matrix(0, nrow = k, ncol = k - 1)
                for (j in 1:(k - 1)) {
                    c_mat[1:j, j] <- -1 / j
                    c_mat[j + 1, j] <- 1
                }
            } else {
                c_mat <- stats::contr.poly(k)
            }
            stats::contrasts(idata[[rmFactorFormula]]) <- c_mat

            # Call car::Anova
            av <- tryCatch(
                car::Anova(fit, idata = idata, idesign = idesign, type = "III"),
                error = function(e) {
                    jmvcore::reject(paste0(.("Error evaluating MANOVA model"), ": ", e$message))
                }
            )

            av_sum <- suppressWarnings(summary(av, multivariate = TRUE))

            # Model residual matrix for assumption diagnostics
            res_matrix <- stats::residuals(fit)

            # 1. Fill Multivariate Table
            private$.fillMultivariateTable(av_sum, rmFactorDisplay = rmFactorDisplay, rmFactorFormula = rmFactorFormula)

            # 2. Fill Univariate Table
            if (self$options$showUnivariate) {
                private$.fillUnivariateTable(av_sum, rmFactorDisplay = rmFactorDisplay, rmFactorFormula = rmFactorFormula)
            }

            # 3. Fill Sphericity Table
            if (self$options$showSphericity) {
                private$.fillSphericityTable(av_sum, k = k, fit = fit, rmFactorDisplay = rmFactorDisplay, rmFactorFormula = rmFactorFormula)
            }

            # 4. Fill Profile Analysis Table
            if (self$options$showProfileAnalysis) {
                private$.fillProfileTable(av_sum, between = between, rmFactorDisplay = rmFactorDisplay, rmFactorFormula = rmFactorFormula)
            }

            # 5. Fill Contrasts Table
            if (self$options$showContrastsTable) {
                private$.fillContrastsTable(dat_clean, rmCells, rmFactorDisplay = rmFactorDisplay, rmFactorFormula = rmFactorFormula, between = between, covs = covs, contrastType = contrastType)
            }

            # 6. Fill Assumptions Table
            if (self$options$showAssumptionChecks) {
                private$.fillAssumptionsTable(dat_clean, rmCells, between, res_matrix = res_matrix)
            }

            # 7. Fill Means Table
            if (self$options$showMeansTable) {
                private$.fillMeansTable(dat_clean, rmCells, between)
            }

            # Multiple between factors notice
            if (!is.null(between) && length(between) > 1) {
                notice <- jmvcore::Notice$new(self$options, name = "multiBetweenNotice",
                    type = jmvcore::NoticeType$INFO,
                    content = jmvcore::format(
                        .("Multiple between-subjects factors specified. Descriptives, Box's M, profile analysis, and plots evaluate primary factor '{factor}' pooled across remaining factors."),
                        factor = between[1]
                    )
                )
                self$results$insert(1, notice)
            }

            # 8. Set lightweight states for plots (ensures full exportability)
            has_between <- (!is.null(between) && length(between) > 0)
            summary_list <- list()
            groups <- if (has_between) levels(dat_clean[[between[1]]]) else "Total"
            for (g in groups) {
                sub_df <- if (has_between) dat_clean[dat_clean[[between[1]]] == g, , drop = FALSE] else dat_clean
                for (t_val in rmCells) {
                    vals <- sub_df[[t_val]]
                    m <- mean(vals, na.rm = TRUE)
                    s <- stats::sd(vals, na.rm = TRUE)
                    n_s <- length(vals)
                    se <- if (n_s > 0) s / sqrt(n_s) else NA_real_
                    df_t <- max(1, n_s - 1)
                    t_crit <- stats::qt(0.975, df = df_t)
                    summary_list[[length(summary_list) + 1]] <- data.frame(
                        group = as.character(g),
                        time = t_val,
                        mean = m,
                        se = se,
                        ci_lower = m - t_crit * se,
                        ci_upper = m + t_crit * se,
                        stringsAsFactors = FALSE
                    )
                }
            }
            sum_df <- do.call(rbind, summary_list)
            sum_df$time <- factor(sum_df$time, levels = rmCells)

            self$results$profilePlot$setState(list(
                summary = sum_df,
                has_between = has_between,
                betweenName = if (has_between) between[1] else "",
                rmFactorName = rmFactorDisplay,
                rmCells = rmCells
            ))

            outlier_res <- private$.calcMahalanobis(res_matrix)
            if (!is.null(outlier_res)) {
                self$results$outlierPlot$setState(list(
                    d2 = outlier_res$d2,
                    crit = outlier_res$crit
                ))
            }

            self$results$hePlot$setState(list(
                vars = rmCells[1:2],
                between = between,
                row_indices = rownames(dat_clean)
            ))
        },

        .calcPillai = function(eig, q, df.res) {
            test <- sum(eig / (1 + eig))
            p <- length(eig)
            s <- min(p, q)
            n <- 0.5 * (df.res - p - 1)
            m <- 0.5 * (abs(p - q) - 1)
            tmp1 <- 2 * m + s + 1
            tmp2 <- 2 * n + s + 1
            c(test, (tmp2 / tmp1 * test) / (s - test), s * tmp1, s * tmp2)
        },

        .calcWilks = function(eig, q, df.res) {
            test <- prod(1 / (1 + eig))
            p <- length(eig)
            tmp1 <- df.res - 0.5 * (p - q + 1)
            tmp2 <- (p * q - 2) / 4
            tmp3 <- p^2 + q^2 - 5
            tmp3 <- if (tmp3 > 0) sqrt(((p * q)^2 - 4) / tmp3) else 1
            c(test, ((test^(-1 / tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2)) / p / q, p * q, tmp1 * tmp3 - 2 * tmp2)
        },

        .calcHL = function(eig, q, df.res) {
            test <- sum(eig)
            p <- length(eig)
            m <- 0.5 * (abs(p - q) - 1)
            n <- 0.5 * (df.res - p - 1)
            s <- min(p, q)
            tmp1 <- 2 * m + s + 1
            tmp2 <- 2 * (s * n + 1)
            c(test, (tmp2 * test) / s / s / tmp1, s * tmp1, tmp2)
        },

        .calcRoy = function(eig, q, df.res) {
            p <- length(eig)
            test <- max(eig)
            tmp1 <- max(p, q)
            tmp2 <- df.res - tmp1 + q
            c(test, (tmp2 * test) / tmp1, tmp1, tmp2)
        },

        .fillMultivariateTable = function(av_sum, rmFactorDisplay = "Time", rmFactorFormula = "Time") {
            table <- self$results$multivariateTable
            table$deleteRows()

            m_tests <- av_sum$multivariate.tests

            if (is.null(m_tests) || length(m_tests) == 0) return()

            test_configs <- list(
                list(id = "Pillai", name = .("Pillai's Trace"), opt = self$options$testPillai, fn = private$.calcPillai),
                list(id = "Wilks", name = .("Wilks' Lambda"), opt = self$options$testWilks, fn = private$.calcWilks),
                list(id = "Hotelling-Lawley", name = .("Hotelling–Lawley Trace"), opt = self$options$testHotelling, fn = private$.calcHL),
                list(id = "Roy", name = .("Roy's Largest Root"), opt = self$options$testRoy, fn = private$.calcRoy)
            )

            term_names <- names(m_tests)
            for (term in term_names) {
                x <- m_tests[[term]]
                if (is.null(x$SSPH) || is.null(x$SSPE)) next

                clean_term <- gsub("`", "", term)
                parts <- unlist(strsplit(clean_term, ":", fixed = TRUE))
                parts[parts == rmFactorFormula] <- rmFactorDisplay
                clean_term_display <- paste(parts, collapse = ":")

                if (!is.null(x$singular) && x$singular) {
                    notice <- jmvcore::Notice$new(self$options, name = paste0("sing_", clean_term),
                        type = jmvcore::NoticeType$WARNING,
                        content = jmvcore::format(.("Term '{term}' has a singular hypothesis or error matrix and was omitted from multivariate tests."), term = clean_term_display)
                    )
                    self$results$insert(1, notice)
                    next
                }

                SSPE_qr <- qr(x$SSPE)
                eigs <- tryCatch(
                    Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values),
                    error = function(e) NULL
                )
                if (is.null(eigs)) next

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
                        term = clean_term_display,
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

            if (table$rowCount > 0) {
                table$addFootnote(
                    rowNo = 1,
                    col = "test",
                    note = .("Multivariate tests make no sphericity assumption on the covariance matrix (O'Brien & Kaiser, 1985). Pillai's trace is robust to heterogeneity of covariance matrices.")
                )
            }
        },

        .fillUnivariateTable = function(av_sum, rmFactorDisplay = "Time", rmFactorFormula = "Time") {
            table <- self$results$univariateTable
            table$deleteRows()

            univ <- av_sum$univariate.tests
            adjust <- av_sum$pval.adjustments

            if (is.null(univ) || !is.matrix(univ)) return()

            univ_terms <- rownames(univ)

            # Filter out purely between-subjects terms: keep only terms involving the within-subjects factor
            is_within <- vapply(univ_terms, function(tm) {
                components <- gsub("`", "", unlist(strsplit(tm, ":", fixed = TRUE)))
                rmFactorFormula %in% components
            }, logical(1))
            univ_terms <- univ_terms[is_within]

            for (term in univ_terms) {
                clean_term <- gsub("`", "", term)
                parts <- unlist(strsplit(clean_term, ":", fixed = TRUE))
                parts[parts == rmFactorFormula] <- rmFactorDisplay
                clean_term_display <- paste(parts, collapse = ":")

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
                    term = clean_term_display,
                    correction = .("Sphericity Assumed"),
                    ss = ss_val,
                    df1 = df_val,
                    df2 = den_df,
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
                            term = clean_term_display,
                            correction = .("Greenhouse–Geisser"),
                            ss = ss_val,
                            df1 = df_val * gg_eps,
                            df2 = den_df * gg_eps,
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
                            term = clean_term_display,
                            correction = .("Huynh–Feldt"),
                            ss = ss_val,
                            df1 = df_val * hf_eps_capped,
                            df2 = den_df * hf_eps_capped,
                            ms = ss_val / (df_val * hf_eps_capped),
                            f = f_val,
                            p = hf_p,
                            etaSqP = eta_p
                        ))
                    }
                }
            }
        },

                .fillSphericityTable = function(av_sum, k, fit = NULL, rmFactorDisplay = "Time", rmFactorFormula = "Time") {
            table <- self$results$sphericityTable
            table$deleteRows()

            sph <- av_sum$sphericity.tests
            adjust <- av_sum$pval.adjustments

            if (is.null(sph) || !is.matrix(sph)) return()

            df_residual <- if (!is.null(fit)) fit$df.residual else (nrow(self$data) - 1)

            terms <- rownames(sph)
            for (term in terms) {
                clean_term <- gsub("`", "", term)
                parts <- unlist(strsplit(clean_term, ":", fixed = TRUE))
                parts[parts == rmFactorFormula] <- rmFactorDisplay
                clean_term_display <- paste(parts, collapse = ":")

                w_val <- if ("Test statistic" %in% colnames(sph)) as.numeric(sph[term, "Test statistic"]) else as.numeric(sph[term, 1])
                p_val <- if ("p-value" %in% colnames(sph)) as.numeric(sph[term, "p-value"]) else as.numeric(sph[term, 2])

                gg_eps <- NA
                hf_eps <- NA
                if (!is.null(adjust) && term %in% rownames(adjust)) {
                    gg_eps <- as.numeric(adjust[term, "GG eps"])
                    hf_eps <- as.numeric(adjust[term, "HF eps"])
                }

                lower_eps <- 1 / (k - 1)
                p_dim <- k - 1
                df_sph <- (k * (k - 1) / 2) - 1

                # Bartlett-Box multiplier: (N - g) - (2*p^2 + p + 2) / (6*p)
                c_factor <- (2 * p_dim^2 + p_dim + 2) / (6 * p_dim)
                d_factor <- df_residual - c_factor
                chisq_val <- if (!is.na(w_val) && w_val > 0 && d_factor > 0) -log(w_val) * d_factor else NA_real_

                table$addRow(rowKey = clean_term, values = list(
                    term = clean_term_display,
                    mauchlyW = w_val,
                    chisq = chisq_val,
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
                    note = .("A significant p-value indicates departure from sphericity. When sphericity is violated, use epsilon corrections or multivariate tests.")
                )
            }
        },

                .fillProfileTable = function(av_sum, between, rmFactorDisplay = "Time", rmFactorFormula = "Time") {
            table <- self$results$profileTable
            table$deleteRows()

            m_tests <- av_sum$multivariate.tests
            if (is.null(m_tests)) return()

            clean_term_names <- gsub("`", "", names(m_tests))

            find_term <- function(t1, t2 = NULL) {
                if (is.null(t2)) {
                    idx <- which(clean_term_names == t1)
                } else {
                    target1 <- paste(t1, t2, sep = ":")
                    target2 <- paste(t2, t1, sep = ":")
                    idx <- which(clean_term_names == target1 | clean_term_names == target2)
                }
                if (length(idx) > 0) names(m_tests)[idx[1]] else NULL
            }

            has_between <- (!is.null(between) && length(between) > 0)

            # Hypothesis 1: Parallelism (Time x Between interaction)
            if (has_between) {
                matched_term <- find_term(between[1], rmFactorFormula)
                if (!is.null(matched_term)) {
                    x <- m_tests[[matched_term]]
                    clean_term <- gsub("`", "", matched_term)
                    parts <- unlist(strsplit(clean_term, ":", fixed = TRUE))
                    parts[parts == rmFactorFormula] <- rmFactorDisplay
                    clean_term_display <- paste(parts, collapse = ":")

                    if (!is.null(x$singular) && isTRUE(x$singular)) {
                        table$setNote(paste0("sing_", clean_term), jmvcore::format(.("Term '{term}' has a singular hypothesis or error matrix; profile test omitted."), term = clean_term_display))
                    } else if (is.null(x$SSPH) || is.null(x$SSPE)) {
                        table$setNote(paste0("sing_", clean_term), jmvcore::format(.("Term '{term}' has insufficient residual degrees of freedom for multivariate profile test."), term = clean_term_display))
                    } else {
                        SSPE_qr <- qr(x$SSPE)
                        eigs <- tryCatch(
                            Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values),
                            error = function(e) NULL
                        )
                        if (is.null(eigs) || any(!is.finite(eigs))) {
                            table$setNote(paste0("sing_", clean_term), jmvcore::format(.("Term '{term}' has insufficient residual degrees of freedom for multivariate profile test."), term = clean_term_display))
                        } else {
                            wlk <- private$.calcWilks(eigs, x$df, x$df.residual)
                            w_stat <- as.numeric(wlk[1])
                            f_val  <- as.numeric(wlk[2])
                            df1    <- as.integer(round(wlk[3]))
                            df2    <- as.numeric(wlk[4])
                            p_val  <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

                            conclusion <- if (p_val < 0.05) {
                                .("Profiles deviate from parallelism (interaction is significant).")
                            } else {
                                .("Parallelism supported (no significant interaction).")
                            }

                            table$addRow(rowKey = "parallelism", values = list(
                                hypothesis = jmvcore::format(.("1. Parallelism ({time} × {group} Interaction)"), time = rmFactorDisplay, group = between[1]),
                                test = .("Wilks' Lambda"),
                                stat = w_stat,
                                f = f_val,
                                df1 = df1,
                                df2 = df2,
                                p = p_val,
                                conclusion = conclusion
                            ))
                        }
                    }
                }

                # Hypothesis 2: Equal Levels / Coincidence (Between-groups main effect)
                matched_b <- find_term(between[1])
                if (!is.null(matched_b)) {
                    x <- m_tests[[matched_b]]
                    clean_term_b <- gsub("`", "", matched_b)
                    if (!is.null(x$singular) && isTRUE(x$singular)) {
                        table$setNote(paste0("sing_", clean_term_b), jmvcore::format(.("Term '{term}' has a singular hypothesis or error matrix; profile test omitted."), term = clean_term_b))
                    } else if (is.null(x$SSPH) || is.null(x$SSPE)) {
                        table$setNote(paste0("sing_", clean_term_b), jmvcore::format(.("Term '{term}' has insufficient residual degrees of freedom for multivariate profile test."), term = clean_term_b))
                    } else {
                        SSPE_qr <- qr(x$SSPE)
                        eigs <- tryCatch(
                            Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values),
                            error = function(e) NULL
                        )
                        if (is.null(eigs) || any(!is.finite(eigs))) {
                            table$setNote(paste0("sing_", clean_term_b), jmvcore::format(.("Term '{term}' has insufficient residual degrees of freedom for multivariate profile test."), term = clean_term_b))
                        } else {
                            pil <- private$.calcPillai(eigs, x$df, x$df.residual)
                            p_stat <- as.numeric(pil[1])
                            f_val  <- as.numeric(pil[2])
                            df1    <- as.integer(round(pil[3]))
                            df2    <- as.numeric(pil[4])
                            p_val  <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

                            conclusion <- if (p_val < 0.05) {
                                .("Profiles differ in elevation (significant group main effect).")
                            } else {
                                .("Coincidence supported (no significant elevation difference).")
                            }

                            table$addRow(rowKey = "coincidence", values = list(
                                hypothesis = jmvcore::format(.("2. Equal Levels (Coincidence / {group} Main Effect)"), group = between[1]),
                                test = .("Pillai's Trace"),
                                stat = p_stat,
                                f = f_val,
                                df1 = df1,
                                df2 = df2,
                                p = p_val,
                                conclusion = conclusion
                            ))
                        }
                    }
                }

                if (length(between) > 1) {
                    table$setNote("profile_between_factor", jmvcore::format(.("Profile analysis evaluates between-subjects factor '{factor}'."), factor = between[1]))
                }
            }

            # Hypothesis 3: Flatness (Within-subjects time main effect)
            matched_time <- find_term(rmFactorFormula)
            if (!is.null(matched_time)) {
                x <- m_tests[[matched_time]]
                clean_term_t <- gsub("`", "", matched_time)
                if (!is.null(x$singular) && isTRUE(x$singular)) {
                    table$setNote(paste0("sing_", clean_term_t), jmvcore::format(.("Term '{term}' has a singular hypothesis or error matrix; profile test omitted."), term = rmFactorDisplay))
                } else if (is.null(x$SSPH) || is.null(x$SSPE)) {
                    table$setNote(paste0("sing_", clean_term_t), jmvcore::format(.("Term '{term}' has insufficient residual degrees of freedom for multivariate profile test."), term = rmFactorDisplay))
                } else {
                    SSPE_qr <- qr(x$SSPE)
                    eigs <- tryCatch(
                        Re(eigen(qr.coef(SSPE_qr, x$SSPH), symmetric = FALSE)$values),
                        error = function(e) NULL
                    )
                    if (is.null(eigs) || any(!is.finite(eigs))) {
                        table$setNote(paste0("sing_", clean_term_t), jmvcore::format(.("Term '{term}' has insufficient residual degrees of freedom for multivariate profile test."), term = rmFactorDisplay))
                    } else {
                        wlk <- private$.calcWilks(eigs, x$df, x$df.residual)
                        w_stat <- as.numeric(wlk[1])
                        f_val  <- as.numeric(wlk[2])
                        df1    <- as.integer(round(wlk[3]))
                        df2    <- as.numeric(wlk[4])
                        p_val  <- stats::pf(f_val, df1, df2, lower.tail = FALSE)

                        conclusion <- if (p_val < 0.05) {
                            .("Profiles show change across repeated measures.")
                        } else {
                            .("Flatness supported (no significant change across repeated measures).")
                        }

                        table$addRow(rowKey = "flatness", values = list(
                            hypothesis = jmvcore::format(.("3. Flatness (Within-Subjects Change over {time})"), time = rmFactorDisplay),
                            test = .("Wilks' Lambda"),
                            stat = w_stat,
                            f = f_val,
                            df1 = df1,
                            df2 = df2,
                            p = p_val,
                            conclusion = conclusion
                        ))
                    }
                }
            }
        },

                .fillContrastsTable = function(dat_clean, rmCells, rmFactorDisplay = "Time", rmFactorFormula = "Time", between = NULL, covs = NULL, contrastType = "polynomial") {
            table <- self$results$contrastsTable
            table$deleteRows()

            k <- length(rmCells)
            if (k < 2) return()

            Y <- as.matrix(dat_clean[, rmCells, drop = FALSE])

            if (contrastType == "polynomial") {
                c_mat <- stats::contr.poly(k)
                c_names <- c(.("Linear"), .("Quadratic"), .("Cubic"), .("Quartic"), .("Quintic"))
                c_labels <- sapply(1:(k - 1), function(d) if (d <= length(c_names)) c_names[d] else paste0(.("Degree"), " ", d))
                table$setNote("contrast_type_note", .("<b>Orthogonal Polynomial Contrasts</b>: Decompose the within-subject factor into orthogonal trends: Linear (constant rate of change), Quadratic (curvature/plateau), and Cubic (inflection points)."))
            } else if (contrastType == "repeated") {
                c_mat <- matrix(0, nrow = k, ncol = k - 1)
                c_labels <- character(k - 1)
                for (j in 1:(k - 1)) {
                    c_mat[j, j] <- -1
                    c_mat[j + 1, j] <- 1
                    c_labels[j] <- paste(rmCells[j + 1], "–", rmCells[j])
                }
                table$setNote("contrast_type_note", .("<b>Repeated Contrasts</b>: Compare each successive time point / condition to the immediately preceding one."))
            } else if (contrastType == "treatment") {
                c_mat <- matrix(0, nrow = k, ncol = k - 1)
                c_labels <- character(k - 1)
                for (j in 1:(k - 1)) {
                    c_mat[1, j] <- -1
                    c_mat[j + 1, j] <- 1
                    c_labels[j] <- paste(rmCells[j + 1], "–", rmCells[1])
                }
                table$setNote("contrast_type_note", .("<b>Treatment Contrasts</b>: Compare each subsequent time point / condition to the first (baseline)."))
            } else if (contrastType == "helmert") {
                c_mat <- matrix(0, nrow = k, ncol = k - 1)
                c_labels <- character(k - 1)
                for (j in 1:(k - 1)) {
                    c_mat[j, j] <- 1
                    c_mat[(j + 1):k, j] <- -1 / (k - j)
                    c_labels[j] <- paste0(rmCells[j], " vs ", .("subsequent"))
                }
                table$setNote("contrast_type_note", .("<b>Helmert Contrasts</b>: Compare each time point to the average of subsequent time points."))
            } else if (contrastType == "difference") {
                c_mat <- matrix(0, nrow = k, ncol = k - 1)
                c_labels <- character(k - 1)
                for (j in 1:(k - 1)) {
                    c_mat[1:j, j] <- -1 / j
                    c_mat[j + 1, j] <- 1
                    c_labels[j] <- paste0(rmCells[j + 1], " vs ", .("preceding"))
                }
                table$setNote("contrast_type_note", .("<b>Difference Contrasts</b>: Compare each time point to the average of preceding time points (reverse Helmert)."))
            } else {
                c_mat <- stats::contr.poly(k)
                c_labels <- paste0(.("Degree"), " ", 1:(k - 1))
                table$setNote("contrast_type_note", NULL)
            }

            old_contr <- options(contrasts = c("contr.sum", "contr.poly"))
            on.exit(options(old_contr), add = TRUE)

            rhs_terms <- c()
            if (!is.null(between) && length(between) > 0) {
                rhs_terms <- c(rhs_terms, paste(paste0("`", between, "`"), collapse = " * "))
            }
            if (!is.null(covs) && length(covs) > 0) {
                rhs_terms <- c(rhs_terms, paste(paste0("`", covs, "`"), collapse = " + "))
            }
            rhs <- if (length(rhs_terms) > 0) paste(rhs_terms, collapse = " + ") else "1"

            for (col_idx in 1:(k - 1)) {
                c_weight <- c_mat[, col_idx]
                Y_c <- Y %*% c_weight
                c_label <- c_labels[col_idx]

                c_fit <- stats::lm(stats::as.formula(paste("Y_c ~", rhs)), data = dat_clean)

                aov_c <- tryCatch(
                    car::Anova(c_fit, type = "III"),
                    error = function(e) NULL
                )

                if (!is.null(aov_c) && ("(Intercept)" %in% rownames(aov_c)) && ("Residuals" %in% rownames(aov_c))) {
                    ss_intercept <- aov_c["(Intercept)", "Sum Sq"]
                    df_intercept <- aov_c["(Intercept)", "Df"]
                    ms_intercept <- ss_intercept / df_intercept
                    ss_res <- aov_c["Residuals", "Sum Sq"]
                    df_res <- aov_c["Residuals", "Df"]
                    ms_res <- ss_res / df_res
                    f_val <- aov_c["(Intercept)", "F value"]
                    p_val <- aov_c["(Intercept)", "Pr(>F)"]
                    eta_p <- if (!is.na(ss_intercept) && !is.na(ss_res) && (ss_intercept + ss_res) > 0) ss_intercept / (ss_intercept + ss_res) else NA_real_
                } else {
                    sm <- summary(c_fit)
                    t_val <- sm$coefficients["(Intercept)", "t value"]
                    f_val <- t_val^2
                    df_intercept <- 1
                    df_res <- c_fit$df.residual
                    ss_res <- sum(stats::residuals(c_fit)^2)
                    ms_res <- ss_res / df_res
                    ss_intercept <- f_val * ms_res
                    ms_intercept <- ss_intercept
                    p_val <- stats::pf(f_val, 1, df_res, lower.tail = FALSE)
                    eta_p <- if (!is.na(ss_intercept) && !is.na(ss_res) && (ss_intercept + ss_res) > 0) ss_intercept / (ss_intercept + ss_res) else NA_real_
                }

                table$addRow(rowKey = paste0("contrast_", col_idx), values = list(
                    contrast = paste0(rmFactorDisplay, " (", c_label, ")"),
                    ss = ss_intercept,
                    df = as.integer(df_intercept),
                    ms = ms_intercept,
                    f = f_val,
                    p = p_val,
                    etaSqP = eta_p
                ))

                if (!is.null(aov_c)) {
                    b_terms <- setdiff(rownames(aov_c), c("(Intercept)", "Residuals"))
                    for (term_b in b_terms) {
                        ss_b <- aov_c[term_b, "Sum Sq"]
                        df_b <- aov_c[term_b, "Df"]
                        ms_b <- ss_b / df_b
                        f_b  <- aov_c[term_b, "F value"]
                        p_b  <- aov_c[term_b, "Pr(>F)"]
                        eta_b <- if ((ss_b + ss_res) > 0) ss_b / (ss_b + ss_res) else NA_real_

                        term_label <- gsub("`", "", term_b)
                        term_label <- gsub(":", " × ", term_label)

                        table$addRow(rowKey = paste0("contrast_", col_idx, "_", term_b), values = list(
                            contrast = paste0(rmFactorDisplay, " (", c_label, ") × ", term_label),
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
        },

                .fillAssumptionsTable = function(dat_clean, vars, between, res_matrix = NULL) {
            table <- self$results$assumptionsTable
            table$deleteRows()

            Y <- as.matrix(dat_clean[, vars, drop = FALSE])
            E <- if (!is.null(res_matrix)) as.matrix(res_matrix) else Y
            n <- nrow(E)
            p <- ncol(E)

            # 1. Box's M Test
            if (!is.null(between) && length(between) > 0) {
                grp <- dat_clean[[between[1]]]
                box_res <- private$.calcBoxM(Y, grp)
                if (!is.null(box_res)) {
                    if (!is.null(box_res$insufficient_n) && box_res$insufficient_n) {
                        table$addRow(rowKey = "box_m", values = list(
                            test = .("Box's M (Equality of Covariance Matrices)"),
                            stat = NA,
                            fOrChisq = NA,
                            df1 = NA,
                            df2 = NA,
                            p = NA,
                            assessment = box_res$msg
                        ))
                    } else {
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
                if (length(between) > 1) {
                    table$setNote("boxm_factor_note", jmvcore::format(
                        .("Box's M evaluates covariance homogeneity across levels of primary factor '{factor}'."),
                        factor = between[1]
                    ))
                } else {
                    table$setNote("boxm_factor_note", NULL)
                }
            }

            # 2. Mardia's Multivariate Normality of Residuals
            mardia_res <- private$.calcMardia(E)
            if (!is.null(mardia_res)) {
                # Skewness
                assess_skew <- if (mardia_res$skew$p < 0.05) .("Multivariate skewness significant (departure from normality).") else .("Multivariate skewness normal.")
                table$addRow(rowKey = "mardia_skew", values = list(
                    test = .("Mardia Multivariate Skewness (Residuals)"),
                    stat = mardia_res$skew$stat,
                    fOrChisq = mardia_res$skew$chisq,
                    df1 = mardia_res$skew$df,
                    df2 = NA,
                    p = mardia_res$skew$p,
                    assessment = assess_skew
                ))

                # Kurtosis
                assess_kurt <- if (mardia_res$kurt$p < 0.05) .("Multivariate kurtosis significant (departure from normality).") else .("Multivariate kurtosis normal.")
                table$addRow(rowKey = "mardia_kurt", values = list(
                    test = .("Mardia Multivariate Kurtosis (Residuals)"),
                    stat = mardia_res$kurt$stat,
                    fOrChisq = mardia_res$kurt$z,
                    df1 = NA,
                    df2 = NA,
                    p = mardia_res$kurt$p,
                    assessment = assess_kurt
                ))
            }

            # 3. Multivariate Outliers (Mahalanobis D^2 on Residuals)
            outlier_res <- private$.calcMahalanobis(E)
            if (!is.null(outlier_res)) {
                assess_out <- if (outlier_res$n_outliers > 0) {
                    paste0(outlier_res$n_outliers, " ", .("residual observation(s) exceed critical Mahalanobis D² (p < .001)."))
                } else {
                    .("No multivariate residual outliers detected at critical threshold (p < .001).")
                }
                table$addRow(rowKey = "mahalanobis", values = list(
                    test = .("Mahalanobis Distance D² (Residual Outliers)"),
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
                if (length(between) > 1) {
                    table$setNote("means_factor_note", jmvcore::format(
                        .("Marginal means and descriptives are evaluated for primary factor '{factor}' pooled across remaining factors."),
                        factor = between[1]
                    ))
                } else {
                    table$setNote("means_factor_note", NULL)
                }

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
                        df_t <- max(1, n_sub - 1)
                        t_crit <- stats::qt(0.975, df = df_t)
                        ci_lower <- m_val - t_crit * se_val
                        ci_upper <- m_val + t_crit * se_val

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
                    df_t <- max(1, n_all - 1)
                    t_crit <- stats::qt(0.975, df = df_t)
                    ci_lower <- m_val - t_crit * se_val
                    ci_upper <- m_val + t_crit * se_val

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

            covs <- self$options$covs
            if (!is.null(covs) && length(covs) > 0) {
                table$setNote("covs_means_note", .("Reported values are observed cell means and SDs with 95% Student's t confidence intervals. Model tests evaluate effects at the mean of the covariates."))
            } else {
                table$setNote("covs_means_note", .("Confidence intervals are 95% Student's t intervals."))
            }
        },

        .calcBoxM = function(Y, grp) {
            grp <- as.factor(grp)
            levels_g <- levels(grp)
            g <- length(levels_g)
            p <- ncol(Y)
            N <- nrow(Y)

            if (g < 2) return(NULL)

            n_vec <- table(grp)
            if (any(n_vec <= p)) {
                return(list(
                    insufficient_n = TRUE,
                    msg = .("Sample size in one or more groups (n <= p) is insufficient for Box's M test.")
                ))
            }

            det_vec <- numeric(g)
            pooled_cov <- matrix(0, p, p)

            for (i in seq_along(levels_g)) {
                Yi <- Y[grp == levels_g[i], , drop = FALSE]
                Si <- stats::cov(Yi)
                det_i <- det(Si)
                if (is.na(det_i) || det_i <= 0) {
                    return(list(
                        insufficient_n = TRUE,
                        msg = .("Covariance matrix in one or more groups is singular or non-positive definite.")
                    ))
                }
                det_vec[i] <- det_i
                pooled_cov <- pooled_cov + (n_vec[i] - 1) * Si
            }

            pooled_cov <- pooled_cov / (N - g)
            det_pooled <- det(pooled_cov)
            if (is.na(det_pooled) || det_pooled <= 0) {
                return(list(
                    insufficient_n = TRUE,
                    msg = .("Pooled covariance matrix is singular or non-positive definite.")
                ))
            }

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
                # Box (1949) inverted F approximation when c2 - c1^2 <= 0
                denom <- c1^2 - c2
                if (denom <= 0) denom <- 1e-6
                df2 <- (df1 + 2) / denom
                b_val <- df2 / (1 - c1 - 2 / df2)
                f_val <- if ((b_val - M) > 0) (df2 * M) / (df1 * (b_val - M)) else (M * (1 - c1)) / df1
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

        .plotProfile = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$summary)) return(FALSE)

            sum_df <- state$summary
            rmCells <- state$rmCells
            has_between <- state$has_between
            betweenName <- state$betweenName
            rmFactorName <- state$rmFactorName
            plotError <- self$options$plotError

            sum_df$time <- factor(sum_df$time, levels = rmCells)

            p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = time, y = mean, group = group, color = group)) +
                ggplot2::geom_line(linewidth = 1.1) +
                ggplot2::geom_point(size = 3) +
                ggplot2::labs(
                    title = .("Repeated Measures Trajectory Profile"),
                    x = rmFactorName,
                    y = .("Mean Response"),
                    color = if (has_between) betweenName else ""
                )

            if (!is.null(ggtheme)) {
                p <- p + ggtheme
            } else {
                p <- p + ggplot2::theme_minimal(base_size = 12)
            }
            p <- p + ggplot2::theme(
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

        .plotHe = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$vars)) return(FALSE)

            vars <- state$vars
            between <- state$between
            dat <- self$data

            if (is.null(dat) || is.null(vars) || length(vars) < 2) return(FALSE)

            # Restrict to complete cases from the fitted model
            if (!is.null(state$row_indices) && all(state$row_indices %in% rownames(dat))) {
                dat <- dat[state$row_indices, , drop = FALSE]
            }

            plot_cols <- vars
            has_between <- (!is.null(between) && length(between) > 0 && between[1] %in% names(dat))
            if (has_between) plot_cols <- c(plot_cols, between[1])
            dat_plot <- stats::na.omit(dat[, plot_cols, drop = FALSE])
            if (nrow(dat_plot) < 3) return(FALSE)

            for (v in vars) dat_plot[[v]] <- jmvcore::toNumeric(dat_plot[[v]])
            df_plot <- data.frame(
                V1 = dat_plot[[vars[1]]],
                V2 = dat_plot[[vars[2]]]
            )

            if (has_between) {
                df_plot$Group <- as.factor(dat_plot[[between[1]]])
            } else {
                df_plot$Group <- factor("Total")
            }

            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = V1, y = V2, color = Group)) +
                ggplot2::geom_point(alpha = 0.6, size = 2) +
                ggplot2::stat_ellipse(level = 0.68, linetype = "dashed", linewidth = 0.9) +
                ggplot2::stat_ellipse(level = 0.95, linewidth = 1.1) +
                ggplot2::labs(
                    title = .("Group Scatter with Data Ellipses"),
                    subtitle = jmvcore::format(.("Data ellipses (68% dashed, 95% solid) for '{v1}' and '{v2}'"), v1 = vars[1], v2 = vars[2]),
                    x = vars[1],
                    y = vars[2],
                    color = if (has_between) between[1] else ""
                )

            if (!is.null(ggtheme)) {
                p <- p + ggtheme
            } else {
                p <- p + ggplot2::theme_minimal(base_size = 12)
            }
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(hjust = 0.5),
                legend.position = "top"
            )

            print(p)
            TRUE
        },

        .plotOutlier = function(image, ggtheme = NULL, theme = NULL, ...) {
            state <- image$state
            if (is.null(state) || is.null(state$d2)) return(FALSE)

            d2 <- state$d2
            crit <- state$crit

            df_out <- data.frame(
                Index = seq_along(d2),
                D2 = d2,
                Outlier = d2 > crit
            )

            p <- ggplot2::ggplot(df_out, ggplot2::aes(x = Index, y = D2, color = Outlier)) +
                ggplot2::geom_point(size = 2.5) +
                ggplot2::geom_hline(yintercept = crit, linetype = "dashed", color = "#D9534F", linewidth = 1) +
                ggplot2::scale_color_manual(values = c("FALSE" = "#2C5E8A", "TRUE" = "#D9534F")) +
                ggplot2::labs(
                    title = .("Mahalanobis Distance D² (Residual Outliers)"),
                    x = .("Observation Index"),
                    y = .("Mahalanobis Distance D²"),
                    subtitle = paste0(.("Critical threshold (p < .001)"), ": ", round(crit, 2))
                )

            if (!is.null(ggtheme)) {
                p <- p + ggtheme
            } else {
                p <- p + ggplot2::theme_minimal(base_size = 12)
            }
            p <- p + ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(hjust = 0.5),
                legend.position = "none"
            )

            print(p)
            TRUE
        }
    )
)