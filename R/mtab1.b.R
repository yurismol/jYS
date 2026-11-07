
# This file is a part of the jYS package for jamovi

mTAB1Class <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "mTAB1Class",
    inherit = mTAB1Base,
    private = list(
        .init = function() {
            # HTML Instructions
            html <- self$results$instructions
            htmlContent <- paste0(
                "<div style='font-family: inherit; font-size: 13px; line-height: 1.5; color: #2c3e50;'>",
                "<h3 style='margin-top: 0; margin-bottom: 8px;'>", .("Baseline Characteristics (Table 1)"), "</h3>",
                "<p>", .("Automated publication-ready summary table (Table 1) generator for research datasets and group comparisons."), "</p>",
                "<ul>",
                "<li><b>", .("Continuous variables"), "</b>: ", .("automatic normality audit (Shapiro–Wilk) selects Mean ± SD (parametric) or Median [IQR] (non-parametric)."), "</li>",
                "<li><b>", .("Categorical variables"), "</b>: ", .("summarized as n (%) with automatic Chi-square or Fisher's exact test when expected cell counts < 5."), "</li>",
                "<li><b>", .("Group comparisons"), "</b>: ", .("Two-sample Student/Welch t, Mann–Whitney U, One-way/Welch ANOVA, Kruskal–Wallis, and Standardized Mean Difference (SMD)."), "</li>",
                "</ul>",
                "</div>"
            )
            html$setContent(htmlContent)

            # Table 1 dynamic columns
            tab1 <- self$results$tab1
            group_name <- self$options$group
            has_data <- !is.null(self$data) && nrow(self$data) > 0

            if (!is.null(group_name) && group_name != "" && !is.null(self$data) && group_name %in% names(self$data)) {
                grp_col <- as.factor(self$data[[group_name]])
                grp_levels <- levels(grp_col)
                grp_levels <- grp_levels[!is.na(grp_levels)]

                if (length(grp_levels) > 0) {
                    for (i in seq_along(grp_levels)) {
                        lvl <- grp_levels[i]
                        n_lvl <- if (has_data) sum(grp_col == lvl, na.rm = TRUE) else 0
                        col_title <- if (has_data && n_lvl > 0) paste0(lvl, " (N = ", n_lvl, ")") else as.character(lvl)
                        tab1$addColumn(
                            name = paste0("grp_", i),
                            title = col_title,
                            type = "text",
                            superTitle = group_name
                        )
                    }

                    # Determine whether df column is applicable
                    has_df_tests <- FALSE
                    if (self$options$showStat) {
                        if (length(grp_levels) >= 3) {
                            has_df_tests <- TRUE
                        } else if (length(grp_levels) == 2) {
                            if (self$options$method == "param" || self$options$catTest == "chisq") {
                                has_df_tests <- TRUE
                            } else if (has_data && !is.null(self$options$vars) && length(self$options$vars) > 0) {
                                for (v_chk in self$options$vars) {
                                    if (v_chk %in% names(self$data)) {
                                        vec_chk <- self$data[[v_chk]]
                                        v_valid_chk <- na.omit(vec_chk)
                                        is_cat_chk <- is.factor(vec_chk) || is.character(vec_chk) || (is.numeric(vec_chk) && length(v_valid_chk) > 0 && length(unique(v_valid_chk)) <= 2 && all(v_valid_chk %in% c(0, 1)))
                                        if (!is_cat_chk) {
                                            if (self$options$method != "nonparam") {
                                                v_valid <- na.omit(as.numeric(vec_chk))
                                                if (length(v_valid) >= 3) {
                                                    sw_chk <- tryCatch(shapiro.test(if (length(v_valid) > 5000) sample(v_valid, 5000) else v_valid), error = function(e) list(p.value = 0))
                                                    if (!is.na(sw_chk$p.value) && sw_chk$p.value >= self$options$normAlpha) {
                                                        has_df_tests <- TRUE
                                                        break
                                                    }
                                                }
                                            }
                                        } else {
                                            if (self$options$catTest != "fisher") {
                                                has_df_tests <- TRUE
                                                break
                                            }
                                        }
                                    }
                                }
                            } else {
                                has_df_tests <- TRUE
                            }
                        }
                    }

                    if (self$options$showStat) {
                        tab1$addColumn(name = "stat", title = .("Statistic"), type = "text")
                        tab1$addColumn(name = "df", title = .("df"), type = "text", visible = has_df_tests)
                    }
                    if (self$options$showPval) {
                        tab1$addColumn(name = "p", title = .("p"), type = "text")
                    }
                    if (self$options$padjust != "none") {
                        tab1$addColumn(name = "padj", title = .("p (adj)"), type = "text")
                        padj_method_title <- switch(self$options$padjust,
                            holm = .("Holm"),
                            bonferroni = .("Bonferroni"),
                            BH = .("Benjamini–Hochberg (FDR)"),
                            BY = .("Benjamini–Yekutieli"),
                            self$options$padjust
                        )
                        tab1$setNote("padj_note", jmvcore::format(.("p (adj) values adjusted for multiple comparisons using {method} method."), method = padj_method_title))
                    } else {
                        tab1$setNote("padj_note", NULL)
                    }
                    if (self$options$showSmd) {
                        tab1$addColumn(name = "smd", title = .("SMD"), type = "text")
                    }
                    if (self$options$showTestName) {
                        tab1$addColumn(name = "test", title = .("Test"), type = "text")
                    }
                }
            }

            # Update overall column title
            tot_title <- if (has_data) paste0(.("Overall"), " (N = ", nrow(self$data), ")") else .("Overall")
            col_tot <- tryCatch(tab1$getColumn("total"), error = function(e) NULL)
            if (!is.null(col_tot)) {
                col_tot$setTitle(tot_title)
            }

            # Pre-populate plots array if showPlot is enabled
            if (self$options$showPlot) {
                plots_array <- self$results$plots
                vars <- self$options$vars
                p_vars <- private$.getPlotVars(vars)
                plots_array$clear()
                if (!is.null(p_vars) && length(p_vars) > 0) {
                    for (v in p_vars) {
                        item <- plots_array$addItem(key = v)
                        item$setTitle(v)
                    }
                }
            } else {
                self$results$plots$clear()
            }

            # Post-Hoc visibility: hide table and show notice if group has < 3 levels
            if (self$options$showPostHoc) {
                has_3_levels <- FALSE
                if (!is.null(group_name) && group_name != "" && has_data && group_name %in% names(self$data)) {
                    grp_vals <- self$data[[group_name]]
                    grp_levels_init <- levels(as.factor(grp_vals))
                    grp_levels_init <- grp_levels_init[!is.na(grp_levels_init)]
                    has_3_levels <- length(grp_levels_init) >= 3
                }
                self$results$postHoc$setVisible(has_3_levels)
                self$results$postHocNotice$setVisible(!has_3_levels)
                if (!has_3_levels) {
                    msg <- .("Pairwise post-hoc analysis requires more than 2 comparison groups.")
                    notice_html <- paste0(
                        "<div style='padding: 10px 14px; background-color: #f8f9fa; border-left: 4px solid #e74c3c; color: #2c3e50; font-size: 13px; margin: 8px 0;'>",
                        "<b>", .("Note"), ":</b> ", msg,
                        "</div>"
                    )
                    self$results$postHocNotice$setContent(notice_html)
                    self$results$postHoc$setNote("method_note", NULL)
                } else {
                    ph_method_title <- switch(self$options$postHocMethod,
                        holm = .("Holm"),
                        bonferroni = .("Bonferroni"),
                        dscf = .("Dwass–Steel–Critchlow–Fligner (DSCF) / Games–Howell"),
                        tukey_dunn = .("Tukey HSD / Dunn"),
                        fdr = .("Benjamini–Hochberg (FDR)"),
                        self$options$postHocMethod
                    )
                    self$results$postHoc$setNote("method_note", jmvcore::format(.("Pairwise post-hoc p-values adjusted using {method} method."), method = ph_method_title))
                }
            } else {
                self$results$postHoc$setVisible(FALSE)
                self$results$postHocNotice$setVisible(FALSE)
                self$results$postHoc$setNote("method_note", NULL)
            }
        },

        .run = function() {
            vars <- self$options$vars
            if (is.null(vars) || length(vars) == 0)
                return()
            if (is.null(self$data) || nrow(self$data) == 0)
                return()

            data <- self$data
            group_name <- self$options$group
            has_group <- !is.null(group_name) && group_name != "" && (group_name %in% names(data))

            if (has_group) {
                group_col <- as.factor(data[[group_name]])
                grp_levels <- levels(group_col)
                grp_levels <- grp_levels[!is.na(grp_levels)]
                if (length(grp_levels) < 2)
                    has_group <- FALSE
            } else {
                grp_levels <- character(0)
            }

            # Post-Hoc visibility check: strictly hide table and show notice if group < 3 levels
            if (self$options$showPostHoc) {
                if (!has_group || length(grp_levels) < 3) {
                    self$results$postHoc$setVisible(FALSE)
                    self$results$postHocNotice$setVisible(TRUE)
                    msg <- .("Pairwise post-hoc analysis requires more than 2 comparison groups.")
                    notice_html <- paste0(
                        "<div style='padding: 10px 14px; background-color: #f8f9fa; border-left: 4px solid #e74c3c; color: #2c3e50; font-size: 13px; margin: 8px 0;'>",
                        "<b>", .("Note"), ":</b> ", msg,
                        "</div>"
                    )
                    self$results$postHocNotice$setContent(notice_html)
                } else {
                    self$results$postHoc$setVisible(TRUE)
                    self$results$postHocNotice$setVisible(FALSE)
                }
            } else {
                self$results$postHoc$setVisible(FALSE)
                self$results$postHocNotice$setVisible(FALSE)
            }

            # Significance symbol helper
            add_sig_symbol <- function(table_obj, row_key, col_name, p_val) {
                if (!self$options$flag || is.na(p_val) || is.null(p_val)) return()
                if (p_val < 0.001) {
                    table_obj$addSymbol(rowKey = row_key, col = col_name, symbol = "***")
                } else if (p_val < 0.01) {
                    table_obj$addSymbol(rowKey = row_key, col = col_name, symbol = "**")
                } else if (p_val < 0.05) {
                    table_obj$addSymbol(rowKey = row_key, col = col_name, symbol = "*")
                }
            }

            # Update column titles with actual counts
            tab1 <- self$results$tab1
            if (self$options$showTotal) {
                col_tot <- tryCatch(tab1$getColumn("total"), error = function(e) NULL)
                if (!is.null(col_tot)) {
                    col_tot$setTitle(paste0(.("Overall"), " (N = ", nrow(data), ")"))
                }
            }
            if (has_group) {
                for (i in seq_along(grp_levels)) {
                    lvl <- grp_levels[i]
                    n_lvl <- sum(group_col == lvl, na.rm = TRUE)
                    col_name <- paste0("grp_", i)
                    col_grp <- tryCatch(tab1$getColumn(col_name), error = function(e) NULL)
                    if (!is.null(col_grp)) {
                        col_grp$setTitle(paste0(lvl, " (N = ", n_lvl, ")"))
                    }
                }
            }

            # Resolve Decimal Mark, Digits, and p-value digits (inheriting from Jamovi global settings if not overridden)
            global_set <- private$.getJamoviGlobalSettings()

            opt_dec_sym <- tryCatch(self$options$decSymbol, error = function(e) NULL)
            opt_dec <- if (!is.null(opt_dec_sym) && nzchar(opt_dec_sym)) opt_dec_sym else self$options$decMark
            dec_sym <- if (identical(opt_dec, "comma") || identical(opt_dec, ",")) {
                ","
            } else if (identical(opt_dec, "dot") || identical(opt_dec, ".")) {
                "."
            } else {
                global_set$decMark
            }

            opt_digits <- self$options$digits
            digits <- if (!is.null(opt_digits) && is.numeric(opt_digits) && opt_digits >= 0) {
                as.integer(opt_digits)
            } else {
                global_set$digits
            }

            opt_pdigits <- self$options$pDigits
            pDigits <- if (!is.null(opt_pdigits) && is.numeric(opt_pdigits) && opt_pdigits >= 0) {
                as.integer(opt_pdigits)
            } else {
                global_set$pDigits
            }

            fmt_num <- function(val) {
                if (is.na(val) || is.null(val)) return("–")
                s <- sprintf(paste0("%.", digits, "f"), val)
                if (identical(dec_sym, ",")) {
                    s <- gsub(".", ",", s, fixed = TRUE)
                }
                s
            }

            fmt_pct <- function(val) {
                if (is.na(val) || is.null(val)) return("–")
                s <- sprintf(paste0("%.", max(1, digits), "f%%"), val)
                if (identical(dec_sym, ",")) {
                    s <- gsub(".", ",", s, fixed = TRUE)
                }
                s
            }

            fmt_pval <- function(val) {
                if (is.na(val) || is.null(val)) return("")
                cutoff <- 10^(-pDigits)
                if (val < cutoff) {
                    paste0("< ", if (identical(dec_sym, ",")) gsub(".", ",", sprintf(paste0("%.", pDigits, "f"), cutoff), fixed = TRUE) else sprintf(paste0("%.", pDigits, "f"), cutoff))
                } else {
                    s <- sprintf(paste0("%.", pDigits, "f"), val)
                    if (identical(dec_sym, ",")) s <- gsub(".", ",", s, fixed = TRUE)
                    s
                }
            }

            row_idx <- 1
            p_raw_vector <- numeric(0)
            p_row_keys <- character(0)

            has_cont_vars <- FALSE
            has_cat_vars <- FALSE
            has_norm_cont <- FALSE
            has_nonnorm_cont <- FALSE
            has_any_df <- FALSE
            has_df_col <- !is.null(tryCatch(self$results$tab1$getColumn("df"), error = function(e) NULL))

            skew_calc <- function(x) {
                m3 <- mean((x - mean(x))^3)
                s <- sd(x)
                if (is.na(s) || s == 0) return(NA_real_)
                m3 / (s^3)
            }

            kurt_calc <- function(x) {
                m4 <- mean((x - mean(x))^4)
                s <- sd(x)
                if (is.na(s) || s == 0) return(NA_real_)
                (m4 / (s^4)) - 3
            }

            plot_stats_map <- list()

            for (v in vars) {
                # Reset per-variable statistics
                pval <- NA_real_
                test_name <- ""
                is_normal <- FALSE
                ph_pairs_info <- list()

                vec <- data[[v]]

                # Identify if categorical or continuous
                v_valid_cat <- na.omit(vec)
                is_cat <- is.factor(vec) || is.character(vec) || (is.numeric(vec) && length(v_valid_cat) > 0 && length(unique(v_valid_cat)) <= 2 && all(v_valid_cat %in% c(0, 1)))

                if (!is_cat) {
                    # Continuous variable
                    vec_num <- as.numeric(vec)
                    vec_valid <- na.omit(vec_num)
                    n_total <- length(vec_valid)

                    if (n_total == 0) next

                    has_cont_vars <- TRUE

                    m_tot <- mean(vec_valid)
                    s_tot <- sd(vec_valid)
                    med_tot <- median(vec_valid)
                    q1_tot <- as.numeric(quantile(vec_valid, 0.25))
                    q3_tot <- as.numeric(quantile(vec_valid, 0.75))
                    iqr_tot <- q3_tot - q1_tot
                    se_tot <- if (n_total > 1) s_tot / sqrt(n_total) else 0
                    ci_l_tot <- m_tot - qt(0.975, df = max(1, n_total - 1)) * se_tot
                    ci_h_tot <- m_tot + qt(0.975, df = max(1, n_total - 1)) * se_tot

                    # Normality test
                    sw_tot <- if (n_total >= 3 && n_total <= 5000) {
                        tryCatch(shapiro.test(vec_valid), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
                    } else if (n_total > 5000) {
                        tryCatch(shapiro.test(sample(vec_valid, 5000)), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
                    } else {
                        list(statistic = NA_real_, p.value = NA_real_)
                    }

                    is_normal <- TRUE
                    if (self$options$method == "param") {
                        is_normal <- TRUE
                    } else if (self$options$method == "nonparam") {
                        is_normal <- FALSE
                    } else {
                        # auto
                        alpha_norm <- self$options$normAlpha
                        if (!is.na(sw_tot$p.value) && sw_tot$p.value < alpha_norm) {
                            is_normal <- FALSE
                        }
                        if (has_group) {
                            for (g in grp_levels) {
                                v_g <- na.omit(vec_num[group_col == g])
                                if (length(v_g) >= 3) {
                                    sw_g <- if (length(v_g) <= 5000) {
                                        tryCatch(shapiro.test(v_g), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
                                    } else {
                                        tryCatch(shapiro.test(sample(v_g, 5000)), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
                                    }
                                    if (!is.na(sw_g$p.value) && sw_g$p.value < alpha_norm) {
                                        is_normal <- FALSE
                                    }
                                }
                            }
                        }
                    }

                    if (is_normal) {
                        has_norm_cont <- TRUE
                    } else {
                        has_nonnorm_cont <- TRUE
                    }

                    # Format continuous summary
                    fmt_cont <- function(mean_v, sd_v, med_v, q1_v, q3_v, ci_l, ci_h, norm_flag) {
                        fmt_choice <- self$options$statFormatCont
                        if (self$options$method == "auto") {
                            if (fmt_choice == "mean_sd") {
                                if (norm_flag) {
                                    paste0(fmt_num(mean_v), " ± ", fmt_num(sd_v))
                                } else {
                                    paste0(fmt_num(med_v), " [", fmt_num(q1_v), "; ", fmt_num(q3_v), "]")
                                }
                            } else if (fmt_choice == "median_iqr") {
                                paste0(fmt_num(med_v), " [", fmt_num(q1_v), "; ", fmt_num(q3_v), "]")
                            } else if (fmt_choice == "mean_ci") {
                                paste0(fmt_num(mean_v), " (", fmt_num(ci_l), "; ", fmt_num(ci_h), ")")
                            } else {
                                paste0(fmt_num(mean_v), " ± ", fmt_num(sd_v), "; ", fmt_num(med_v), " [", fmt_num(q1_v), "; ", fmt_num(q3_v), "]")
                            }
                        } else if (self$options$method == "param") {
                            if (fmt_choice == "mean_ci") {
                                paste0(fmt_num(mean_v), " (", fmt_num(ci_l), "; ", fmt_num(ci_h), ")")
                            } else if (fmt_choice == "median_iqr") {
                                paste0(fmt_num(med_v), " [", fmt_num(q1_v), "; ", fmt_num(q3_v), "]")
                            } else {
                                paste0(fmt_num(mean_v), " ± ", fmt_num(sd_v))
                            }
                        } else {
                            paste0(fmt_num(med_v), " [", fmt_num(q1_v), "; ", fmt_num(q3_v), "]")
                        }
                    }

                    summary_label <- if (self$options$method == "auto") {
                        if (self$options$statFormatCont == "mean_sd") {
                            if (is_normal) .("Mean ± SD") else .("Median [Q1; Q3]")
                        } else if (self$options$statFormatCont == "median_iqr") {
                            .("Median [Q1; Q3]")
                        } else if (self$options$statFormatCont == "mean_ci") {
                            .("Mean (95% CI)")
                        } else {
                            .("Mean ± SD / Median [Q1; Q3]")
                        }
                    } else if (self$options$method == "param") {
                        if (self$options$statFormatCont == "mean_ci") .("Mean (95% CI)") else .("Mean ± SD")
                    } else {
                        .("Median [Q1; Q3]")
                    }

                    total_str <- fmt_cont(m_tot, s_tot, med_tot, q1_tot, q3_tot, ci_l_tot, ci_h_tot, is_normal)

                    grp_vals <- list()
                    if (has_group) {
                        for (i in seq_along(grp_levels)) {
                            g <- grp_levels[i]
                            v_g <- na.omit(vec_num[group_col == g])
                            if (length(v_g) > 0) {
                                m_g <- mean(v_g)
                                s_g <- sd(v_g)
                                med_g <- median(v_g)
                                q1_g <- as.numeric(quantile(v_g, 0.25))
                                q3_g <- as.numeric(quantile(v_g, 0.75))
                                se_g <- if (length(v_g) > 1) s_g / sqrt(length(v_g)) else 0
                                ci_l_g <- m_g - qt(0.975, df = max(1, length(v_g) - 1)) * se_g
                                ci_h_g <- m_g + qt(0.975, df = max(1, length(v_g) - 1)) * se_g
                                grp_vals[[paste0("grp_", i)]] <- fmt_cont(m_g, s_g, med_g, q1_g, q3_g, ci_l_g, ci_h_g, is_normal)
                            } else {
                                grp_vals[[paste0("grp_", i)]] <- "-"
                            }
                        }
                    }

                    # Statistical test across groups
                    test_name <- ""
                    stat_str <- ""
                    df_str <- ""
                    pval <- NA_real_
                    smd_val <- NA_real_

                    if (has_group) {
                        df_sub <- data.frame(y = vec_num, g = group_col)
                        df_sub <- na.omit(df_sub)

                        if (length(unique(df_sub$g)) >= 2) {
                            if (length(grp_levels) == 2) {
                                # 2 groups
                                if (is_normal) {
                                    use_var_eq <- FALSE
                                    if (self$options$varEqual == "equal") {
                                        use_var_eq <- TRUE
                                    } else if (self$options$varEqual == "levene") {
                                        p_lev <- tryCatch(car::leveneTest(y ~ g, data = df_sub)$"Pr(>F)"[1], error = function(e) NA_real_)
                                        use_var_eq <- (!is.na(p_lev) && p_lev >= 0.05)
                                    }
                                    tt <- tryCatch(stats::t.test(y ~ g, data = df_sub, var.equal = use_var_eq), error = function(e) NULL)
                                    if (!is.null(tt)) {
                                        pval <- tt$p.value
                                        stat_str <- paste0("t = ", fmt_num(tt$statistic))
                                        df_val <- tt$parameter
                                        df_str <- if (abs(df_val - round(df_val)) < 1e-4) as.character(round(df_val)) else fmt_num(df_val)
                                        has_any_df <- TRUE
                                        test_name <- if (use_var_eq) .("Student's t-test") else .("Welch's t-test")
                                    }
                                } else {
                                    wt <- tryCatch(stats::wilcox.test(y ~ g, data = df_sub, exact = FALSE), error = function(e) NULL)
                                    if (!is.null(wt)) {
                                        pval <- wt$p.value
                                        stat_str <- paste0("W = ", fmt_num(wt$statistic))
                                        df_str <- "–"
                                        test_name <- .("Mann–Whitney U")
                                    }
                                }

                                # SMD for 2 groups
                                g1 <- df_sub$y[df_sub$g == grp_levels[1]]
                                g2 <- df_sub$y[df_sub$g == grp_levels[2]]
                                n1 <- length(g1); n2 <- length(g2)
                                if (n1 > 1 && n2 > 1) {
                                    s_pool <- sqrt(((n1 - 1) * var(g1) + (n2 - 1) * var(g2)) / (n1 + n2 - 2))
                                    if (!is.na(s_pool) && s_pool > 0) {
                                        smd_val <- abs(mean(g1) - mean(g2)) / s_pool
                                    }
                                }
                            } else {
                                # 3+ groups
                                if (is_normal) {
                                    if (self$options$varEqual == "welch") {
                                        ow <- tryCatch(stats::oneway.test(y ~ g, data = df_sub, var.equal = FALSE), error = function(e) NULL)
                                        if (!is.null(ow)) {
                                            pval <- ow$p.value
                                            stat_str <- paste0("F = ", fmt_num(ow$statistic))
                                            df1 <- round(ow$parameter[1])
                                            df2_val <- ow$parameter[2]
                                            df2 <- if (abs(df2_val - round(df2_val)) < 1e-4) as.character(round(df2_val)) else fmt_num(df2_val)
                                            sep <- if (identical(dec_sym, ",")) "; " else ", "
                                            df_str <- paste0(df1, sep, df2)
                                            has_any_df <- TRUE
                                            test_name <- .("Welch's ANOVA")
                                        }
                                    } else {
                                        fit <- tryCatch(stats::aov(y ~ g, data = df_sub), error = function(e) NULL)
                                        if (!is.null(fit)) {
                                            sfit <- summary(fit)[[1]]
                                            pval <- sfit$"Pr(>F)"[1]
                                            stat_str <- paste0("F = ", fmt_num(sfit$"F value"[1]))
                                            df1 <- sfit$Df[1]
                                            df2 <- sfit$Df[2]
                                            sep <- if (identical(dec_sym, ",")) "; " else ", "
                                            df_str <- paste0(df1, sep, df2)
                                            has_any_df <- TRUE
                                            test_name <- .("One-way ANOVA")
                                        }
                                    }
                                } else {
                                    kt <- tryCatch(stats::kruskal.test(y ~ g, data = df_sub), error = function(e) NULL)
                                    if (!is.null(kt)) {
                                        pval <- kt$p.value
                                        stat_str <- paste0("χ² = ", fmt_num(kt$statistic))
                                        df_str <- as.character(round(kt$parameter))
                                        has_any_df <- TRUE
                                        test_name <- .("Kruskal–Wallis")
                                    }
                                }

                                # Max pairwise SMD across all pairs
                                pairs <- combn(grp_levels, 2, simplify = FALSE)
                                smd_pairs <- numeric(0)
                                for (pr in pairs) {
                                    g1 <- df_sub$y[df_sub$g == pr[1]]
                                    g2 <- df_sub$y[df_sub$g == pr[2]]
                                    n1 <- length(g1); n2 <- length(g2)
                                    if (n1 > 1 && n2 > 1) {
                                        sp <- sqrt(((n1 - 1) * var(g1) + (n2 - 1) * var(g2)) / (n1 + n2 - 2))
                                        if (!is.na(sp) && sp > 0) {
                                            smd_pairs <- c(smd_pairs, abs(mean(g1) - mean(g2)) / sp)
                                        }
                                    }
                                }
                                if (length(smd_pairs) > 0) {
                                    smd_val <- max(smd_pairs)
                                }
                            }
                        }
                    }

                    # Add main row to tab1
                    row_data <- list(var = v, level = summary_label, total = total_str)
                    for (nm in names(grp_vals)) {
                        row_data[[nm]] <- grp_vals[[nm]]
                    }
                    if (self$options$showStat) {
                        row_data[["stat"]] <- stat_str
                        if (has_df_col) {
                            row_data[["df"]] <- df_str
                        }
                    }
                    if (self$options$showPval) row_data[["p"]] <- if (!is.na(pval)) fmt_pval(pval) else ""
                    if (self$options$showSmd) row_data[["smd"]] <- if (!is.na(smd_val)) fmt_num(smd_val) else ""
                    if (self$options$showTestName) row_data[["test"]] <- test_name

                    cur_key <- paste0("r_", row_idx)
                    self$results$tab1$addRow(rowKey = cur_key, values = row_data)
                    if (has_group && self$options$showPval && !is.na(pval)) {
                        add_sig_symbol(self$results$tab1, cur_key, "p", pval)
                    }
                    if (!is.na(pval)) {
                        p_raw_vector <- c(p_raw_vector, pval)
                        p_row_keys <- c(p_row_keys, cur_key)
                    }
                    row_idx <- row_idx + 1

                    # Missing row if applicable
                    n_na_tot <- sum(is.na(vec_num))
                    show_na_row <- (self$options$missing == "always") || (self$options$missing == "ifany" && n_na_tot > 0)
                    if (show_na_row) {
                        na_row_data <- list(var = v, level = .("Missing (NA)"))
                        na_row_data[["total"]] <- paste0(n_na_tot, " (", fmt_pct(100 * n_na_tot / length(vec_num)), ")")
                        if (has_group) {
                            for (i in seq_along(grp_levels)) {
                                g <- grp_levels[i]
                                n_g_tot <- sum(group_col == g, na.rm = TRUE)
                                n_g_na <- sum(is.na(vec_num[group_col == g]))
                                pct_g_na <- if (n_g_tot > 0) 100 * n_g_na / n_g_tot else 0
                                na_row_data[[paste0("grp_", i)]] <- paste0(n_g_na, " (", fmt_pct(pct_g_na), ")")
                            }
                        }
                        self$results$tab1$addRow(rowKey = paste0("r_", row_idx), values = na_row_data)
                        row_idx <- row_idx + 1
                    }

                    # Distribution & Normality Audit table
                    if (self$options$showNormAudit) {
                        dec_tot <- if (is.na(sw_tot$p.value) || sw_tot$p.value >= self$options$normAlpha) {
                            .("Normal (Parametric)")
                        } else {
                            .("Non-normal (Non-parametric)")
                        }
                        norm_tot_row <- list(
                            var = v,
                            grp = .("Overall"),
                            n = n_total,
                            mean = fmt_num(m_tot),
                            sd = fmt_num(s_tot),
                            median = fmt_num(med_tot),
                            iqr = fmt_num(iqr_tot),
                            skew = fmt_num(skew_calc(vec_valid)),
                            kurt = fmt_num(kurt_calc(vec_valid)),
                            sw_w = if (!is.na(sw_tot$statistic)) fmt_num(as.numeric(sw_tot$statistic)) else "–",
                            sw_p = if (!is.na(sw_tot$p.value)) fmt_pval(sw_tot$p.value) else "–",
                            decision = dec_tot
                        )
                        self$results$normAudit$addRow(rowKey = paste0("na_", v, "_tot"), values = norm_tot_row)

                        if (has_group) {
                            for (g in grp_levels) {
                                v_g <- na.omit(vec_num[group_col == g])
                                if (length(v_g) >= 3) {
                                    sw_g <- if (length(v_g) <= 5000) {
                                        tryCatch(shapiro.test(v_g), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
                                    } else {
                                        tryCatch(shapiro.test(sample(v_g, 5000)), error = function(e) list(statistic = NA_real_, p.value = NA_real_))
                                    }
                                    dec_g <- if (is.na(sw_g$p.value) || sw_g$p.value >= self$options$normAlpha) {
                                        .("Normal (Parametric)")
                                    } else {
                                        .("Non-normal (Non-parametric)")
                                    }
                                    norm_g_row <- list(
                                        var = v,
                                        grp = as.character(g),
                                        n = length(v_g),
                                        mean = fmt_num(mean(v_g)),
                                        sd = fmt_num(sd(v_g)),
                                        median = fmt_num(median(v_g)),
                                        iqr = fmt_num(IQR(v_g)),
                                        skew = fmt_num(skew_calc(v_g)),
                                        kurt = fmt_num(kurt_calc(v_g)),
                                        sw_w = if (!is.na(sw_g$statistic)) fmt_num(as.numeric(sw_g$statistic)) else "–",
                                        sw_p = if (!is.na(sw_g$p.value)) fmt_pval(sw_g$p.value) else "–",
                                        decision = dec_g
                                    )
                                    self$results$normAudit$addRow(rowKey = paste0("na_", v, "_", g), values = norm_g_row)
                                }
                            }
                        }
                    }

                    # Pairwise Post-Hoc comparisons
                    if (self$options$showPostHoc && has_group && length(grp_levels) >= 3) {
                        pairs <- combn(grp_levels, 2, simplify = FALSE)
                        k_grp <- length(grp_levels)
                        ph_raw_p <- numeric(0)
                        ph_adj_p_direct <- numeric(0)
                        ph_items <- list()

                        for (pr in pairs) {
                            sub_d <- df_sub[df_sub$g %in% pr, ]
                            sub_d$g <- factor(sub_d$g, levels = pr)

                            if (self$options$postHocMethod == "dscf") {
                                if (is_normal) {
                                    d1 <- df_sub$y[df_sub$g == pr[1]]
                                    d2 <- df_sub$y[df_sub$g == pr[2]]
                                    n1 <- length(d1); n2 <- length(d2)
                                    s1 <- sd(d1); s2 <- sd(d2)
                                    diff_val <- abs(mean(d1) - mean(d2))
                                    se <- sqrt(s1^2 / n1 + s2^2 / n2)
                                    if (se > 0 && n1 > 1 && n2 > 1) {
                                        t_val <- diff_val / se
                                        q_stat <- sqrt(2) * t_val
                                        df_gh <- (s1^2/n1 + s2^2/n2)^2 / (((s1^2/n1)^2 / (n1 - 1)) + ((s2^2/n2)^2 / (n2 - 1)))
                                        p_ph <- 2 * stats::pt(t_val, df = df_gh, lower.tail = FALSE)
                                        p_adj <- stats::ptukey(q_stat, nmeans = k_grp, df = df_gh, lower.tail = FALSE)
                                        st_ph <- paste0("t = ", fmt_num(t_val))
                                    } else {
                                        p_ph <- NA_real_; p_adj <- NA_real_; st_ph <- ""
                                    }
                                    nm_ph <- .("Games–Howell")
                                } else {
                                    y_pr <- sub_d$y
                                    g_pr <- factor(sub_d$g, levels = pr)
                                    n1 <- sum(g_pr == pr[1]); n2 <- sum(g_pr == pr[2])
                                    N_pr <- n1 + n2
                                    r <- rank(y_pr)
                                    W1 <- sum(r[g_pr == pr[1]])
                                    E_W <- n1 * (N_pr + 1) / 2
                                    ties <- table(y_pr)
                                    tie_adj <- sum(ties^3 - ties)
                                    var_W <- (n1 * n2 * (N_pr + 1) / 12) - (n1 * n2 * tie_adj / (12 * N_pr * (N_pr - 1)))
                                    if (var_W > 0) {
                                        z <- (W1 - E_W) / sqrt(var_W)
                                        q_stat <- sqrt(2) * abs(z)
                                        p_ph <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
                                        p_adj <- stats::ptukey(q_stat, nmeans = k_grp, df = Inf, lower.tail = FALSE)
                                        st_ph <- paste0("W = ", fmt_num(q_stat))
                                    } else {
                                        p_ph <- NA_real_; p_adj <- NA_real_; st_ph <- ""
                                    }
                                    nm_ph <- .("Dwass–Steel–Critchlow–Fligner")
                                }
                                ph_raw_p <- c(ph_raw_p, p_ph)
                                ph_adj_p_direct <- c(ph_adj_p_direct, p_adj)
                                ph_items[[length(ph_items) + 1]] <- list(comp = paste0(pr[1], " vs ", pr[2]), stat = st_ph, p_raw = p_ph, test = nm_ph)

                            } else if (self$options$postHocMethod == "tukey_dunn") {
                                if (is_normal) {
                                    aov_fit <- tryCatch(stats::aov(y ~ g, data = df_sub), error = function(e) NULL)
                                    s_aov <- summary(aov_fit)[[1]]
                                    ms_err <- s_aov["Residuals", "Mean Sq"]
                                    df_err <- s_aov["Residuals", "Df"]
                                    d1 <- df_sub$y[df_sub$g == pr[1]]; d2 <- df_sub$y[df_sub$g == pr[2]]
                                    n1 <- length(d1); n2 <- length(d2)
                                    diff_val <- abs(mean(d1) - mean(d2))
                                    se <- sqrt(ms_err * (1/n1 + 1/n2))
                                    t_val <- diff_val / se
                                    q_stat <- sqrt(2) * t_val
                                    p_ph <- 2 * stats::pt(t_val, df = df_err, lower.tail = FALSE)
                                    p_adj <- stats::ptukey(q_stat, nmeans = k_grp, df = df_err, lower.tail = FALSE)
                                    st_ph <- paste0("q = ", fmt_num(q_stat))
                                    nm_ph <- .("Tukey HSD")
                                    ph_raw_p <- c(ph_raw_p, p_ph)
                                    ph_adj_p_direct <- c(ph_adj_p_direct, p_adj)
                                } else {
                                    r_all <- rank(df_sub$y)
                                    N_tot <- length(r_all)
                                    ties <- table(df_sub$y)
                                    tie_adj <- sum(ties^3 - ties)
                                    r1_mean <- mean(r_all[df_sub$g == pr[1]])
                                    r2_mean <- mean(r_all[df_sub$g == pr[2]])
                                    n1 <- sum(df_sub$g == pr[1]); n2 <- sum(df_sub$g == pr[2])
                                    se_dunn <- sqrt((N_tot * (N_tot + 1) / 12 - tie_adj / (12 * (N_tot - 1))) * (1/n1 + 1/n2))
                                    z_dunn <- if (se_dunn > 0) abs(r1_mean - r2_mean) / se_dunn else 0
                                    p_ph <- 2 * stats::pnorm(z_dunn, lower.tail = FALSE)
                                    st_ph <- paste0("z = ", fmt_num(z_dunn))
                                    nm_ph <- .("Dunn's test")
                                    ph_raw_p <- c(ph_raw_p, p_ph)
                                }
                                ph_items[[length(ph_items) + 1]] <- list(comp = paste0(pr[1], " vs ", pr[2]), stat = st_ph, p_raw = p_ph, test = nm_ph)

                            } else {
                                # holm, bonferroni, fdr
                                if (is_normal) {
                                    ph_t <- tryCatch(stats::t.test(y ~ g, data = sub_d, var.equal = FALSE), error = function(e) NULL)
                                    p_ph <- if (!is.null(ph_t)) ph_t$p.value else NA_real_
                                    st_ph <- if (!is.null(ph_t)) paste0("t = ", fmt_num(ph_t$statistic)) else ""
                                    nm_ph <- .("Welch's t-test")
                                } else {
                                    ph_t <- tryCatch(stats::wilcox.test(y ~ g, data = sub_d, exact = FALSE), error = function(e) NULL)
                                    p_ph <- if (!is.null(ph_t)) ph_t$p.value else NA_real_
                                    st_ph <- if (!is.null(ph_t)) paste0("W = ", fmt_num(ph_t$statistic)) else ""
                                    nm_ph <- .("Mann–Whitney U")
                                }
                                ph_raw_p <- c(ph_raw_p, p_ph)
                                ph_items[[length(ph_items) + 1]] <- list(comp = paste0(pr[1], " vs ", pr[2]), stat = st_ph, p_raw = p_ph, test = nm_ph)
                            }
                        }

                        final_adj_p <- if (length(ph_adj_p_direct) == length(ph_raw_p)) {
                            ph_adj_p_direct
                        } else {
                            adj_m <- switch(self$options$postHocMethod, holm = "holm", bonferroni = "bonferroni", fdr = "BH", tukey_dunn = "holm", "holm")
                            p.adjust(ph_raw_p, method = adj_m)
                        }

                        for (k in seq_along(ph_items)) {
                            item <- ph_items[[k]]
                            ph_row <- list(var = v, comp = item$comp, stat = item$stat, p_raw = if (!is.na(item$p_raw)) fmt_pval(item$p_raw) else "", p_adj = if (!is.na(final_adj_p[k])) fmt_pval(final_adj_p[k]) else "", test = item$test)
                            ph_key <- paste0("ph_", v, "_", k)
                            self$results$postHoc$addRow(rowKey = ph_key, values = ph_row)
                            add_sig_symbol(self$results$postHoc, ph_key, "p_raw", item$p_raw)
                            add_sig_symbol(self$results$postHoc, ph_key, "p_adj", final_adj_p[k])
                            pr <- pairs[[k]]
                            i_idx <- which(grp_levels == pr[1])
                            j_idx <- which(grp_levels == pr[2])
                            ph_pairs_info[[k]] <- list(i = i_idx, j = j_idx, p_adj = final_adj_p[k])
                        }
                    }

                    plot_stats_map[[v]] <- list(pval = pval, test_name = test_name, is_normal = is_normal, post_hoc = ph_pairs_info, is_cat = FALSE)

                } else {
                    # Categorical variable
                    has_cat_vars <- TRUE
                    f_vec <- as.factor(vec)
                    cat_levels <- levels(f_vec)
                    n_total_cat <- sum(!is.na(f_vec))

                    # Statistical test across groups
                    test_name <- ""
                    stat_str <- ""
                    df_str <- ""
                    pval <- NA_real_
                    smd_val <- NA_real_

                    if (has_group) {
                        valid_mask <- !is.na(f_vec) & !is.na(group_col)
                        tab <- table(f_vec[valid_mask], group_col[valid_mask])

                        if (nrow(tab) >= 1 && ncol(tab) >= 2) {
                            if (self$options$catTest == "fisher") {
                                ft <- tryCatch(stats::fisher.test(tab, simulate.p.value = (nrow(tab) > 2 || ncol(tab) > 2)), error = function(e) NULL)
                                if (!is.null(ft)) {
                                    pval <- ft$p.value
                                    stat_str <- "–"
                                    df_str <- "–"
                                    test_name <- .("Fisher's exact test")
                                }
                            } else if (self$options$catTest == "chisq") {
                                ct <- tryCatch(stats::chisq.test(tab, correct = FALSE), error = function(e) NULL)
                                if (!is.null(ct)) {
                                    pval <- ct$p.value
                                    stat_str <- paste0("χ² = ", fmt_num(ct$statistic))
                                    df_str <- as.character(round(ct$parameter))
                                    has_any_df <- TRUE
                                    test_name <- .("Chi-square test")
                                }
                            } else {
                                # auto
                                exp_c <- tryCatch(outer(rowSums(tab), colSums(tab)) / sum(tab), error = function(e) NULL)
                                if (!is.null(exp_c) && any(exp_c < 5)) {
                                    ft <- tryCatch(stats::fisher.test(tab, simulate.p.value = (nrow(tab) > 2 || ncol(tab) > 2)), error = function(e) stats::chisq.test(tab, correct = TRUE))
                                    if (!is.null(ft)) {
                                        pval <- ft$p.value
                                        if (inherits(ft, "htest") && grepl("Fisher", ft$method)) {
                                            stat_str <- "–"
                                            df_str <- "–"
                                            test_name <- .("Fisher's exact test")
                                        } else {
                                            stat_str <- if (!is.null(ft$statistic)) paste0("χ² = ", fmt_num(ft$statistic)) else ""
                                            df_str <- if (!is.null(ft$parameter)) as.character(round(ft$parameter)) else "–"
                                            if (nzchar(df_str) && df_str != "–") has_any_df <- TRUE
                                            test_name <- .("Chi-square (continuity corr.)")
                                        }
                                    }
                                } else {
                                    ct <- tryCatch(stats::chisq.test(tab, correct = FALSE), error = function(e) NULL)
                                    if (!is.null(ct)) {
                                        pval <- ct$p.value
                                        stat_str <- paste0("χ² = ", fmt_num(ct$statistic))
                                        df_str <- as.character(round(ct$parameter))
                                        has_any_df <- TRUE
                                        test_name <- .("Chi-square test")
                                    }
                                }
                            }

                            # SMD for categorical (Austin 2009)
                            if (length(grp_levels) == 2 && ncol(tab) == 2) {
                                col1 <- tab[, 1]
                                col2 <- tab[, 2]
                                s1 <- sum(col1)
                                s2 <- sum(col2)
                                if (s1 > 0 && s2 > 0) {
                                    p1 <- col1 / s1
                                    p2 <- col2 / s2
                                    if (length(p1) == 2) {
                                        p1_val <- p1[2]
                                        p2_val <- p2[2]
                                        denom <- sqrt((p1_val * (1 - p1_val) + p2_val * (1 - p2_val)) / 2)
                                        smd_val <- if (denom > 0) abs(p1_val - p2_val) / denom else 0
                                    } else {
                                        denom <- (p1 * (1 - p1) + p2 * (1 - p2)) / 2
                                        valid_d <- denom > 0
                                        if (any(valid_d)) {
                                            smd_val <- sqrt(0.5 * sum((p1[valid_d] - p2[valid_d])^2 / denom[valid_d]))
                                        }
                                    }
                                }
                            }
                        }
                    }

                    # Header row for categorical
                    header_label <- switch(self$options$statFormatCat, n_pct = .("n (%)"), pct_n = .("% (n)"), n = .("n"), pct = .("%"))
                    head_row_data <- list(var = v, level = header_label, total = paste0("N = ", n_total_cat))
                    if (has_group) {
                        for (i in seq_along(grp_levels)) {
                            g <- grp_levels[i]
                            n_g_tot <- sum(!is.na(f_vec[group_col == g]))
                            head_row_data[[paste0("grp_", i)]] <- paste0("N = ", n_g_tot)
                        }
                        if (self$options$showStat) {
                            head_row_data[["stat"]] <- stat_str
                            if (has_df_col) {
                                head_row_data[["df"]] <- df_str
                            }
                        }
                        if (self$options$showPval) head_row_data[["p"]] <- if (!is.na(pval)) fmt_pval(pval) else ""
                        if (self$options$showSmd) head_row_data[["smd"]] <- if (!is.na(smd_val)) fmt_num(smd_val) else ""
                        if (self$options$showTestName) head_row_data[["test"]] <- test_name
                    }

                    cur_key <- paste0("r_", row_idx)
                    self$results$tab1$addRow(rowKey = cur_key, values = head_row_data)
                    if (has_group && self$options$showPval && !is.na(pval)) {
                        add_sig_symbol(self$results$tab1, cur_key, "p", pval)
                    }
                    if (!is.na(pval)) {
                        p_raw_vector <- c(p_raw_vector, pval)
                        p_row_keys <- c(p_row_keys, cur_key)
                    }
                    row_idx <- row_idx + 1

                    # Formatter for category cell
                    fmt_cat_val <- function(cnt, base_tot) {
                        pct <- if (base_tot > 0) 100 * cnt / base_tot else 0
                        if (self$options$statFormatCat == "n_pct") {
                            paste0(cnt, " (", fmt_pct(pct), ")")
                        } else if (self$options$statFormatCat == "pct_n") {
                            paste0(fmt_pct(pct), " (", cnt, ")")
                        } else if (self$options$statFormatCat == "n") {
                            as.character(cnt)
                        } else {
                            fmt_pct(pct)
                        }
                    }

                    # Level rows
                    for (lvl in cat_levels) {
                        cnt_tot <- sum(f_vec == lvl, na.rm = TRUE)
                        tot_str <- fmt_cat_val(cnt_tot, n_total_cat)
                        lvl_row_data <- list(var = v, level = as.character(lvl), total = tot_str)

                        if (has_group) {
                            for (i in seq_along(grp_levels)) {
                                g <- grp_levels[i]
                                cnt_g <- sum(f_vec == lvl & group_col == g, na.rm = TRUE)
                                base_g <- if (self$options$pctType == "col") {
                                    sum(!is.na(f_vec[group_col == g]))
                                } else {
                                    cnt_tot
                                }
                                lvl_row_data[[paste0("grp_", i)]] <- fmt_cat_val(cnt_g, base_g)
                            }
                        }
                        self$results$tab1$addRow(rowKey = paste0("r_", row_idx), values = lvl_row_data)
                        row_idx <- row_idx + 1
                    }

                    # Missing row
                    n_na_cat <- sum(is.na(f_vec))
                    show_na_cat <- (self$options$missing == "always") || (self$options$missing == "ifany" && n_na_cat > 0)
                    if (show_na_cat) {
                        na_cat_data <- list(var = v, level = .("Missing (NA)"))
                        na_cat_data[["total"]] <- paste0(n_na_cat, " (", fmt_pct(100 * n_na_cat / length(f_vec)), ")")
                        if (has_group) {
                            for (i in seq_along(grp_levels)) {
                                g <- grp_levels[i]
                                n_g_tot <- sum(group_col == g, na.rm = TRUE)
                                n_g_na <- sum(is.na(f_vec[group_col == g]))
                                pct_g_na <- if (n_g_tot > 0) 100 * n_g_na / n_g_tot else 0
                                na_cat_data[[paste0("grp_", i)]] <- paste0(n_g_na, " (", fmt_pct(pct_g_na), ")")
                            }
                        }
                        self$results$tab1$addRow(rowKey = paste0("r_", row_idx), values = na_cat_data)
                        row_idx <- row_idx + 1
                    }

                    # Pairwise Post-Hoc for categorical (3+ groups)
                    if (self$options$showPostHoc && has_group && length(grp_levels) >= 3) {
                        pairs <- combn(grp_levels, 2, simplify = FALSE)
                        ph_raw_p <- numeric(0)
                        ph_items <- list()

                        for (pr in pairs) {
                            sub_mask <- group_col %in% pr & !is.na(f_vec)
                            tab_pr <- table(f_vec[sub_mask], factor(group_col[sub_mask], levels = pr))
                            ft_pr <- tryCatch(stats::fisher.test(tab_pr, simulate.p.value = (nrow(tab_pr) > 2)), error = function(e) stats::chisq.test(tab_pr))
                            p_ph <- if (!is.null(ft_pr)) ft_pr$p.value else NA_real_
                            st_ph <- if (!is.null(ft_pr$statistic)) paste0("χ² = ", fmt_num(ft_pr$statistic)) else ""
                            nm_ph <- if (grepl("Fisher", ft_pr$method)) .("Fisher's exact test") else .("Chi-square test")

                            ph_raw_p <- c(ph_raw_p, p_ph)
                            ph_items[[length(ph_items) + 1]] <- list(comp = paste0(pr[1], " vs ", pr[2]), stat = st_ph, p_raw = p_ph, test = nm_ph)
                        }

                        adj_m <- switch(self$options$postHocMethod, holm = "holm", bonferroni = "bonferroni", fdr = "BH", tukey_dunn = "holm", dscf = "holm", "holm")
                        ph_adj_p <- p.adjust(ph_raw_p, method = adj_m)

                        for (k in seq_along(ph_items)) {
                            item <- ph_items[[k]]
                            ph_row <- list(var = v, comp = item$comp, stat = item$stat, p_raw = if (!is.na(item$p_raw)) fmt_pval(item$p_raw) else "", p_adj = if (!is.na(ph_adj_p[k])) fmt_pval(ph_adj_p[k]) else "", test = item$test)
                            ph_key <- paste0("ph_", v, "_", k)
                            self$results$postHoc$addRow(rowKey = ph_key, values = ph_row)
                            add_sig_symbol(self$results$postHoc, ph_key, "p_raw", item$p_raw)
                            add_sig_symbol(self$results$postHoc, ph_key, "p_adj", ph_adj_p[k])
                            pr <- pairs[[k]]
                            i_idx <- which(grp_levels == pr[1])
                            j_idx <- which(grp_levels == pr[2])
                            ph_pairs_info[[k]] <- list(i = i_idx, j = j_idx, p_adj = ph_adj_p[k])
                        }
                    }
                }

                plot_stats_map[[v]] <- list(pval = pval, test_name = test_name, is_normal = FALSE, post_hoc = ph_pairs_info, is_cat = TRUE)
            }

            # Multiple testing correction across variables
            if (self$options$padjust != "none" && length(p_raw_vector) > 0) {
                p_adj_vector <- p.adjust(p_raw_vector, method = self$options$padjust)
                for (k in seq_along(p_row_keys)) {
                    rk <- p_row_keys[k]
                    self$results$tab1$setCell(rowKey = rk, col = "padj", value = fmt_pval(p_adj_vector[k]))
                    add_sig_symbol(self$results$tab1, rk, "padj", p_adj_vector[k])
                }
            }

            # Update df column visibility: show only if at least one test has degrees of freedom
            if (self$options$showStat && has_df_col) {
                col_df <- tryCatch(self$results$tab1$getColumn("df"), error = function(e) NULL)
                if (!is.null(col_df)) {
                    col_df$setVisible(has_any_df)
                }
            }

            # Continuous presentation description
            cont_str <- if (self$options$method == "param") {
                if (self$options$statFormatCont == "mean_ci") {
                    .("Mean (95% CI)")
                } else if (self$options$statFormatCont == "median_iqr") {
                    .("Median\u00A0[Q1; Q3]")
                } else {
                    .("Mean ± SD")
                }
            } else if (self$options$method == "nonparam") {
                .("Median\u00A0[Q1; Q3]")
            } else {
                # auto
                if (self$options$statFormatCont == "mean_ci") {
                    .("Mean (95% CI)")
                } else if (self$options$statFormatCont == "median_iqr") {
                    .("Median\u00A0[Q1; Q3]")
                } else if (self$options$statFormatCont == "both") {
                    .("Mean ± SD and Median\u00A0[Q1; Q3]")
                } else {
                    # mean_sd
                    if (has_norm_cont && has_nonnorm_cont) {
                        .("Mean ± SD or Median\u00A0[Q1; Q3]")
                    } else if (has_norm_cont && !has_nonnorm_cont) {
                        .("Mean ± SD")
                    } else if (!has_norm_cont && has_nonnorm_cont) {
                        .("Median\u00A0[Q1; Q3]")
                    } else {
                        .("Mean ± SD or Median\u00A0[Q1; Q3]")
                    }
                }
            }

            cat_str <- switch(self$options$statFormatCat,
                n_pct = "n (%)",
                pct_n = "% (n)",
                n = "n",
                pct = "%",
                "n (%)"
            )

            summary_desc <- if (has_cont_vars && has_cat_vars) {
                jmvcore::format(.("Values are {cont} for continuous variables; {cat} for categorical variables."), cont = cont_str, cat = cat_str)
            } else if (has_cont_vars && !has_cat_vars) {
                jmvcore::format(.("Values are {cont} for continuous variables."), cont = cont_str)
            } else if (!has_cont_vars && has_cat_vars) {
                jmvcore::format(.("Values are {cat} for categorical variables."), cat = cat_str)
            } else {
                ""
            }

            note_text <- paste0(
                summary_desc,
                if (has_group && nzchar(summary_desc)) paste0(" ", .("p-values test for differences across groups.")) else if (has_group) .("p-values test for differences across groups.") else ""
            )
            self$results$tab1$setNote("summary_note", note_text)

            if (self$options$padjust != "none") {
                padj_method_title <- switch(self$options$padjust,
                    holm = .("Holm"),
                    bonferroni = .("Bonferroni"),
                    BH = .("Benjamini–Hochberg (FDR)"),
                    BY = .("Benjamini–Yekutieli"),
                    self$options$padjust
                )
                padj_note <- jmvcore::format(.("p (adj) values adjusted for multiple comparisons using {method} method."), method = padj_method_title)
                self$results$tab1$setNote("padj_note", padj_note)
            } else {
                self$results$tab1$setNote("padj_note", NULL)
            }

            if (self$results$postHoc$visible) {
                ph_method_title <- switch(self$options$postHocMethod,
                    holm = .("Holm"),
                    bonferroni = .("Bonferroni"),
                    dscf = .("Dwass–Steel–Critchlow–Fligner (DSCF) / Games–Howell"),
                    tukey_dunn = .("Tukey HSD / Dunn"),
                    fdr = .("Benjamini–Hochberg (FDR)"),
                    self$options$postHocMethod
                )
                ph_note <- jmvcore::format(.("Pairwise post-hoc p-values adjusted using {method} method."), method = ph_method_title)
                self$results$postHoc$setNote("method_note", ph_note)
            } else {
                self$results$postHoc$setNote("method_note", NULL)
            }

            if (self$options$flag) {
                sign_note <- "* p < .05, ** p < .01, *** p < .001"
                self$results$tab1$setNote("flag_note", sign_note)
                if (self$results$postHoc$visible) {
                    self$results$postHoc$setNote("flag_note", sign_note)
                }
            } else {
                self$results$tab1$setNote("flag_note", NULL)
                self$results$postHoc$setNote("flag_note", NULL)
            }

            private$plot_stats_map <- plot_stats_map

            # Multi-variable Plot generation
            if (self$options$showPlot) {
                plots_array <- self$results$plots
                p_vars <- private$.getPlotVars(vars)
                p_vars <- intersect(p_vars, names(data))

                current_keys <- as.character(unlist(plots_array$itemKeys))
                if (!identical(current_keys, p_vars)) {
                    plots_array$clear()
                    for (v in p_vars) {
                        item <- plots_array$addItem(key = v)
                        item$setTitle(v)
                        item$setState(list(var = v, stat = plot_stats_map[[v]]))
                    }
                } else {
                    for (v in p_vars) {
                        item <- plots_array$get(key = v)
                        item$setTitle(v)
                        item$setState(list(var = v, stat = plot_stats_map[[v]]))
                    }
                }
            } else {
                self$results$plots$clear()
            }
        },

        .plot = function(image, ggtheme = NULL, theme = NULL, ...) {
            plotVar <- if (!is.null(image$state) && !is.null(image$state$var)) {
                image$state$var
            } else if (!is.null(image$key)) {
                image$key
            } else {
                NULL
            }

            if (is.null(plotVar) || !(plotVar %in% names(self$data))) {
                return(FALSE)
            }

            p_vars <- private$.getPlotVars(self$options$vars)
            if (!(plotVar %in% p_vars)) {
                return(FALSE)
            }

            stat_info <- if (!is.null(image$state) && !is.null(image$state$stat)) {
                image$state$stat
            } else if (!is.null(private$plot_stats_map[[plotVar]])) {
                private$plot_stats_map[[plotVar]]
            } else {
                private$.computePlotSignifStat(plotVar)
            }

            global_set <- private$.getJamoviGlobalSettings()
            opt_dec_sym <- tryCatch(self$options$decSymbol, error = function(e) NULL)
            opt_dec <- if (!is.null(opt_dec_sym) && nzchar(opt_dec_sym)) opt_dec_sym else self$options$decMark
            dec_sym <- if (identical(opt_dec, "comma") || identical(opt_dec, ",")) "," else if (identical(opt_dec, "dot") || identical(opt_dec, ".")) "." else global_set$decMark
            opt_pdigits <- self$options$pDigits
            pDigits <- if (!is.null(opt_pdigits) && is.numeric(opt_pdigits) && opt_pdigits >= 0) as.integer(opt_pdigits) else global_set$pDigits

            fmt_p_label <- function(p) {
                if (is.na(p) || is.null(p)) return("")
                threshold <- 10^(-pDigits)
                if (p < threshold) {
                    val_str <- sprintf(paste0("%.", pDigits, "f"), threshold)
                    if (identical(dec_sym, ",")) val_str <- gsub(".", ",", val_str, fixed = TRUE)
                    paste0("p < ", val_str)
                } else {
                    val_str <- sprintf(paste0("%.", pDigits, "f"), p)
                    if (identical(dec_sym, ",")) val_str <- gsub(".", ",", val_str, fixed = TRUE)
                    paste0("p = ", val_str)
                }
            }

            group_name <- self$options$group
            has_group <- !is.null(group_name) && group_name != "" && (group_name %in% names(self$data))

            data <- self$data
            vec <- data[[plotVar]]
            v_valid <- na.omit(vec)
            is_cat <- is.factor(vec) || is.character(vec) || (is.numeric(vec) && length(v_valid) > 0 && length(unique(v_valid)) <= 2 && all(v_valid %in% c(0, 1)))

            plot_type_cont <- self$options$plotTypeCont
            plot_type_cat  <- self$options$plotTypeCat

            # Compatibility with legacy plotType
            legacy_pt <- self$options$plotType
            if (!is.null(legacy_pt) && legacy_pt != "auto") {
                if (legacy_pt %in% c("box", "box_clean", "violin", "mean_ci", "density")) {
                    plot_type_cont <- legacy_pt
                } else if (legacy_pt %in% c("bar", "bar_pct")) {
                    plot_type_cat <- legacy_pt
                }
            }

            if (!is_cat) {
                # Continuous plot
                plot_type <- plot_type_cont
                df_plot <- if (has_group) {
                    data.frame(x = as.factor(data[[group_name]]), y = as.numeric(vec))
                } else {
                    data.frame(x = factor(.("Sample")), y = as.numeric(vec))
                }
                df_plot <- na.omit(df_plot)

                if (isTRUE(self$options$plotRemoveOutliers) && nrow(df_plot) > 0) {
                    # Filter out outliers per group using Tukey's fences (1.5 * IQR)
                    keep_rows <- integer(0)
                    for (lvl in levels(df_plot$x)) {
                        sub_idx <- which(df_plot$x == lvl)
                        sub_y <- df_plot$y[sub_idx]
                        if (length(sub_y) >= 4) {
                            qs <- stats::quantile(sub_y, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
                            iqr <- qs[2] - qs[1]
                            if (iqr > 0) {
                                lower_fence <- qs[1] - 1.5 * iqr
                                upper_fence <- qs[2] + 1.5 * iqr
                                valid <- sub_idx[sub_y >= lower_fence & sub_y <= upper_fence]
                                keep_rows <- c(keep_rows, valid)
                            } else {
                                keep_rows <- c(keep_rows, sub_idx)
                            }
                        } else {
                            keep_rows <- c(keep_rows, sub_idx)
                        }
                    }
                    if (length(keep_rows) > 0) {
                        df_plot <- df_plot[sort(keep_rows), , drop = FALSE]
                    }
                }

                if (nrow(df_plot) == 0) return(FALSE)

                n_levels <- length(levels(df_plot$x))

                if (plot_type == "density") {
                    p <- if (has_group) {
                        ggplot2::ggplot(df_plot, ggplot2::aes(x = y, fill = x, color = x)) +
                            ggplot2::geom_density(alpha = 0.35, linewidth = 0.8) +
                            ggplot2::labs(
                                x = plotVar,
                                y = .("Density")
                            )
                    } else {
                        ggplot2::ggplot(df_plot, ggplot2::aes(x = y)) +
                            ggplot2::geom_density(alpha = 0.45, fill = "#3498db", color = "#2980b9", linewidth = 0.8) +
                            ggplot2::labs(
                                x = plotVar,
                                y = .("Density")
                            )
                    }

                    if (!is.null(theme) && !is.null(theme$palette) && has_group) {
                        cols <- jmvcore::colorPalette(n = max(1, n_levels), theme$palette, type = "fill")
                        p <- p + ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_color_manual(values = cols)
                    }

                    if (!is.null(ggtheme)) p <- p + ggtheme

                    if (has_group) {
                        p <- p +
                            ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, nrow = 1),
                                            color = ggplot2::guide_legend(title = NULL, nrow = 1)) +
                            ggplot2::theme(
                                legend.position = "bottom",
                                legend.direction = "horizontal",
                                legend.title = ggplot2::element_blank(),
                                plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                            )
                    } else {
                        p <- p + ggplot2::theme(
                            legend.position = "none",
                            plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                        )
                    }

                    if (isTRUE(self$options$plotSignif) && has_group && !is.null(stat_info) && !is.na(stat_info$pval)) {
                        p_lbl <- fmt_p_label(stat_info$pval)
                        p <- p + ggplot2::annotate("label", x = Inf, y = Inf, label = p_lbl, hjust = 1.1, vjust = 1.4, color = "#e74c3c", fill = "white", fontface = "bold", size = 3.6)
                    }

                    print(p)
                    return(TRUE)
                }

                p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, fill = x))

                if (plot_type == "box_clean") {
                    # Boxplot without jitter
                    box_outlier_shape <- if (isTRUE(self$options$plotRemoveOutliers)) NA else 16
                    p <- p + ggplot2::geom_boxplot(alpha = 0.8, width = 0.5, outlier.size = 1.8, outlier.shape = box_outlier_shape, outlier.colour = "#2c3e50")
                } else if (plot_type == "violin") {
                    p <- p +
                        ggplot2::geom_violin(trim = FALSE, alpha = 0.5, color = "#2c3e50") +
                        ggplot2::geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8, fill = "white") +
                        ggplot2::geom_jitter(width = 0.1, alpha = 0.4, size = 1.5, color = "#34495e")
                } else if (plot_type == "mean_ci") {
                    mean_ci_fun <- function(y_vals) {
                        m <- mean(y_vals)
                        se <- if (length(y_vals) > 1) sd(y_vals) / sqrt(length(y_vals)) else 0
                        ci <- qt(0.975, df = max(1, length(y_vals) - 1)) * se
                        data.frame(y = m, ymin = m - ci, ymax = m + ci)
                    }
                    p <- p +
                        ggplot2::stat_summary(fun.data = mean_ci_fun, geom = "errorbar", width = 0.2, linewidth = 0.9, color = "#2c3e50") +
                        ggplot2::stat_summary(fun.data = mean_ci_fun, geom = "point", size = 3.5, shape = 21, stroke = 1.5, color = "#2c3e50")
                } else {
                    # Boxplot with jitter (default/auto/box)
                    p <- p +
                        ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.45) +
                        ggplot2::geom_jitter(width = 0.15, alpha = 0.4, size = 1.8, color = "#2c3e50")
                }

                p <- p +
                    ggplot2::labs(
                        x = if (has_group) group_name else "",
                        y = plotVar
                    )

                if (!is.null(theme) && !is.null(theme$palette)) {
                    cols <- jmvcore::colorPalette(n = max(1, n_levels), theme$palette, type = "fill")
                    p <- p + ggplot2::scale_fill_manual(values = cols)
                }

                if (!is.null(ggtheme)) p <- p + ggtheme

                if (has_group) {
                    p <- p +
                        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, nrow = 1)) +
                        ggplot2::theme(
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.box = "horizontal",
                            legend.title = ggplot2::element_blank(),
                            plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                        )
                } else {
                    p <- p + ggplot2::theme(
                        legend.position = "none",
                        plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                    )
                }

                if (isTRUE(self$options$plotSignif) && has_group && !is.null(stat_info)) {
                    grp_levels <- levels(df_plot$x)
                    k_grp <- length(grp_levels)
                    if (k_grp >= 2) {
                        y_vals <- df_plot$y
                        y_max <- max(y_vals, na.rm = TRUE)
                        y_min <- min(y_vals, na.rm = TRUE)
                        if (plot_type == "mean_ci") {
                            ci_max <- tryCatch(max(tapply(y_vals, df_plot$x, function(v) {
                                v <- na.omit(v)
                                if (length(v) <= 1) return(mean(v))
                                mean(v) + qt(0.975, df = length(v) - 1) * sd(v) / sqrt(length(v))
                            }), na.rm = TRUE), error = function(e) y_max)
                            if (!is.na(ci_max) && ci_max > y_max) y_max <- ci_max
                        }
                        y_rng <- y_max - y_min
                        if (y_rng <= 0) y_rng <- if (abs(y_max) > 0) abs(y_max) * 0.1 else 1
                        tick_h <- y_rng * 0.018
                        step_h <- y_rng * 0.075
                        base_offset <- 0.04

                        if (k_grp == 2) {
                            if (!is.na(stat_info$pval)) {
                                p_lbl <- fmt_p_label(stat_info$pval)
                                y_bar <- y_max + y_rng * base_offset
                                b_df <- data.frame(
                                    x = c(1, 1, 2, 2),
                                    y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                )
                                p <- p +
                                    ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                    ggplot2::annotate("text", x = 1.5, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0)
                                y_top <- y_bar + tick_h + y_rng * 0.035
                                y_bot <- y_min - y_rng * 0.05
                                p <- p + ggplot2::coord_cartesian(ylim = c(y_bot, y_top))
                            }
                        } else {
                            ph_pairs <- stat_info$post_hoc
                            use_post_hoc <- isTRUE(self$options$showPostHoc) && !is.null(ph_pairs) && length(ph_pairs) > 0

                            if (use_post_hoc) {
                                pairs_to_draw <- if (k_grp <= 3) {
                                    ph_pairs
                                } else {
                                    sig_p <- ph_pairs[sapply(ph_pairs, function(item) !is.na(item$p_adj) && item$p_adj < 0.05)]
                                    if (length(sig_p) > 0) sig_p else list()
                                }

                                if (length(pairs_to_draw) > 0) {
                                    # Sort pairs by span (j - i), then by i
                                    spans <- sapply(pairs_to_draw, function(pr) pr$j - pr$i)
                                    starts <- sapply(pairs_to_draw, function(pr) pr$i)
                                    pairs_to_draw <- pairs_to_draw[order(spans, starts)]

                                    pair_layers <- integer(length(pairs_to_draw))
                                    layer_alloc <- list()

                                    for (idx in seq_along(pairs_to_draw)) {
                                        pr <- pairs_to_draw[[idx]]
                                        i_val <- pr$i
                                        j_val <- pr$j
                                        placed_layer <- 0
                                        for (l_idx in seq_along(layer_alloc)) {
                                            has_overlap <- FALSE
                                            for (placed in layer_alloc[[l_idx]]) {
                                                if (max(placed[1], i_val) < min(placed[2], j_val)) {
                                                    has_overlap <- TRUE
                                                    break
                                                }
                                            }
                                            if (!has_overlap) {
                                                placed_layer <- l_idx
                                                break
                                            }
                                        }
                                        if (placed_layer == 0) {
                                            placed_layer <- length(layer_alloc) + 1
                                            layer_alloc[[placed_layer]] <- list()
                                        }
                                        layer_alloc[[placed_layer]][[length(layer_alloc[[placed_layer]]) + 1]] <- c(i_val, j_val)
                                        pair_layers[idx] <- placed_layer
                                    }

                                    dx <- 0.05
                                    highest_y <- y_max

                                    for (idx in seq_along(pairs_to_draw)) {
                                        pr <- pairs_to_draw[[idx]]
                                        layer <- pair_layers[idx]
                                        y_bar <- y_max + y_rng * base_offset + (layer - 1) * step_h
                                        if (y_bar > highest_y) highest_y <- y_bar

                                        # Check if another pair in the same layer touches on the left or right
                                        left_touches <- any(sapply(seq_along(pairs_to_draw), function(k) {
                                            k != idx && pair_layers[k] == layer && pairs_to_draw[[k]]$j == pr$i
                                        }))
                                        right_touches <- any(sapply(seq_along(pairs_to_draw), function(k) {
                                            k != idx && pair_layers[k] == layer && pairs_to_draw[[k]]$i == pr$j
                                        }))

                                        x_start <- if (left_touches) pr$i + dx else pr$i
                                        x_end   <- if (right_touches) pr$j - dx else pr$j

                                        b_df <- data.frame(
                                            x = c(x_start, x_start, x_end, x_end),
                                            y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                        )
                                        p_lbl <- fmt_p_label(pr$p_adj)
                                        p <- p +
                                            ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                            ggplot2::annotate("text", x = (x_start + x_end) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.5, fontface = "bold", vjust = 0)
                                    }
                                    y_top <- highest_y + tick_h + y_rng * 0.035
                                    y_bot <- y_min - y_rng * 0.05
                                    p <- p + ggplot2::coord_cartesian(ylim = c(y_bot, y_top))
                                } else {
                                    if (!is.na(stat_info$pval)) {
                                        p_lbl <- fmt_p_label(stat_info$pval)
                                        y_bar <- y_max + y_rng * base_offset
                                        b_df <- data.frame(
                                            x = c(1, 1, k_grp, k_grp),
                                            y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                        )
                                        p <- p +
                                            ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                            ggplot2::annotate("text", x = (1 + k_grp) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0)
                                        y_top <- y_bar + tick_h + y_rng * 0.035
                                        y_bot <- y_min - y_rng * 0.05
                                        p <- p + ggplot2::coord_cartesian(ylim = c(y_bot, y_top))
                                    }
                                }
                            } else {
                                if (!is.na(stat_info$pval)) {
                                    p_lbl <- fmt_p_label(stat_info$pval)
                                    y_bar <- y_max + y_rng * base_offset
                                    b_df <- data.frame(
                                        x = c(1, 1, k_grp, k_grp),
                                        y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                    )
                                    p <- p +
                                        ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                        ggplot2::annotate("text", x = (1 + k_grp) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0)
                                    y_top <- y_bar + tick_h + y_rng * 0.035
                                    y_bot <- y_min - y_rng * 0.05
                                    p <- p + ggplot2::coord_cartesian(ylim = c(y_bot, y_top))
                                }
                            }
                        }
                    }
                }

            } else {
                # Categorical bar plot
                plot_type <- plot_type_cat
                df_plot <- if (has_group) {
                    data.frame(Category = as.factor(vec), Group = as.factor(data[[group_name]]))
                } else {
                    data.frame(Category = as.factor(vec), Group = factor(.("Sample")))
                }
                df_plot <- na.omit(df_plot)

                if (nrow(df_plot) == 0) return(FALSE)

                if (plot_type == "bar_pct") {
                    # 100% Stacked percent bar chart
                    if (has_group) {
                        cat_n <- length(levels(df_plot$Category))
                        k_grp <- length(levels(df_plot$Group))
                        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Group, fill = Category)) +
                            ggplot2::geom_bar(position = "fill", alpha = 0.85, color = "#2c3e50") +
                            ggplot2::labs(
                                x = group_name,
                                y = .("Percent")
                            )

                        if (!is.null(theme) && !is.null(theme$palette)) {
                            cols <- jmvcore::colorPalette(n = max(1, cat_n), theme$palette, type = "fill")
                            p <- p + ggplot2::scale_fill_manual(values = cols)
                        }

                        if (!is.null(ggtheme)) p <- p + ggtheme

                        p <- p +
                            ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, nrow = 1)) +
                            ggplot2::theme(
                                legend.position = "bottom",
                                legend.direction = "horizontal",
                                legend.box = "horizontal",
                                legend.title = ggplot2::element_blank(),
                                plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                            )

                        if (isTRUE(self$options$plotSignif) && !is.null(stat_info)) {
                            tick_h <- 0.015
                            step_h <- 0.05
                            base_offset <- 0.035

                            if (k_grp == 2) {
                                if (!is.na(stat_info$pval)) {
                                    p_lbl <- fmt_p_label(stat_info$pval)
                                    y_bar <- 1.0 + base_offset
                                    b_df <- data.frame(
                                        x = c(1, 1, 2, 2),
                                        y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                    )
                                    y_top <- y_bar + tick_h + 0.025
                                    p <- p +
                                        ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                        ggplot2::annotate("text", x = 1.5, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0) +
                                        ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"), expand = ggplot2::expansion(mult = c(0, 0))) +
                                        ggplot2::coord_cartesian(ylim = c(0, y_top))
                                } else {
                                    p <- p + ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"))
                                }
                            } else {
                                ph_pairs <- stat_info$post_hoc
                                use_post_hoc <- isTRUE(self$options$showPostHoc) && !is.null(ph_pairs) && length(ph_pairs) > 0

                                if (use_post_hoc) {
                                    pairs_to_draw <- if (k_grp <= 3) {
                                        ph_pairs
                                    } else {
                                        sig_p <- ph_pairs[sapply(ph_pairs, function(item) !is.na(item$p_adj) && item$p_adj < 0.05)]
                                        if (length(sig_p) > 0) sig_p else list()
                                    }

                                    if (length(pairs_to_draw) > 0) {
                                        spans <- sapply(pairs_to_draw, function(pr) pr$j - pr$i)
                                        starts <- sapply(pairs_to_draw, function(pr) pr$i)
                                        pairs_to_draw <- pairs_to_draw[order(spans, starts)]

                                        pair_layers <- integer(length(pairs_to_draw))
                                        layer_alloc <- list()

                                        for (idx in seq_along(pairs_to_draw)) {
                                            pr <- pairs_to_draw[[idx]]
                                            i_val <- pr$i
                                            j_val <- pr$j
                                            placed_layer <- 0
                                            for (l_idx in seq_along(layer_alloc)) {
                                                has_overlap <- FALSE
                                                for (placed in layer_alloc[[l_idx]]) {
                                                    if (max(placed[1], i_val) < min(placed[2], j_val)) {
                                                        has_overlap <- TRUE
                                                        break
                                                    }
                                                }
                                                if (!has_overlap) {
                                                    placed_layer <- l_idx
                                                    break
                                                }
                                            }
                                            if (placed_layer == 0) {
                                                placed_layer <- length(layer_alloc) + 1
                                                layer_alloc[[placed_layer]] <- list()
                                            }
                                            layer_alloc[[placed_layer]][[length(layer_alloc[[placed_layer]]) + 1]] <- c(i_val, j_val)
                                            pair_layers[idx] <- placed_layer
                                        }

                                        dx <- 0.05
                                        highest_y <- 1.0

                                        for (idx in seq_along(pairs_to_draw)) {
                                            pr <- pairs_to_draw[[idx]]
                                            layer <- pair_layers[idx]
                                            y_bar <- 1.0 + base_offset + (layer - 1) * step_h
                                            if (y_bar > highest_y) highest_y <- y_bar

                                            left_touches <- any(sapply(seq_along(pairs_to_draw), function(k) {
                                                k != idx && pair_layers[k] == layer && pairs_to_draw[[k]]$j == pr$i
                                            }))
                                            right_touches <- any(sapply(seq_along(pairs_to_draw), function(k) {
                                                k != idx && pair_layers[k] == layer && pairs_to_draw[[k]]$i == pr$j
                                            }))

                                            x_start <- if (left_touches) pr$i + dx else pr$i
                                            x_end   <- if (right_touches) pr$j - dx else pr$j

                                            b_df <- data.frame(
                                                x = c(x_start, x_start, x_end, x_end),
                                                y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                            )
                                            p_lbl <- fmt_p_label(pr$p_adj)
                                            p <- p +
                                                ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                                ggplot2::annotate("text", x = (x_start + x_end) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.5, fontface = "bold", vjust = 0)
                                        }
                                        y_top <- highest_y + tick_h + 0.025
                                        p <- p +
                                            ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"), expand = ggplot2::expansion(mult = c(0, 0))) +
                                            ggplot2::coord_cartesian(ylim = c(0, y_top))
                                    } else {
                                        if (!is.na(stat_info$pval)) {
                                            p_lbl <- fmt_p_label(stat_info$pval)
                                            y_bar <- 1.0 + base_offset
                                            b_df <- data.frame(
                                                x = c(1, 1, k_grp, k_grp),
                                                y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                            )
                                            y_top <- y_bar + tick_h + 0.025
                                            p <- p +
                                                ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                                ggplot2::annotate("text", x = (1 + k_grp) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0) +
                                                ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"), expand = ggplot2::expansion(mult = c(0, 0))) +
                                                ggplot2::coord_cartesian(ylim = c(0, y_top))
                                        } else {
                                            p <- p + ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"))
                                        }
                                    }
                                } else {
                                    if (!is.na(stat_info$pval)) {
                                        p_lbl <- fmt_p_label(stat_info$pval)
                                        y_bar <- 1.0 + base_offset
                                        b_df <- data.frame(
                                            x = c(1, 1, k_grp, k_grp),
                                            y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                                        )
                                        y_top <- y_bar + tick_h + 0.025
                                        p <- p +
                                            ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                            ggplot2::annotate("text", x = (1 + k_grp) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0) +
                                            ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"), expand = ggplot2::expansion(mult = c(0, 0))) +
                                            ggplot2::coord_cartesian(ylim = c(0, y_top))
                                    } else {
                                        p <- p + ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"))
                                    }
                                }
                            }
                        } else {
                            p <- p + ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%"))
                        }
                    } else {
                        cat_n <- length(levels(df_plot$Category))
                        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Category, fill = Category)) +
                            ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(count) / sum(ggplot2::after_stat(count))), alpha = 0.85, color = "#2c3e50") +
                            ggplot2::scale_y_continuous(labels = function(z) paste0(round(z * 100), "%")) +
                            ggplot2::labs(
                                x = plotVar,
                                y = .("Percent")
                            )

                        if (!is.null(theme) && !is.null(theme$palette)) {
                            cols <- jmvcore::colorPalette(n = max(1, cat_n), theme$palette, type = "fill")
                            p <- p + ggplot2::scale_fill_manual(values = cols)
                        }

                        if (!is.null(ggtheme)) p <- p + ggtheme

                        p <- p + ggplot2::theme(
                            legend.position = "none",
                            plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                        )
                    }

                    print(p)
                    return(TRUE)
                }

                if (has_group) {
                    n_levels <- length(levels(df_plot$Group))
                    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Category, fill = Group)) +
                        ggplot2::geom_bar(position = "dodge", alpha = 0.85, color = "#2c3e50") +
                        ggplot2::labs(
                            x = plotVar,
                            y = .("Count")
                        )

                    if (!is.null(theme) && !is.null(theme$palette)) {
                        cols <- jmvcore::colorPalette(n = max(1, n_levels), theme$palette, type = "fill")
                        p <- p + ggplot2::scale_fill_manual(values = cols)
                    }

                    if (!is.null(ggtheme)) p <- p + ggtheme

                    p <- p +
                        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, nrow = 1)) +
                        ggplot2::theme(
                            legend.position = "bottom",
                            legend.direction = "horizontal",
                            legend.box = "horizontal",
                            legend.title = ggplot2::element_blank(),
                            plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                        )

                    if (isTRUE(self$options$plotSignif) && !is.null(stat_info) && !is.na(stat_info$pval)) {
                        p_lbl <- fmt_p_label(stat_info$pval)
                        n_cat <- length(levels(df_plot$Category))
                        tab_cg <- table(df_plot$Category, df_plot$Group)
                        y_max <- if (length(tab_cg) > 0) max(tab_cg) else 10
                        if (y_max <= 0) y_max <- 10
                        y_bar <- y_max * 1.04
                        tick_h <- y_max * 0.018
                        y_top <- y_bar + tick_h + y_max * 0.035
                        if (n_cat >= 2) {
                            b_df <- data.frame(
                                x = c(1, 1, n_cat, n_cat),
                                y = c(y_bar - tick_h, y_bar, y_bar, y_bar - tick_h)
                            )
                            p <- p +
                                ggplot2::geom_path(data = b_df, ggplot2::aes(x = x, y = y), color = "#e74c3c", linewidth = 0.7, inherit.aes = FALSE) +
                                ggplot2::annotate("text", x = (1 + n_cat) / 2, y = y_bar + tick_h * 0.5, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0) +
                                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
                                ggplot2::coord_cartesian(ylim = c(0, y_top))
                        } else {
                            p <- p +
                                ggplot2::annotate("text", x = 1, y = y_bar, label = p_lbl, color = "#e74c3c", size = 3.6, fontface = "bold", vjust = 0) +
                                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0))) +
                                ggplot2::coord_cartesian(ylim = c(0, y_top))
                        }
                    }
                } else {
                    n_levels <- length(levels(df_plot$Category))
                    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = Category, fill = Category)) +
                        ggplot2::geom_bar(alpha = 0.85, color = "#2c3e50") +
                        ggplot2::labs(
                            x = plotVar,
                            y = .("Count")
                        )

                    if (!is.null(theme) && !is.null(theme$palette)) {
                        cols <- jmvcore::colorPalette(n = max(1, n_levels), theme$palette, type = "fill")
                        p <- p + ggplot2::scale_fill_manual(values = cols)
                    }

                    if (!is.null(ggtheme)) p <- p + ggtheme

                    p <- p + ggplot2::theme(
                        legend.position = "none",
                        plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5)
                    )
                }
            }

            print(p)
            return(TRUE)
        },

        .getPlotVars = function(vars) {
            if (is.null(vars) || length(vars) == 0) return(character(0))
            data <- self$data
            scope <- self$options$plotScope

            # 1. If plotVars was passed explicitly in R call, honor it
            explicit_pv <- self$options$plotVars
            if (!is.null(explicit_pv) && length(explicit_pv) > 0) {
                matched <- intersect(explicit_pv, vars)
                if (length(matched) > 0) return(matched)
            }

            is_cat_var <- function(v) {
                if (is.null(data) || !(v %in% names(data))) return(FALSE)
                vec <- data[[v]]
                if (is.factor(vec) || is.character(vec)) return(TRUE)
                v_valid <- na.omit(vec)
                if (is.numeric(vec) && length(v_valid) > 0 && length(unique(v_valid)) <= 2 && all(v_valid %in% c(0, 1))) {
                    return(TRUE)
                }
                return(FALSE)
            }

            # 2. Scope-based selection from vars
            if (identical(scope, "cont")) {
                if (!is.null(data) && ncol(data) > 0) {
                    p_vars <- vars[!sapply(vars, is_cat_var)]
                    return(p_vars)
                }
            } else if (identical(scope, "cat")) {
                if (!is.null(data) && ncol(data) > 0) {
                    p_vars <- vars[sapply(vars, is_cat_var)]
                    return(p_vars)
                }
            }

            # Default: all variables from vars
            return(vars)
        },

        plot_stats_map = list(),

        .computePlotSignifStat = function(plotVar) {
            data <- self$data
            if (is.null(data) || !(plotVar %in% names(data))) return(NULL)
            group_name <- self$options$group
            if (is.null(group_name) || group_name == "" || !(group_name %in% names(data))) return(NULL)

            vec <- data[[plotVar]]
            grp <- as.factor(data[[group_name]])
            grp_levels <- levels(grp)
            if (length(grp_levels) < 2) return(NULL)

            v_valid_sig <- na.omit(vec)
            is_cat <- is.factor(vec) || is.character(vec) || (is.numeric(vec) && length(v_valid_sig) > 0 && length(unique(v_valid_sig)) <= 2 && all(v_valid_sig %in% c(0, 1)))

            pval <- NA_real_
            test_name <- ""
            post_hoc <- list()

            if (!is_cat) {
                vec_num <- as.numeric(vec)
                df_sub <- na.omit(data.frame(y = vec_num, g = grp))
                if (length(unique(df_sub$g)) >= 2) {
                    is_normal <- TRUE
                    if (self$options$method == "param") {
                        is_normal <- TRUE
                    } else if (self$options$method == "nonparam") {
                        is_normal <- FALSE
                    } else {
                        alpha_norm <- self$options$normAlpha
                        sw_tot <- if (nrow(df_sub) >= 3 && nrow(df_sub) <= 5000) {
                            tryCatch(shapiro.test(df_sub$y), error = function(e) list(p.value = NA_real_))
                        } else if (nrow(df_sub) > 5000) {
                            tryCatch(shapiro.test(sample(df_sub$y, 5000)), error = function(e) list(p.value = NA_real_))
                        } else {
                            list(p.value = NA_real_)
                        }
                        if (!is.na(sw_tot$p.value) && sw_tot$p.value < alpha_norm) {
                            is_normal <- FALSE
                        }
                        for (g_lvl in grp_levels) {
                            sub_y <- df_sub$y[df_sub$g == g_lvl]
                            if (length(sub_y) >= 3) {
                                sw_g <- if (length(sub_y) <= 5000) {
                                    tryCatch(shapiro.test(sub_y), error = function(e) list(p.value = NA_real_))
                                } else {
                                    tryCatch(shapiro.test(sample(sub_y, 5000)), error = function(e) list(p.value = NA_real_))
                                }
                                if (!is.na(sw_g$p.value) && sw_g$p.value < alpha_norm) {
                                    is_normal <- FALSE
                                    break
                                }
                            }
                        }
                    }

                    if (length(grp_levels) == 2) {
                        if (is_normal) {
                            use_var_eq <- FALSE
                            if (self$options$varEqual == "equal") {
                                use_var_eq <- TRUE
                            } else if (self$options$varEqual == "levene") {
                                p_lev <- tryCatch(car::leveneTest(y ~ g, data = df_sub)$"Pr(>F)"[1], error = function(e) NA_real_)
                                use_var_eq <- (!is.na(p_lev) && p_lev >= 0.05)
                            }
                            tt <- tryCatch(stats::t.test(y ~ g, data = df_sub, var.equal = use_var_eq), error = function(e) NULL)
                            if (!is.null(tt)) {
                                pval <- tt$p.value
                                test_name <- if (use_var_eq) .("Student's t-test") else .("Welch's t-test")
                            }
                        } else {
                            wt <- tryCatch(stats::wilcox.test(y ~ g, data = df_sub, exact = FALSE), error = function(e) NULL)
                            if (!is.null(wt)) {
                                pval <- wt$p.value
                                test_name <- .("Mann–Whitney U")
                            }
                        }
                    } else {
                        if (is_normal) {
                            if (self$options$varEqual == "welch") {
                                ow <- tryCatch(stats::oneway.test(y ~ g, data = df_sub, var.equal = FALSE), error = function(e) NULL)
                                if (!is.null(ow)) {
                                    pval <- ow$p.value
                                    test_name <- .("Welch's ANOVA")
                                }
                            } else {
                                fit <- tryCatch(stats::aov(y ~ g, data = df_sub), error = function(e) NULL)
                                if (!is.null(fit)) {
                                    sfit <- summary(fit)[[1]]
                                    pval <- sfit$"Pr(>F)"[1]
                                    test_name <- .("One-way ANOVA")
                                }
                            }
                        } else {
                            kt <- tryCatch(stats::kruskal.test(y ~ g, data = df_sub), error = function(e) NULL)
                            if (!is.null(kt)) {
                                pval <- kt$p.value
                                test_name <- .("Kruskal–Wallis")
                            }
                        }

                        if (isTRUE(self$options$showPostHoc)) {
                            pairs_idx <- utils::combn(seq_along(grp_levels), 2, simplify = FALSE)
                            ph_raw_p <- numeric(0)
                            ph_adj_p_direct <- numeric(0)
                            ph_pairs_list <- list()

                            fit_aov <- if (is_normal) tryCatch(stats::aov(y ~ g, data = df_sub), error = function(e) NULL) else NULL
                            s_aov <- if (!is.null(fit_aov)) summary(fit_aov)[[1]] else NULL
                            ms_err <- if (!is.null(s_aov)) s_aov$"Mean Sq"[2] else NA_real_
                            df_err <- if (!is.null(s_aov)) s_aov$Df[2] else NA_real_
                            k_grp <- length(grp_levels)

                            for (pr_idx in pairs_idx) {
                                i_idx <- pr_idx[1]
                                j_idx <- pr_idx[2]
                                g1_name <- grp_levels[i_idx]
                                g2_name <- grp_levels[j_idx]
                                sub_d <- df_sub[df_sub$g %in% c(g1_name, g2_name), ]
                                sub_d$g <- factor(sub_d$g, levels = c(g1_name, g2_name))

                                if (self$options$postHocMethod == "dscf") {
                                    if (is_normal) {
                                        s1 <- sd(sub_d$y[sub_d$g == g1_name]); s2 <- sd(sub_d$y[sub_d$g == g2_name])
                                        n1 <- sum(sub_d$g == g1_name); n2 <- sum(sub_d$g == g2_name)
                                        m1 <- mean(sub_d$y[sub_d$g == g1_name]); m2 <- mean(sub_d$y[sub_d$g == g2_name])
                                        se_gh <- sqrt(s1^2 / n1 + s2^2 / n2)
                                        t_gh <- abs(m1 - m2) / se_gh
                                        q_gh <- sqrt(2) * t_gh
                                        df_gh <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
                                        p_adj <- stats::ptukey(q_gh, nmeans = k_grp, df = max(1, df_gh), lower.tail = FALSE)
                                        p_ph <- 2 * stats::pt(t_gh, df = max(1, df_gh), lower.tail = FALSE)
                                        ph_raw_p <- c(ph_raw_p, p_ph)
                                        ph_adj_p_direct <- c(ph_adj_p_direct, p_adj)
                                    } else {
                                        wt <- tryCatch(stats::wilcox.test(y ~ g, data = sub_d, exact = FALSE), error = function(e) NULL)
                                        p_ph <- if (!is.null(wt)) wt$p.value else NA_real_
                                        W_stat <- if (!is.null(wt)) wt$statistic else NA_real_
                                        n1 <- sum(sub_d$g == g1_name); n2 <- sum(sub_d$g == g2_name)
                                        mu_w <- n1 * (n1 + n2 + 1) / 2
                                        var_w <- n1 * n2 * (n1 + n2 + 1) / 12
                                        z_stat <- if (!is.na(W_stat) && var_w > 0) abs(W_stat - mu_w) / sqrt(var_w) else 0
                                        q_dscf <- sqrt(2) * z_stat
                                        p_adj <- stats::ptukey(q_dscf, nmeans = k_grp, df = Inf, lower.tail = FALSE)
                                        ph_raw_p <- c(ph_raw_p, p_ph)
                                        ph_adj_p_direct <- c(ph_adj_p_direct, p_adj)
                                    }
                                } else if (self$options$postHocMethod == "tukey_dunn") {
                                    if (is_normal && !is.na(ms_err) && !is.na(df_err) && df_err > 0) {
                                        n1 <- sum(sub_d$g == g1_name); n2 <- sum(sub_d$g == g2_name)
                                        diff_val <- abs(mean(sub_d$y[sub_d$g == g1_name]) - mean(sub_d$y[sub_d$g == g2_name]))
                                        se <- sqrt(ms_err * (1/n1 + 1/n2))
                                        t_val <- diff_val / se
                                        q_stat <- sqrt(2) * t_val
                                        p_ph <- 2 * stats::pt(t_val, df = df_err, lower.tail = FALSE)
                                        p_adj <- stats::ptukey(q_stat, nmeans = k_grp, df = df_err, lower.tail = FALSE)
                                        ph_raw_p <- c(ph_raw_p, p_ph)
                                        ph_adj_p_direct <- c(ph_adj_p_direct, p_adj)
                                    } else {
                                        r_all <- rank(df_sub$y)
                                        N_tot <- length(r_all)
                                        ties <- table(df_sub$y)
                                        tie_adj <- sum(ties^3 - ties)
                                        r1_mean <- mean(r_all[df_sub$g == g1_name]); r2_mean <- mean(r_all[df_sub$g == g2_name])
                                        n1 <- sum(df_sub$g == g1_name); n2 <- sum(df_sub$g == g2_name)
                                        se_dunn <- sqrt((N_tot * (N_tot + 1) / 12 - tie_adj / (12 * (N_tot - 1))) * (1/n1 + 1/n2))
                                        z_dunn <- if (se_dunn > 0) abs(r1_mean - r2_mean) / se_dunn else 0
                                        p_ph <- 2 * stats::pnorm(z_dunn, lower.tail = FALSE)
                                        ph_raw_p <- c(ph_raw_p, p_ph)
                                    }
                                } else {
                                    if (is_normal) {
                                        ph_t <- tryCatch(stats::t.test(y ~ g, data = sub_d, var.equal = FALSE), error = function(e) NULL)
                                        p_ph <- if (!is.null(ph_t)) ph_t$p.value else NA_real_
                                    } else {
                                        ph_t <- tryCatch(stats::wilcox.test(y ~ g, data = sub_d, exact = FALSE), error = function(e) NULL)
                                        p_ph <- if (!is.null(ph_t)) ph_t$p.value else NA_real_
                                    }
                                    ph_raw_p <- c(ph_raw_p, p_ph)
                                }
                                ph_pairs_list[[length(ph_pairs_list) + 1]] <- list(i = i_idx, j = j_idx)
                            }

                            final_adj <- if (length(ph_adj_p_direct) == length(ph_raw_p)) {
                                ph_adj_p_direct
                            } else {
                                adj_m <- switch(self$options$postHocMethod, holm = "holm", bonferroni = "bonferroni", fdr = "BH", tukey_dunn = "holm", "holm")
                                stats::p.adjust(ph_raw_p, method = adj_m)
                            }

                            for (idx_p in seq_along(ph_pairs_list)) {
                                ph_pairs_list[[idx_p]]$p_adj <- final_adj[idx_p]
                            }
                            post_hoc <- ph_pairs_list
                        }
                    }
                }
            } else {
                f_vec <- as.factor(vec)
                tab <- table(f_vec, grp)
                if (length(tab) > 0 && nrow(tab) >= 2 && ncol(tab) >= 2) {
                    if (self$options$catTest == "fisher") {
                        ft <- tryCatch(stats::fisher.test(tab, simulate.p.value = (nrow(tab) > 2 || ncol(tab) > 2)), error = function(e) NULL)
                        if (!is.null(ft)) pval <- ft$p.value
                    } else if (self$options$catTest == "chisq") {
                        ct <- tryCatch(stats::chisq.test(tab, correct = FALSE), error = function(e) NULL)
                        if (!is.null(ct)) pval <- ct$p.value
                    } else {
                        ct_check <- tryCatch(stats::chisq.test(tab, correct = FALSE), error = function(e) NULL)
                        exp_c <- if (!is.null(ct_check)) ct_check$expected else NULL
                        if (!is.null(exp_c) && any(exp_c < 5)) {
                            ft <- tryCatch(stats::fisher.test(tab, simulate.p.value = (nrow(tab) > 2 || ncol(tab) > 2)), error = function(e) stats::chisq.test(tab, correct = TRUE))
                            if (!is.null(ft)) pval <- ft$p.value
                        } else {
                            ct <- tryCatch(stats::chisq.test(tab, correct = FALSE), error = function(e) NULL)
                            if (!is.null(ct)) pval <- ct$p.value
                        }
                    }

                    if (isTRUE(self$options$showPostHoc) && length(grp_levels) >= 3) {
                        pairs <- utils::combn(grp_levels, 2, simplify = FALSE)
                        ph_raw_p <- numeric(0)
                        ph_pairs_list <- list()

                        for (pr in pairs) {
                            sub_mask <- grp %in% pr & !is.na(f_vec)
                            tab_pr <- table(f_vec[sub_mask], factor(grp[sub_mask], levels = pr))
                            ft_pr <- tryCatch(stats::fisher.test(tab_pr, simulate.p.value = (nrow(tab_pr) > 2)), error = function(e) stats::chisq.test(tab_pr))
                            p_ph <- if (!is.null(ft_pr)) ft_pr$p.value else NA_real_
                            ph_raw_p <- c(ph_raw_p, p_ph)
                            i_idx <- which(grp_levels == pr[1])
                            j_idx <- which(grp_levels == pr[2])
                            ph_pairs_list[[length(ph_pairs_list) + 1]] <- list(i = i_idx, j = j_idx)
                        }

                        adj_m <- switch(self$options$postHocMethod, holm = "holm", bonferroni = "bonferroni", fdr = "BH", tukey_dunn = "holm", dscf = "holm", "holm")
                        ph_adj_p <- stats::p.adjust(ph_raw_p, method = adj_m)

                        for (idx_p in seq_along(ph_pairs_list)) {
                            ph_pairs_list[[idx_p]]$p_adj <- ph_adj_p[idx_p]
                        }
                        post_hoc <- ph_pairs_list
                    }
                }
            }

            list(pval = pval, test_name = test_name, post_hoc = post_hoc, is_cat = is_cat)
        },

        .getJamoviGlobalSettings = function() {
            paths <- c(
                file.path(Sys.getenv("APPDATA"), "jamovi", "settings.json"),
                file.path(Sys.getenv("USERPROFILE"), "AppData", "Roaming", "jamovi", "settings.json"),
                file.path(Sys.getenv("HOME"), ".config", "jamovi", "settings.json"),
                file.path(Sys.getenv("HOME"), "Library", "Application Support", "jamovi", "settings.json"),
                file.path(Sys.getenv("HOME"), "AppData", "Roaming", "jamovi", "settings.json")
            )
            res <- list(
                decMark = if (identical(getOption("OutDec"), ",")) "," else ".",
                digits = 2,
                pDigits = 3
            )
            for (p in paths) {
                if (file.exists(p)) {
                    tryCatch({
                        txt <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = " ")
                        if (requireNamespace("jsonlite", quietly = TRUE)) {
                            content <- jsonlite::fromJSON(txt)
                            if (!is.null(content$main)) {
                                if (!is.null(content$main$decSymbol) && nzchar(content$main$decSymbol)) {
                                    res$decMark <- content$main$decSymbol
                                }
                                if (!is.null(content$main$format)) {
                                    fmt <- if (is.character(content$main$format)) {
                                        jsonlite::fromJSON(content$main$format)
                                    } else {
                                        content$main$format
                                    }
                                    if (!is.null(fmt$n) && is.numeric(fmt$n) && fmt$n >= 0) {
                                        res$digits <- as.integer(fmt$n)
                                    }
                                    if (!is.null(fmt$p) && is.numeric(fmt$p) && fmt$p >= 0) {
                                        res$pDigits <- as.integer(fmt$p)
                                    }
                                }
                            }
                        } else {
                            m_dec <- regmatches(txt, regexec('"decSymbol"\\s*:\\s*"([^"]+)"', txt))
                            if (length(m_dec[[1]]) >= 2 && nzchar(m_dec[[1]][2])) {
                                res$decMark <- m_dec[[1]][2]
                            }
                            m_n <- regmatches(txt, regexec('"n"\\s*:\\s*([0-9]+)', txt))
                            if (length(m_n[[1]]) >= 2) {
                                res$digits <- as.integer(m_n[[1]][2])
                            }
                            m_p <- regmatches(txt, regexec('"p"\\s*:\\s*([0-9]+)', txt))
                            if (length(m_p[[1]]) >= 2) {
                                res$pDigits <- as.integer(m_p[[1]][2])
                            }
                        }
                        break
                    }, error = function(e) {})
                }
            }
            res
        }
    )
)
