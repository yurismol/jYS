# This file is a generated template, your changes will not be overwritten

mMOUTClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMOUTClass",
    inherit = mMOUTBase,
    private = list(

        .init = function() {
            private$.initOutputs()
        },

        .initOutputs = function() {
            description = function(part1) {
                return(
                    jmvcore::format(.("{varType} without outliers"),
                        varType=part1)
                )
            }

            title = function(part1=NULL, part2=NULL) {
                return(jmvcore::format("{} ({})", part2, part1))
            }

            if (self$options$remOut) {
                keys <- self$options$vars
                measureTypes <- sapply(keys, function(x) private$.columnType(self$data[[x]]))

                titles <- vapply(keys, function(key) title(.("outl"), key), '')
                descriptions <- vapply(keys, function(key) description(key), '')
                self$results$remOut$set(keys, titles, descriptions, measureTypes)
            }
        },

        .columnType = function(column) {
            if (inherits(column, "ordered")) {
                return("ordinal")
            } else if (inherits(column, "factor")) {
                return("nominal")
            } else {
                return("continuous")
            }
        },

        .decimalplaces=function(col) {
          maxd = 0
          for (x in col) {
            if (!is.na(x) && (x%%1)!= 0) {
              maxd = max(maxd, nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][2]))
            }
          }
          return(maxd)
        },

        .run = function() {
            vars <- self$options$vars
            group <- self$options$group
            method <- self$options$method
            alpha <- as.numeric(self$options$alpha)

            if (length(vars) < 2) {
                jmvcore::reject(.("Please select at least 2 variables for multivariate analysis."), code='')
            }

            data <- self$data
            keep_cols <- c(vars)
            if (!is.null(group)) {
                keep_cols <- c(keep_cols, group)
            }
            dat <- jmvcore::select(data, keep_cols)

            n_rows <- nrow(data)
            out_distances <- rep(NA_real_, n_rows)
            out_flags <- rep(NA_character_, n_rows)

            results_list <- list()
            warnings_list <- list()

            groups <- if (!is.null(group)) unique(dat[[group]]) else list(NULL)
            if (!is.null(group)) {
                groups <- groups[!is.na(groups)]
            }

            stat_table <- self$results$stat
            oind_table <- self$results$oind

            for (grp_val in groups) {
                if (!is.null(group)) {
                    grp_idx <- which(dat[[group]] == grp_val)
                    grp_dat <- dat[grp_idx, vars, drop = FALSE]
                } else {
                    grp_idx <- seq_len(n_rows)
                    grp_dat <- dat[, vars, drop = FALSE]
                }

                complete_cases <- which(complete.cases(grp_dat))
                grp_dat_complete <- grp_dat[complete_cases, , drop = FALSE]
                n_complete <- nrow(grp_dat_complete)
                p <- length(vars)

                cutoff <- qchisq(1 - alpha, df = p)

                d2 <- rep(NA_real_, nrow(grp_dat))
                is_outlier <- rep(FALSE, nrow(grp_dat))
                fallback_used <- FALSE

                if (n_complete > (p + 1)) {
                    if (method == "robust") {
                        mcd <- try(MASS::cov.mcd(grp_dat_complete), silent = TRUE)
                        if (inherits(mcd, "try-error")) {
                            fallback_used <- TRUE
                            center <- colMeans(grp_dat_complete)
                            cov_mat <- cov(grp_dat_complete)
                        } else {
                            center <- mcd$center
                            cov_mat <- mcd$cov
                        }
                    } else {
                        center <- colMeans(grp_dat_complete)
                        cov_mat <- cov(grp_dat_complete)
                    }

                    d2_comp <- try(mahalanobis(grp_dat_complete, center = center, cov = cov_mat), silent = TRUE)
                    if (inherits(d2_comp, "try-error")) {
                        if (method == "robust") {
                            center_c <- colMeans(grp_dat_complete)
                            cov_mat_c <- cov(grp_dat_complete)
                            d2_comp_c <- try(mahalanobis(grp_dat_complete, center = center_c, cov = cov_mat_c), silent = TRUE)
                            if (inherits(d2_comp_c, "try-error")) {
                                d2[complete_cases] <- NA_real_
                            } else {
                                fallback_used <- TRUE
                                d2[complete_cases] <- d2_comp_c
                            }
                        } else {
                            d2[complete_cases] <- NA_real_
                        }
                    } else {
                        d2[complete_cases] <- d2_comp
                    }
                } else {
                    if (n_complete >= 2) {
                        fallback_used <- TRUE
                        center <- colMeans(grp_dat_complete)
                        cov_mat <- cov(grp_dat_complete)
                        d2_comp <- try(mahalanobis(grp_dat_complete, center = center, cov = cov_mat), silent = TRUE)
                        if (!inherits(d2_comp, "try-error")) {
                            d2[complete_cases] <- d2_comp
                        }
                    }
                }

                is_outlier <- !is.na(d2) & (d2 > cutoff)
                noutl <- sum(is_outlier, na.rm = TRUE)
                pct <- if (n_complete > 0) noutl / n_complete else 0

                global_complete_idx <- grp_idx[complete_cases]
                out_distances[global_complete_idx] <- d2[complete_cases]
                
                out_flags[global_complete_idx] <- ifelse(is_outlier[complete_cases], "Outlier", "Normal")

                outlier_rows <- rownames(grp_dat)[which(is_outlier)]
                outlier_indices_str <- paste(outlier_rows, collapse = ", ")

                grp_label <- if (!is.null(group)) as.character(grp_val) else ""
                row_key <- if (!is.null(group)) as.character(grp_val) else "global"
                
                stat_table$addRow(rowKey = row_key, values = list(
                    grp = grp_label,
                    n = n_complete,
                    noutl = noutl,
                    pct = pct,
                    cutoff = cutoff
                ))

                oind_table$addRow(rowKey = row_key, values = list(
                    grp = grp_label,
                    count = noutl,
                    indices = outlier_indices_str
                ))

                if (fallback_used) {
                    warn_msg <- if (!is.null(group)) {
                        jmvcore::format(.("Group '{grp}': Robust MCD covariance estimation failed (possibly due to small sample size or collinearity). Classic Mahalanobis distance was used instead."), grp = grp_label)
                    } else {
                        .("Robust MCD covariance estimation failed (possibly due to small sample size or collinearity). Classic Mahalanobis distance was used instead.")
                    }
                    warnings_list <- c(warnings_list, warn_msg)
                }

                d2_classic <- rep(NA_real_, length(complete_cases))
                if (n_complete >= 2) {
                    center_c <- colMeans(grp_dat_complete)
                    cov_mat_c <- cov(grp_dat_complete)
                    d2_comp_c <- try(mahalanobis(grp_dat_complete, center = center_c, cov = cov_mat_c), silent = TRUE)
                    if (!inherits(d2_comp_c, "try-error")) {
                        d2_classic <- d2_comp_c
                    }
                }

                results_list[[row_key]] <- list(
                    grp = grp_label,
                    classic_d2 = d2_classic,
                    actual_d2 = d2[complete_cases],
                    is_outlier = is_outlier[complete_cases],
                    labels = rownames(grp_dat_complete),
                    cutoff = cutoff,
                    p = p
                )
            }

            if (length(warnings_list) > 0) {
                for (w in warnings_list) {
                    stat_table$setNote(w, w)
                }
            }

            if (self$options$outDist && self$results$outDist$isNotFilled()) {
                self$results$outDist$setRowNums(rownames(data))
                self$results$outDist$setValues(index = 1, out_distances)
            }
            if (self$options$outFlag && self$results$outFlag$isNotFilled()) {
                self$results$outFlag$setRowNums(rownames(data))
                self$results$outFlag$setValues(index = 1, factor(out_flags, levels = c("Normal", "Outlier")))
            }
            if (self$options$remOut && self$results$remOut$isNotFilled()) {
                self$results$remOut$setRowNums(rownames(data))
                outlier_indices <- which(out_flags == "Outlier")
                for (i in seq_along(vars)) {
                    key <- vars[[i]]
                    d <- data[[key]]
                    dec <- private$.decimalplaces(d)
                    d[outlier_indices] <- NA_real_
                    self$results$remOut$setValues(index = i, round(d, dec))
                }
            }

            if (self$options$showPlots) {
                self$results$ddplot$setState(results_list)
                self$results$qqplot$setState(results_list)
            }
        },

        .plotDD = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            plot_df <- data.frame()
            cutoff_df <- data.frame()

            for (row_key in names(state)) {
                grp_res <- state[[row_key]]
                n_points <- length(grp_res$actual_d2)
                if (n_points == 0) next

                classic_d2 <- grp_res$classic_d2
                robust_d2 <- grp_res$actual_d2

                lbl_normal <- .("Normal")
                lbl_classic_out <- .("Classic Outlier")
                lbl_robust_out <- .("Robust Outlier")
                lbl_both_out <- .("Outlier (Both)")

                classification <- rep(lbl_normal, n_points)
                cutoff <- grp_res$cutoff

                is_c_out <- !is.na(classic_d2) & (classic_d2 > cutoff)
                is_r_out <- !is.na(robust_d2) & (robust_d2 > cutoff)

                classification[is_c_out & !is_r_out] <- lbl_classic_out
                classification[!is_c_out & is_r_out] <- lbl_robust_out
                classification[is_c_out & is_r_out] <- lbl_both_out

                tmp_df <- data.frame(
                    Classic = classic_d2,
                    Robust = robust_d2,
                    Class = factor(classification, levels = c(lbl_normal, lbl_classic_out, lbl_robust_out, lbl_both_out)),
                    Label = grp_res$labels,
                    Group = grp_res$grp,
                    stringsAsFactors = FALSE
                )
                plot_df <- rbind(plot_df, tmp_df)

                cutoff_df <- rbind(cutoff_df, data.frame(
                    Group = grp_res$grp,
                    Cutoff = cutoff,
                    stringsAsFactors = FALSE
                ))
            }

            if (nrow(plot_df) == 0) return(FALSE)

            lbl_normal <- .("Normal")
            lbl_classic_out <- .("Classic Outlier")
            lbl_robust_out <- .("Robust Outlier")
            lbl_both_out <- .("Outlier (Both)")

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Classic, y = Robust)) +
                ggplot2::geom_point(ggplot2::aes(color = Class, shape = Class), size = 2.5, alpha = 0.8) +
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50")

            p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = Cutoff), data = cutoff_df, linetype = "dashed", color = "red", alpha = 0.7) +
                     ggplot2::geom_vline(ggplot2::aes(xintercept = Cutoff), data = cutoff_df, linetype = "dashed", color = "red", alpha = 0.7)

            outliers_df <- plot_df[plot_df$Class != lbl_normal, ]
            if (nrow(outliers_df) > 0) {
                p <- p + ggrepel::geom_text_repel(
                    ggplot2::aes(label = Label),
                    data = outliers_df,
                    size = 3,
                    color = "black",
                    box.padding = 0.3,
                    point.padding = 0.3,
                    max.overlaps = 20
                )
            }

            color_vals <- stats::setNames(
                c("#377eb8", "#ff7f00", "#984ea3", "#e41a1c"),
                c(lbl_normal, lbl_classic_out, lbl_robust_out, lbl_both_out)
            )
            shape_vals <- stats::setNames(
                c(16, 17, 15, 18),
                c(lbl_normal, lbl_classic_out, lbl_robust_out, lbl_both_out)
            )

            p <- p + ggplot2::scale_color_manual(values = color_vals) +
                     ggplot2::scale_shape_manual(values = shape_vals)

            has_group <- any(plot_df$Group != "")
            if (has_group) {
                p <- p + ggplot2::facet_wrap(~ Group)
            }

            p <- p + ggplot2::labs(
                title = .("Distance-Distance Plot"),
                x = .("Classical Squared Mahalanobis Distance"),
                y = .("Robust Squared Mahalanobis Distance"),
                color = NULL,
                shape = NULL
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
                panel.grid.major = ggplot2::element_line(linetype = "dashed", color = "#e0e0e0"),
                panel.grid.minor = ggplot2::element_blank(),
                legend.position = "bottom"
            )

            p <- p + ggtheme
            p <- p + ggplot2::theme(legend.position = "bottom")
            print(p)
            return(TRUE)
        },

        .plotQQ = function(image, ggtheme, theme, ...) {
            state <- image$state
            if (is.null(state)) return(FALSE)

            plot_df <- data.frame()
            cutoff_df <- data.frame()

            for (row_key in names(state)) {
                grp_res <- state[[row_key]]
                n_points <- length(grp_res$actual_d2)
                if (n_points == 0) next

                d2 <- grp_res$actual_d2
                cutoff <- grp_res$cutoff
                p_dim <- grp_res$p

                ord <- order(d2)
                sorted_d2 <- d2[ord]
                sorted_labels <- grp_res$labels[ord]

                i <- seq_len(n_points)
                probs <- (i - 0.5) / n_points
                theoretical <- qchisq(probs, df = p_dim)

                lbl_normal <- .("Normal")
                lbl_outlier <- .("Outlier")

                classification <- ifelse(sorted_d2 > cutoff, lbl_outlier, lbl_normal)

                tmp_df <- data.frame(
                    Theoretical = theoretical,
                    Empirical = sorted_d2,
                    Class = factor(classification, levels = c(lbl_normal, lbl_outlier)),
                    Label = sorted_labels,
                    Group = grp_res$grp,
                    stringsAsFactors = FALSE
                )
                plot_df <- rbind(plot_df, tmp_df)

                cutoff_df <- rbind(cutoff_df, data.frame(
                    Group = grp_res$grp,
                    Cutoff = cutoff,
                    stringsAsFactors = FALSE
                ))
            }

            if (nrow(plot_df) == 0) return(FALSE)

            lbl_normal <- .("Normal")
            lbl_outlier <- .("Outlier")

            p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Theoretical, y = Empirical)) +
                ggplot2::geom_point(ggplot2::aes(color = Class, shape = Class), size = 2.5, alpha = 0.8) +
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "gray50")

            p <- p + ggplot2::geom_hline(ggplot2::aes(yintercept = Cutoff), data = cutoff_df, linetype = "dashed", color = "red", alpha = 0.7)

            outliers_df <- plot_df[plot_df$Class == lbl_outlier, ]
            if (nrow(outliers_df) > 0) {
                p <- p + ggrepel::geom_text_repel(
                    ggplot2::aes(label = Label),
                    data = outliers_df,
                    size = 3,
                    color = "black",
                    box.padding = 0.3,
                    point.padding = 0.3,
                    max.overlaps = 20
                )
            }

            color_vals <- stats::setNames(
                c("#377eb8", "#e41a1c"),
                c(lbl_normal, lbl_outlier)
            )
            shape_vals <- stats::setNames(
                c(16, 18),
                c(lbl_normal, lbl_outlier)
            )

            p <- p + ggplot2::scale_color_manual(values = color_vals) +
                     ggplot2::scale_shape_manual(values = shape_vals)

            has_group <- any(plot_df$Group != "")
            if (has_group) {
                p <- p + ggplot2::facet_wrap(~ Group)
            }

            method_label <- if (self$options$method == "robust") .("Robust") else .("Classical")
            p <- p + ggplot2::labs(
                title = jmvcore::format(.("Chi-Square Q-Q Plot ({method})"), method = method_label),
                x = .("Theoretical Chi-Square Quantiles"),
                y = .("Empirical Squared Mahalanobis Distances"),
                color = NULL,
                shape = NULL
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
                panel.grid.major = ggplot2::element_line(linetype = "dashed", color = "#e0e0e0"),
                panel.grid.minor = ggplot2::element_blank(),
                legend.position = "bottom"
            )

            p <- p + ggtheme
            p <- p + ggplot2::theme(legend.position = "bottom")
            print(p)
            return(TRUE)
        }
    )
)
