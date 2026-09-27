
# ==============================================================================
# Helper: Unified ROC Curve Plot Renderer for Classifier Analyses
# Shared across mLR, mMLP, and mRF modules
# ==============================================================================

.getClassifierPalette <- function(n_classes,
                                  pal_brewer = "none",
                                  defaultTrainColor = "#3366B2",
                                  defaultCvColor = "#46B233") {
    base_colors <- c(defaultTrainColor, "#E54028", defaultCvColor, "#8A2BE2", "#FF8C00", "#C71585", "#008080", "#8B4513")
    if (pal_brewer != "none") {
        max_n <- switch(pal_brewer,
            "Accent" = 8, "Dark2" = 8, "Paired" = 12, "Pastel1" = 9,
            "Set1" = 9, "Set2" = 8, "Set3" = 12, 8)
        req_n <- max(3, min(n_classes, max_n))
        base_colors <- RColorBrewer::brewer.pal(n = req_n, name = pal_brewer)
        if (n_classes > max_n) {
            base_colors <- grDevices::colorRampPalette(base_colors)(n_classes)
        }
    }
    if (n_classes > length(base_colors)) {
        return(grDevices::rainbow(n_classes))
    }
    return(base_colors[1:n_classes])
}

.renderClassifierRocPlot <- function(image,
                                     ggtheme,
                                     theme,
                                     options,
                                     ensureDataFn = NULL,
                                     runFn = NULL,
                                     defaultTrainColor = "#3366B2",
                                     defaultCvColor = "#46B233",
                                     mainTitle = NULL,
                                     kfoldLabel = NULL) {
    tr <- function(text, n = 1) {
        if (!is.null(options) && is.function(options$translate)) {
            return(options$translate(text, n))
        }
        text
    }

    if (is.null(mainTitle)) mainTitle <- tr("ROC curves")
    if (is.null(kfoldLabel)) kfoldLabel <- tr("K-Fold CV (AUC =")

    roc_data <- image$state
    if (is.null(roc_data)) {
        # Fallback to parent array's state
        parent_state <- image$parent$state
        if (!is.null(parent_state) && !is.null(image$key)) {
            roc_data <- parent_state[[image$key]]
        }
    }
    if (is.null(roc_data)) return(FALSE)
    if (!requireNamespace("pROC", quietly = TRUE)) return(FALSE)

    # Ensure serialization does not distort matrices
    if (!is.null(roc_data$train_prob)) {
        roc_data$train_prob <- as.matrix(as.data.frame(roc_data$train_prob))
    }
    if (!is.null(roc_data$val_prob)) {
        roc_data$val_prob <- as.matrix(as.data.frame(roc_data$val_prob))
    }
    if (!is.null(roc_data$cv_prob)) {
        roc_data$cv_prob <- as.matrix(as.data.frame(roc_data$cv_prob))
    }

    result <- tryCatch({
        roc_x <- roc_data$roc_x
        roc_unit <- roc_data$roc_unit
        partition <- roc_data$partition
        is_pct <- roc_unit == "percent"
        legacy_axes <- (roc_x == "1spec")
        show_roc_cut <- isTRUE(roc_data$show_roc_cut)

        y_levels <- roc_data$y_levels
        C <- if (!is.null(roc_data$C)) roc_data$C else if (!is.null(y_levels)) length(y_levels) else 2

        pal_brewer <- if (!is.null(options) && !is.null(options$palBrewer)) options$palBrewer else "none"

        thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
        x_lab <- ifelse(is_pct, ifelse(legacy_axes, tr("100 - Specificity (%)"), tr("Specificity (%)")), ifelse(legacy_axes, tr("1 - Specificity"), tr("Specificity")))
        y_lab <- ifelse(is_pct, tr("Sensitivity (%)"), tr("Sensitivity"))

        if (roc_data$type == "binary") {
            # ----------------------------------------------------
            # Binary Plotting Logic (C == 2)
            # ----------------------------------------------------
            y_val_bin <- if (!is.null(roc_data$y)) as.numeric(roc_data$y) else NULL
            train_prob_bin <- if (!is.null(roc_data$train_prob)) as.numeric(roc_data$train_prob) else NULL
            val_prob_bin <- if (!is.null(roc_data$val_prob)) as.numeric(roc_data$val_prob) else NULL
            val_y_bin <- if (!is.null(roc_data$val_y)) as.numeric(roc_data$val_y) else NULL
            cv_prob_bin <- if (!is.null(roc_data$cv_prob)) as.numeric(roc_data$cv_prob) else NULL
            cv_y_bin <- if (!is.null(roc_data$cv_y)) as.numeric(roc_data$cv_y) else y_val_bin

            r_tr <- pROC::roc(y_val_bin, train_prob_bin, percent = is_pct, quiet = TRUE)
            cols <- c(defaultTrainColor)
            if (pal_brewer != "none") {
                cols <- RColorBrewer::brewer.pal(n = 3, name = pal_brewer)
            }
            active_cols <- c(cols[1])
            ltys <- c(1)         # Training is solid
            auc_tr_val <- as.numeric(pROC::auc(r_tr))
            auc_tr_str <- if (is_pct) paste0(round(auc_tr_val, 1), "%") else round(auc_tr_val, 3)
            leg_labels <- c(paste0(tr("Training (AUC ="), " ", auc_tr_str, ")"))

            p <- pROC::plot.roc(r_tr, col = cols[1],
                main = mainTitle, cex.main = 1.3,
                percent = is_pct,
                cex.lab = 1.5, cex.axis = 1.3, lwd = 3, lty = 1,
                legacy.axes = legacy_axes,
                xlab = x_lab,
                ylab = y_lab,
                print.thres = show_roc_cut,
                print.thres.col = cols[1],
                print.thres.pch = 19, print.thres.cex = 1.3,
                print.thres.best.method = "youden",
                print.thres.pattern = thres_pattern,
                grid = TRUE, add = FALSE
            )

            if (partition == "holdout" && !is.null(val_prob_bin)) {
                r_va <- pROC::roc(val_y_bin, val_prob_bin, percent = is_pct, quiet = TRUE)
                if (pal_brewer == "none") {
                    cols <- c(cols, "#E54028") # Holdout is Red
                }
                active_cols <- c(active_cols, cols[2])
                ltys <- c(ltys, 1)         # Holdout is solid
                auc_va_val <- as.numeric(pROC::auc(r_va))
                auc_va_str <- if (is_pct) paste0(round(auc_va_val, 1), "%") else round(auc_va_val, 3)
                leg_labels <- c(leg_labels, paste0(tr("Hold-out Validation (AUC ="), " ", auc_va_str, ")"))

                pROC::plot.roc(r_va, col = cols[2],
                    percent = is_pct,
                    lwd = 3, lty = 1,
                    legacy.axes = legacy_axes,
                    print.thres = show_roc_cut,
                    print.thres.col = cols[2],
                    print.thres.pch = 19, print.thres.cex = 1.3,
                    print.thres.best.method = "youden",
                    print.thres.pattern = thres_pattern,
                    add = TRUE
                )
            } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob_bin)) {
                r_cv <- pROC::roc(cv_y_bin, cv_prob_bin, percent = is_pct, quiet = TRUE)
                if (pal_brewer == "none") {
                    cols <- c(cols, defaultCvColor)
                }
                active_cols <- c(active_cols, cols[2])
                ltys <- c(ltys, 2)         # CV is dashed
                auc_cv_val <- as.numeric(pROC::auc(r_cv))
                auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                lbl <- if (partition == "repeated_kfold") tr("Repeated Stratified CV (AUC =") else kfoldLabel
                leg_labels <- c(leg_labels, paste0(lbl, " ", auc_cv_str, ")"))

                pROC::plot.roc(r_cv, col = cols[2],
                    percent = is_pct,
                    lwd = 3, lty = 2,
                    legacy.axes = legacy_axes,
                    print.thres = show_roc_cut,
                    print.thres.col = cols[2],
                    print.thres.pch = 19, print.thres.cex = 1.3,
                    print.thres.best.method = "youden",
                    print.thres.pattern = thres_pattern,
                    add = TRUE
                )
            }

            legend("bottomright",
                cex = 1.1, lwd = 3, col = active_cols,
                lty = ltys,
                bg = "white", box.lwd = 1,
                legend = leg_labels
            )

        } else if (roc_data$type == "combined_training") {
            # ----------------------------------------------------
            # Multiclass Combined Training Plotting Logic
            # ----------------------------------------------------
            y_val <- roc_data$y
            train_prob <- roc_data$train_prob
            cols <- .getClassifierPalette(C, pal_brewer = pal_brewer,
                                          defaultTrainColor = defaultTrainColor,
                                          defaultCvColor = defaultCvColor)
            leg_labels <- c()

            for (c in 1:C) {
                lev <- y_levels[c]
                y_tr_c <- if (is.character(y_val) || is.factor(y_val)) {
                    as.numeric(as.character(y_val) == as.character(lev))
                } else if (all(y_val %in% 1:C)) {
                    as.numeric(y_val == c)
                } else if (all(as.character(y_val) %in% as.character(y_levels))) {
                    as.numeric(as.character(y_val) == as.character(lev))
                } else {
                    as.numeric(y_val == c)
                }

                prob_tr_c <- if (!is.null(colnames(train_prob)) && lev %in% colnames(train_prob)) {
                    train_prob[, lev]
                } else {
                    train_prob[, c]
                }

                r_tr <- pROC::roc(y_tr_c, prob_tr_c, percent = is_pct, quiet = TRUE)
                auc_tr <- as.numeric(pROC::auc(r_tr))
                auc_tr_str <- if (is_pct) paste0(round(auc_tr, 1), "%") else round(auc_tr, 3)
                leg_labels <- c(leg_labels, paste0(lev, " (AUC = ", auc_tr_str, ")"))

                p <- pROC::plot.roc(r_tr, col = cols[c],
                    main = tr("Combined ROC - Training"), cex.main = 1.3,
                    percent = is_pct,
                    cex.lab = 1.5, cex.axis = 1.3, lwd = 3,
                    legacy.axes = legacy_axes,
                    xlab = x_lab,
                    ylab = y_lab,
                    print.thres = show_roc_cut,
                    print.thres.col = cols[c],
                    print.thres.pch = 19, print.thres.cex = 1.3,
                    print.thres.best.method = "youden",
                    print.thres.pattern = thres_pattern,
                    grid = (c == 1), add = (c > 1)
                )
            }

            legend("bottomright",
                cex = 1.1, lwd = 3, col = cols,
                bg = "white", box.lwd = 1,
                legend = leg_labels
            )

        } else if (roc_data$type == "combined_validation") {
            # ----------------------------------------------------
            # Multiclass Combined Validation/CV Plotting Logic
            # ----------------------------------------------------
            val_prob <- roc_data$val_prob
            val_y <- roc_data$val_y
            cv_prob <- roc_data$cv_prob
            cv_y <- roc_data$cv_y

            cols <- .getClassifierPalette(C, pal_brewer = pal_brewer,
                                          defaultTrainColor = defaultTrainColor,
                                          defaultCvColor = defaultCvColor)
            leg_labels <- c()
            plot_title <- if (partition %in% c("kfold", "repeated_kfold")) tr("Combined ROC - Cross-Validation") else tr("Combined ROC - Validation")
            lty_val <- if (partition %in% c("kfold", "repeated_kfold")) 2 else 1

            for (c in 1:C) {
                lev <- y_levels[c]
                if (partition == "holdout" && !is.null(val_prob) && !is.null(val_y)) {
                    y_va_c <- if (is.character(val_y) || is.factor(val_y)) {
                        as.numeric(as.character(val_y) == as.character(lev))
                    } else if (all(val_y %in% 1:C)) {
                        as.numeric(val_y == c)
                    } else if (all(as.character(val_y) %in% as.character(y_levels))) {
                        as.numeric(as.character(val_y) == as.character(lev))
                    } else {
                        as.numeric(val_y == c)
                    }

                    prob_va_c <- if (!is.null(colnames(val_prob)) && lev %in% colnames(val_prob)) {
                        val_prob[, lev]
                    } else {
                        val_prob[, c]
                    }

                    r_va <- pROC::roc(y_va_c, prob_va_c, percent = is_pct, quiet = TRUE)
                    auc_va <- as.numeric(pROC::auc(r_va))
                    auc_va_str <- if (is_pct) paste0(round(auc_va, 1), "%") else round(auc_va, 3)
                    leg_labels <- c(leg_labels, paste0(lev, " (AUC = ", auc_va_str, ")"))

                    p <- pROC::plot.roc(r_va, col = cols[c],
                        main = plot_title, cex.main = 1.3,
                        percent = is_pct,
                        cex.lab = 1.5, cex.axis = 1.3, lwd = 3, lty = lty_val,
                        legacy.axes = legacy_axes,
                        xlab = x_lab,
                        ylab = y_lab,
                        print.thres = show_roc_cut,
                        print.thres.col = cols[c],
                        print.thres.pch = 19, print.thres.cex = 1.3,
                        print.thres.best.method = "youden",
                        print.thres.pattern = thres_pattern,
                        grid = (c == 1), add = (c > 1)
                    )
                } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob) && !is.null(cv_y)) {
                    y_cv_c <- if (is.character(cv_y) || is.factor(cv_y)) {
                        as.numeric(as.character(cv_y) == as.character(lev))
                    } else if (all(cv_y %in% 1:C)) {
                        as.numeric(cv_y == c)
                    } else if (all(as.character(cv_y) %in% as.character(y_levels))) {
                        as.numeric(as.character(cv_y) == as.character(lev))
                    } else {
                        as.numeric(cv_y == c)
                    }

                    prob_cv_c <- if (!is.null(colnames(cv_prob)) && lev %in% colnames(cv_prob)) {
                        cv_prob[, lev]
                    } else {
                        cv_prob[, c]
                    }

                    r_cv <- pROC::roc(y_cv_c, prob_cv_c, percent = is_pct, quiet = TRUE)
                    auc_cv <- as.numeric(pROC::auc(r_cv))
                    auc_cv_str <- if (is_pct) paste0(round(auc_cv, 1), "%") else round(auc_cv, 3)
                    leg_labels <- c(leg_labels, paste0(lev, " (AUC = ", auc_cv_str, ")"))

                    p <- pROC::plot.roc(r_cv, col = cols[c],
                        main = plot_title, cex.main = 1.3,
                        percent = is_pct,
                        cex.lab = 1.5, cex.axis = 1.3, lwd = 3, lty = lty_val,
                        legacy.axes = legacy_axes,
                        xlab = x_lab,
                        ylab = y_lab,
                        print.thres = show_roc_cut,
                        print.thres.col = cols[c],
                        print.thres.pch = 19, print.thres.cex = 1.3,
                        print.thres.best.method = "youden",
                        print.thres.pattern = thres_pattern,
                        grid = (c == 1), add = (c > 1)
                    )
                }
            }

            if (length(leg_labels) > 0) {
                legend("bottomright",
                    cex = 1.1, lwd = 3, col = cols[1:length(leg_labels)],
                    lty = lty_val,
                    bg = "white", box.lwd = 1,
                    legend = leg_labels
                )
            }

        } else {
            # ----------------------------------------------------
            # Multiclass Separate Class Plotting Logic
            # ----------------------------------------------------
            class_name <- roc_data$class_name
            y_val_bin <- if (!is.null(roc_data$train_y)) as.numeric(roc_data$train_y) else NULL
            train_prob_bin <- if (!is.null(roc_data$train_prob)) as.numeric(roc_data$train_prob) else NULL
            val_prob_bin <- if (!is.null(roc_data$val_prob)) as.numeric(roc_data$val_prob) else NULL
            val_y_bin <- if (!is.null(roc_data$val_y)) as.numeric(roc_data$val_y) else NULL
            cv_prob_bin <- if (!is.null(roc_data$cv_prob)) as.numeric(roc_data$cv_prob) else NULL
            cv_y_bin <- if (!is.null(roc_data$cv_y)) as.numeric(roc_data$cv_y) else y_val_bin
            title_text <- jmvcore::format(tr("ROC Analysis for {class}"), class = class_name)

            r_tr <- pROC::roc(y_val_bin, train_prob_bin, percent = is_pct, quiet = TRUE)
            cols <- c(defaultTrainColor)
            if (pal_brewer != "none") {
                cols <- RColorBrewer::brewer.pal(n = 3, name = pal_brewer)
            }
            active_cols <- c(cols[1])
            ltys <- c(1)         # Training is solid
            auc_tr_val <- as.numeric(pROC::auc(r_tr))
            auc_tr_str <- if (is_pct) paste0(round(auc_tr_val, 1), "%") else round(auc_tr_val, 3)
            leg_labels <- c(paste0(tr("Training (AUC ="), " ", auc_tr_str, ")"))

            p <- pROC::plot.roc(r_tr, col = cols[1],
                main = title_text, cex.main = 1.3,
                percent = is_pct,
                cex.lab = 1.5, cex.axis = 1.3, lwd = 3, lty = 1,
                legacy.axes = legacy_axes,
                xlab = x_lab,
                ylab = y_lab,
                print.thres = show_roc_cut,
                print.thres.col = cols[1],
                print.thres.pch = 19, print.thres.cex = 1.3,
                print.thres.best.method = "youden",
                print.thres.pattern = thres_pattern,
                grid = TRUE, add = FALSE
            )

            if (partition == "holdout" && !is.null(val_prob_bin)) {
                r_va <- pROC::roc(val_y_bin, val_prob_bin, percent = is_pct, quiet = TRUE)
                if (pal_brewer == "none") {
                    cols <- c(cols, "#E54028") # Holdout is Red
                }
                active_cols <- c(active_cols, cols[2])
                ltys <- c(ltys, 1)         # Holdout is solid
                auc_va_val <- as.numeric(pROC::auc(r_va))
                auc_va_str <- if (is_pct) paste0(round(auc_va_val, 1), "%") else round(auc_va_val, 3)
                leg_labels <- c(leg_labels, paste0(tr("Hold-out Validation (AUC ="), " ", auc_va_str, ")"))

                pROC::plot.roc(r_va, col = cols[2],
                    percent = is_pct,
                    lwd = 3, lty = 1,
                    legacy.axes = legacy_axes,
                    print.thres = show_roc_cut,
                    print.thres.col = cols[2],
                    print.thres.pch = 19, print.thres.cex = 1.3,
                    print.thres.best.method = "youden",
                    print.thres.pattern = thres_pattern,
                    add = TRUE
                )
            } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob_bin)) {
                r_cv <- pROC::roc(cv_y_bin, cv_prob_bin, percent = is_pct, quiet = TRUE)
                if (pal_brewer == "none") {
                    cols <- c(cols, defaultCvColor)
                }
                active_cols <- c(active_cols, cols[2])
                ltys <- c(ltys, 2)         # CV is dashed
                auc_cv_val <- as.numeric(pROC::auc(r_cv))
                auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                lbl <- if (partition == "repeated_kfold") tr("Repeated Stratified CV (AUC =") else kfoldLabel
                leg_labels <- c(leg_labels, paste0(lbl, " ", auc_cv_str, ")"))

                pROC::plot.roc(r_cv, col = cols[2],
                    percent = is_pct,
                    lwd = 3, lty = 2,
                    legacy.axes = legacy_axes,
                    print.thres = show_roc_cut,
                    print.thres.col = cols[2],
                    print.thres.pch = 19, print.thres.cex = 1.3,
                    print.thres.best.method = "youden",
                    print.thres.pattern = thres_pattern,
                    add = TRUE
                )
            }

            legend("bottomright",
                cex = 1.1, lwd = 3, col = active_cols,
                lty = ltys,
                bg = "white", box.lwd = 1,
                legend = leg_labels
            )
        }

        if (exists("p") && !is.null(p)) {
            print(p)
        }
        return(TRUE)
    }, error = function(e) {
        return(FALSE)
    })
    return(result)
}
