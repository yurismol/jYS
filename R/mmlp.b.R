# This file is a generated template, your changes will not be overwritten

mMLPClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMLPClass",
    inherit = mMLPBase,
    private = list(
        
        .run = function() {
            dep <- self$options$dep
            covs <- self$options$covs
            factors <- self$options$factors

            if (is.null(dep)) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("Please select a dependent variable (target)."))
                return()
            }

            if (length(covs) == 0 && length(factors) == 0) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("Please select at least one predictor variable (factor or covariate)."))
                return()
            }

            # Prepare Data
            data_vars <- c(dep, covs, factors)
            clean_data <- na.omit(self$data[, data_vars, drop=FALSE])

            if (nrow(clean_data) < 10) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("The dataset has too few complete cases to train the neural network."))
                return()
            }

            y <- as.factor(clean_data[[dep]])
            y_levels <- levels(y)

            if (length(y_levels) != 2) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("The dependent variable must have exactly 2 levels (binary classification)."))
                return()
            }

            y_num <- ifelse(y == y_levels[2], 1, 0)

            # Standardize Continuous Covariates
            if (length(covs) > 0) {
                for (cov in covs) {
                    clean_data[[cov]] <- as.numeric(scale(clean_data[[cov]]))
                }
            }

            # Use composeFormula to build formula and model matrix safely
            formula_str <- jmvcore:::composeFormula(NULL, c(covs, factors))
            X_matrix <- model.matrix(as.formula(formula_str), data = clean_data)
            X_matrix <- X_matrix[, -1, drop = FALSE] # Drop intercept column

            # Parse Hidden Layer Structure
            hidden_str <- self$options$hidden_structure
            hidden_sizes <- try(as.integer(trimws(unlist(strsplit(hidden_str, ",")))), silent = TRUE)
            if (inherits(hidden_sizes, "try-error") || any(is.na(hidden_sizes)) || length(hidden_sizes) == 0 || any(hidden_sizes <= 0)) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("Invalid Hidden Layer Structure. Please enter comma-separated positive integers (e.g., 8 or 10,5)."))
                return()
            }

            hidden_activation <- self$options$activation
            out_activation <- self$options$out_activation

            decay <- as.numeric(self$options$decay)
            maxit <- as.integer(self$options$maxit)
            rang <- as.numeric(self$options$rang)
            val_split <- as.numeric(self$options$val_split) / 100
            cv_folds <- as.integer(self$options$cv_folds)

            # 1. Full Model Training
            fit_full <- try(private$.fit_mlp(
                X = X_matrix, 
                y = y_num, 
                hidden_sizes = hidden_sizes, 
                hidden_activation = hidden_activation, 
                out_activation = out_activation, 
                decay = decay, 
                maxit = maxit, 
                rang = rang
            ), silent = TRUE)

            if (inherits(fit_full, "try-error")) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(paste0(.("Error during neural network fitting: "), fit_full))
                return()
            }

            # Baseline Training Performance
            train_prob <- private$.predict_mlp(fit_full, X_matrix)
            train_pred <- ifelse(train_prob >= 0.5, 1, 0)
            train_acc <- mean(train_pred == y_num) * 100

            partition <- self$options$partition
            val_acc <- NA
            val_prob <- NULL
            val_pred <- NULL
            test_idx <- NULL
            cv_acc <- NA
            cv_pred_probs <- NULL
            cv_preds <- NULL
            cv_errors <- 0

            # 2. Hold-out Validation Split (Stratified)
            set.seed(42)
            idx0 <- which(y_num == 0)
            idx1 <- which(y_num == 1)
            
            n_test0 <- max(1, round(length(idx0) * val_split))
            n_test1 <- max(1, round(length(idx1) * val_split))
            
            test_idx0 <- sample(idx0, n_test0)
            test_idx1 <- sample(idx1, n_test1)
            test_idx <- c(test_idx0, test_idx1)
            train_idx <- setdiff(1:nrow(X_matrix), test_idx)

            fit_val <- try(private$.fit_mlp(
                X = X_matrix[train_idx, , drop = FALSE], 
                y = y_num[train_idx], 
                hidden_sizes = hidden_sizes, 
                hidden_activation = hidden_activation, 
                out_activation = out_activation, 
                decay = decay, 
                maxit = maxit, 
                rang = rang
            ), silent = TRUE)

            if (!inherits(fit_val, "try-error")) {
                val_prob <- private$.predict_mlp(fit_val, X_matrix[test_idx, , drop = FALSE])
                val_pred <- ifelse(val_prob >= 0.5, 1, 0)
                if (partition == "holdout") {
                    val_acc <- mean(val_pred == y_num[test_idx]) * 100
                }
            } else {
                val_acc <- NA
                val_prob <- NULL
            }

            # 3. Stratified K-Fold Cross-Validation
            if (partition == "kfold") {
                create_stratified_folds <- function(y_vec, k) {
                    idx_by_class <- split(1:length(y_vec), y_vec)
                    folds <- vector("list", k)
                    for (class_idx in idx_by_class) {
                        shuffled <- sample(class_idx)
                        groups <- cut(seq_along(shuffled), breaks = k, labels = FALSE)
                        for (i in 1:k) {
                            folds[[i]] <- c(folds[[i]], shuffled[groups == i])
                        }
                    }
                    return(folds)
                }

                folds <- create_stratified_folds(y_num, cv_folds)
                cv_pred_probs <- numeric(nrow(X_matrix))
                cv_preds <- numeric(nrow(X_matrix))
                cv_errors <- 0
                
                for (i in 1:cv_folds) {
                    t_idx <- folds[[i]]
                    tr_idx <- setdiff(1:nrow(X_matrix), t_idx)
                    
                    fit_cv <- try(private$.fit_mlp(
                        X = X_matrix[tr_idx, , drop = FALSE], 
                        y = y_num[tr_idx], 
                        hidden_sizes = hidden_sizes, 
                        hidden_activation = hidden_activation, 
                        out_activation = out_activation, 
                        decay = decay, 
                        maxit = maxit, 
                        rang = rang
                    ), silent = TRUE)

                    if (!inherits(fit_cv, "try-error")) {
                        p_prob <- private$.predict_mlp(fit_cv, X_matrix[t_idx, , drop = FALSE])
                        cv_pred_probs[t_idx] <- p_prob
                        cv_preds[t_idx] <- ifelse(p_prob >= 0.5, 1, 0)
                    } else {
                        cv_errors <- cv_errors + 1
                    }
                }

                if (cv_errors == 0) {
                    cv_acc <- mean(cv_preds == y_num) * 100
                } else {
                    cv_acc <- NA
                }
            } else {
                cv_errors <- 1 # CV not run for holdout
            }

            # Write Model Info Table
            info_table <- self$results$infoTable
            info_table$setRow(rowNo = 1, values = list(
                size = paste0(hidden_str, " (", hidden_activation, " / ", out_activation, ")"),
                decay = decay,
                maxit = maxit,
                trainAcc = train_acc,
                valAcc = val_acc,
                cvAcc = cv_acc
            ))

            # 4. Permutation Importance
            if (self$options$show_imp) {
                pred_vars <- c(covs, factors)
                imp_values <- numeric(length(pred_vars))
                
                for (v_idx in seq_along(pred_vars)) {
                    v_name <- pred_vars[v_idx]
                    shuffled_data <- clean_data
                    shuffled_data[[v_name]] <- sample(shuffled_data[[v_name]])
                    
                    # Convert to predictors matrix
                    X_shuffled <- model.matrix(as.formula(formula_str), data = shuffled_data)
                    X_shuffled <- X_shuffled[, -1, drop = FALSE]
                    
                    shuf_prob <- private$.predict_mlp(fit_full, X_shuffled)
                    shuf_pred <- ifelse(shuf_prob >= 0.5, 1, 0)
                    shuf_acc <- mean(shuf_pred == y_num) * 100
                    
                    # Permutation decrease in accuracy
                    imp_values[v_idx] <- max(0, train_acc - shuf_acc)
                }
                
                imp_df <- data.frame(
                    var = pred_vars,
                    imp = imp_values,
                    rank = rank(-imp_values, ties.method = "first")
                )
                imp_df <- imp_df[order(imp_df$rank), ]
                
                imp_table <- self$results$importanceTable
                for (row_idx in 1:nrow(imp_df)) {
                    imp_table$addRow(rowKey = row_idx, values = list(
                        var = imp_df$var[row_idx],
                        imp = imp_df$imp[row_idx],
                        rank = imp_df$rank[row_idx]
                    ))
                }
                
                # Save to plot state
                image_imp <- self$results$importancePlot
                image_imp$setState(imp_df)
            }

            # 5. Save Network Topology for topoPlot
            if (self$options$show_topo) {
                topo_data <- list(
                    W = fit_full$W,
                    b = fit_full$b,
                    layer_sizes = fit_full$layer_sizes,
                    input_names = colnames(X_matrix),
                    output_names = y_levels[2],
                    activations = fit_full$activations
                )
                image_topo <- self$results$topoPlot
                image_topo$setState(topo_data)
            }

            # 6. Learning Curve Calculation
            if (self$options$show_curve && !inherits(fit_val, "try-error")) {
                iter_seq <- seq(10, maxit, length.out = min(10, max(2, maxit / 10)))
                iter_seq <- unique(round(iter_seq))
                
                compute_cross_entropy <- function(y_vec, p_vec) {
                    p_vec <- pmin(pmax(p_vec, 1e-15), 1 - 1e-15)
                    mean(- (y_vec * log(p_vec) + (1 - y_vec) * log(1 - p_vec)))
                }
                
                train_losses <- numeric(length(iter_seq))
                val_losses <- numeric(length(iter_seq))
                
                for (it_idx in seq_along(iter_seq)) {
                    fit_temp <- try(private$.fit_mlp(
                        X = X_matrix[train_idx, , drop = FALSE], 
                        y = y_num[train_idx], 
                        hidden_sizes = hidden_sizes, 
                        hidden_activation = hidden_activation, 
                        out_activation = out_activation, 
                        decay = decay, 
                        maxit = iter_seq[it_idx], 
                        rang = rang
                    ), silent = TRUE)
                    
                    if (!inherits(fit_temp, "try-error")) {
                        p_tr <- private$.predict_mlp(fit_temp, X_matrix[train_idx, , drop = FALSE])
                        p_va <- private$.predict_mlp(fit_temp, X_matrix[test_idx, , drop = FALSE])
                        train_losses[it_idx] <- compute_cross_entropy(y_num[train_idx], p_tr)
                        val_losses[it_idx] <- compute_cross_entropy(y_num[test_idx], p_va)
                    } else {
                        train_losses[it_idx] <- NA
                        val_losses[it_idx] <- NA
                    }
                }
                
                curve_data <- data.frame(
                    Iteration = rep(iter_seq, 2),
                    Loss = c(train_losses, val_losses),
                    Dataset = rep(c("Training", "Validation"), each = length(iter_seq))
                )
                
                image_curve <- self$results$curvePlot
                image_curve$setState(curve_data)
            }

            # 7. Confusion Matrix & Performance Stats
            if (self$options$show_matrix) {
                can_run_matrix <- FALSE
                if (partition == "kfold" && cv_errors == 0) {
                    actual_val <- y_num
                    pred_val <- cv_preds
                    can_run_matrix <- TRUE
                } else if (partition == "holdout" && !is.null(val_pred)) {
                    actual_val <- y_num[test_idx]
                    pred_val <- val_pred
                    can_run_matrix <- TRUE
                }

                if (can_run_matrix) {
                    cm <- table(Actual = actual_val, Predicted = pred_val)
                    
                    neg_neg <- if ("0" %in% rownames(cm) && "0" %in% colnames(cm)) cm["0", "0"] else 0
                    neg_pos <- if ("0" %in% rownames(cm) && "1" %in% colnames(cm)) cm["0", "1"] else 0
                    pos_neg <- if ("1" %in% rownames(cm) && "0" %in% colnames(cm)) cm["1", "0"] else 0
                    pos_pos <- if ("1" %in% rownames(cm) && "1" %in% colnames(cm)) cm["1", "1"] else 0
                    
                    matrix_table <- self$results$matrixTable
                    matrix_table$getColumn("pred_neg")$setTitle(paste0(y_levels[1], " (", .("Predicted"), ")"))
                    matrix_table$getColumn("pred_pos")$setTitle(paste0(y_levels[2], " (", .("Predicted"), ")"))
                    
                    matrix_table$addRow(rowKey = 1, values = list(
                        actual = paste0(y_levels[1], " (", .("Actual"), ")"),
                        pred_neg = neg_neg,
                        pred_pos = neg_pos
                    ))
                    matrix_table$addRow(rowKey = 2, values = list(
                        actual = paste0(y_levels[2], " (", .("Actual"), ")"),
                        pred_neg = pos_neg,
                        pred_pos = pos_pos
                    ))
                    
                    # Performance stats table
                    cm_tr <- table(Actual = y_num, Predicted = train_pred)
                    t_neg_neg <- if ("0" %in% rownames(cm_tr) && "0" %in% colnames(cm_tr)) cm_tr["0", "0"] else 0
                    t_neg_pos <- if ("0" %in% rownames(cm_tr) && "1" %in% colnames(cm_tr)) cm_tr["0", "1"] else 0
                    t_pos_neg <- if ("1" %in% rownames(cm_tr) && "0" %in% colnames(cm_tr)) cm_tr["1", "0"] else 0
                    t_pos_pos <- if ("1" %in% rownames(cm_tr) && "1" %in% colnames(cm_tr)) cm_tr["1", "1"] else 0
                    
                    calc_metrics <- function(tn, fp, fn, tp) {
                        acc <- (tn + tp) / (tn + fp + fn + tp)
                        sen <- tp / (tp + fn)
                        spe <- tn / (tn + fp)
                        prec <- tp / (tp + fp)
                        f1 <- 2 * (prec * sen) / (prec + sen)
                        npv <- tn / (tn + fn)
                        
                        list(
                            acc = if (is.nan(acc)) 0 else acc,
                            sen = if (is.nan(sen)) 0 else sen,
                            spe = if (is.nan(spe)) 0 else spe,
                            prec = if (is.nan(prec)) 0 else prec,
                            f1 = if (is.nan(f1)) 0 else f1,
                            npv = if (is.nan(npv)) 0 else npv
                        )
                    }
                    
                    tr_metrics <- calc_metrics(t_neg_neg, t_neg_pos, t_pos_neg, t_pos_pos)
                    val_metrics_calculated <- calc_metrics(neg_neg, neg_pos, pos_neg, pos_pos)
                    
                    stats_table <- self$results$statsTable
                    if (partition == "kfold") {
                        stats_table$getColumn("cvVal")$setTitle(.("Cross-Validation"))
                    } else {
                        stats_table$getColumn("cvVal")$setTitle(.("Validation"))
                    }

                    is_pct <- self$options$roc_unit == "percent"
                    mult <- ifelse(is_pct, 100, 1)

                    metrics_names <- c(
                        .("Classification Accuracy"), 
                        .("Sensitivity / Recall"), 
                        .("Specificity"), 
                        .("Precision"), 
                        .("F1-Score"), 
                        .("Negative Predictive Value (NPV)")
                    )
                    metrics_keys <- c("acc", "sen", "spe", "prec", "f1", "npv")
                    
                    for (m_idx in seq_along(metrics_names)) {
                        k <- metrics_keys[m_idx]
                        stats_table$addRow(rowKey = m_idx, values = list(
                            metric = metrics_names[m_idx],
                            trainVal = tr_metrics[[k]] * mult,
                            cvVal = val_metrics_calculated[[k]] * mult
                        ))
                    }
                }
            }

            # 8. ROC Data Preparation
            if (self$options$show_roc) {
                can_run_roc <- FALSE
                if (partition == "kfold" && cv_errors == 0) {
                    can_run_roc <- TRUE
                } else if (partition == "holdout" && !inherits(fit_val, "try-error")) {
                    can_run_roc <- TRUE
                }

                if (can_run_roc) {
                    roc_data <- list(
                        y = y_num,
                        train_prob = as.numeric(train_prob),
                        val_prob = if (is.null(val_prob)) NULL else as.numeric(val_prob),
                        val_y = if (is.null(val_prob)) NULL else y_num[test_idx],
                        cv_prob = if (is.null(cv_pred_probs)) NULL else as.numeric(cv_pred_probs),
                        partition = partition,
                        roc_x = self$options$roc_x,
                        roc_unit = self$options$roc_unit
                    )
                    image_roc <- self$results$rocPlot
                    image_roc$setState(roc_data)
                }
            }
        },

        .importancePlot = function(image, ggtheme, theme, ...) {
            imp_df <- image$state
            if (is.null(imp_df)) return(FALSE)
            
            p <- ggplot2::ggplot(imp_df, ggplot2::aes(x = reorder(var, imp), y = imp)) +
                ggplot2::geom_col(fill = "#3366B2", width = 0.6) +
                ggplot2::geom_text(
                    ggplot2::aes(label = sprintf("%.2f%%", imp)),
                    hjust = -0.15,
                    size = 3.5,
                    fontface = "bold"
                ) +
                ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
                ggplot2::coord_flip() +
                ggplot2::labs(
                    title = .("Predictor Importance"),
                    x = .("Predictor"),
                    y = .("Mean Decrease in Accuracy (%)")
                ) +
                ggtheme
                
            print(p)
            return(TRUE)
        },

        .topoPlot = function(image, ggtheme, theme, ...) {
            topo <- image$state
            if (is.null(topo)) return(FALSE)
            
            W <- topo$W
            b <- topo$b
            layer_sizes <- topo$layer_sizes
            L <- length(layer_sizes) - 1
            
            activations <- topo$activations
            if (is.null(activations)) {
                activations <- c(rep("relu", L - 1), "sigmoid")
            }
            
            # Map X coordinates of layers
            x_coords <- 1:(L + 1)
            
            # Calculate coordinates for each node
            node_coords <- list()
            for (l in 1:(L+1)) {
                size_l <- layer_sizes[l]
                y_l <- seq(0.1, 0.9, length.out = size_l)
                if (size_l == 1) y_l <- 0.5
                node_coords[[l]] <- data.frame(
                    x = x_coords[l],
                    y = y_l,
                    label = if (l == 1) {
                        topo$input_names
                    } else if (l == L + 1) {
                        topo$output_names
                    } else {
                        paste0("H", l - 1, "_", 1:size_l)
                    }
                )
            }
            
            # Draw boundaries
            plot(1, type = "n", xlim = c(0.5, L + 1.5), ylim = c(-0.1, 1.1), axes = FALSE, xlab = "", ylab = "", main = .("Neural Network Architecture"))
            
            # Draw connection lines layer by layer
            for (l in 1:L) {
                w_l <- W[[l]]
                max_w_l <- max(abs(w_l), na.rm = TRUE)
                if (max_w_l == 0) max_w_l <- 1
                
                nodes_in <- node_coords[[l]]
                nodes_out <- node_coords[[l+1]]
                
                for (i in 1:nrow(nodes_in)) {
                    for (j in 1:nrow(nodes_out)) {
                        w <- w_l[i, j]
                        lwd_val <- abs(w) / max_w_l * 4 + 0.5
                        color_val <- ifelse(w > 0, "#E54028B2", "#3366B2B2") # positive: red, negative: blue
                        lines(c(nodes_in$x[i], nodes_out$x[j]), c(nodes_in$y[i], nodes_out$y[j]), col = color_val, lwd = lwd_val)
                    }
                }
            }
            
            # Label layers and activation functions
            text(1, 1.06, .("Input Layer"), font = 2, cex = 1.0)
            
            if (L > 1) {
                for (h_idx in 1:(L-1)) {
                    act_name <- activations[h_idx]
                    act_label <- if (act_name == "relu") "ReLU"
                                 else if (act_name == "sigmoid") .("Sigmoid")
                                 else if (act_name == "tanh") "tanh"
                                 else if (act_name == "softmax") "Softmax"
                                 else act_name
                    
                    text(h_idx + 1, 1.06, paste0(.("Hidden Layer"), " ", h_idx), font = 2, cex = 1.0)
                    text(h_idx + 1, 1.00, paste0("(", act_label, ")"), font = 1, cex = 0.85, col = "gray30")
                }
            }
            
            out_act_name <- activations[L]
            out_act_label <- if (out_act_name == "sigmoid") .("Sigmoid")
                             else if (out_act_name == "identity") .("Identity (Linear)")
                             else if (out_act_name == "softmax") "Softmax"
                             else out_act_name
            
            text(L + 1, 1.06, .("Output Layer"), font = 2, cex = 1.0)
            text(L + 1, 1.00, paste0("(", out_act_label, ")"), font = 1, cex = 0.85, col = "gray30")
            
            # Draw connection weights legend at the bottom center
            legend(
                "bottom", 
                legend = c(.("Positive Weight"), .("Negative Weight")), 
                col = c("#E54028", "#3366B2"), 
                lwd = 2, 
                horiz = TRUE, 
                bty = "n", 
                cex = 0.9
            )
            
            # Draw nodes circles and text labels
            for (l in 1:(L+1)) {
                nodes <- node_coords[[l]]
                bg_col <- "white"
                border_col <- if (l == L+1) "#E54028" else "#3366B2"
                cex_val <- if (l == L+1) 7 else 6
                
                points(nodes$x, nodes$y, pch = 21, bg = bg_col, col = border_col, cex = cex_val, lwd = 2)
                
                if (l == 1) {
                    text(nodes$x - 0.10, nodes$y, nodes$label, pos = 2, cex = 0.85, font = 2)
                } else if (l == L + 1) {
                    text(nodes$x + 0.10, nodes$y, nodes$label, pos = 4, cex = 0.95, font = 2)
                } else {
                    text(nodes$x, nodes$y, paste0("H", l - 1, ".", 1:nrow(nodes)), cex = 0.75, font = 2)
                }
            }
            
            return(TRUE)
        },

        .curvePlot = function(image, ggtheme, theme, ...) {
            curve_data <- image$state
            if (is.null(curve_data)) return(FALSE)
            
            # Localize the Dataset column values and swap the order in the legend (Validation first, then Training)
            curve_data$Dataset <- factor(
                ifelse(curve_data$Dataset == "Training", .("Training"), .("Validation")),
                levels = c(.("Validation"), .("Training"))
            )
            
            # Swap colors so Validation is blue and Training is red
            color_vals <- c("#3366B2", "#E54028")
            names(color_vals) <- c(.("Validation"), .("Training"))
            
            p <- ggplot2::ggplot(curve_data, ggplot2::aes(x = Iteration, y = Loss, color = Dataset)) +
                ggplot2::geom_line(linewidth = 1.2) +
                ggplot2::geom_point(size = 2) +
                ggplot2::scale_color_manual(values = color_vals) +
                ggplot2::labs(
                    title = .("MLP Classifier Learning Curve"),
                    x = .("Max Iterations (Epochs)"),
                    y = .("Cross-Entropy Loss")
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom")
                
            print(p)
            return(TRUE)
        },

        .rocPlot = function(image, ggtheme, theme, ...) {
            roc_data <- image$state
            if (is.null(roc_data)) return(FALSE)
            if (!requireNamespace("pROC", quietly = TRUE)) return(FALSE)
            
            roc_x <- roc_data$roc_x
            roc_unit <- roc_data$roc_unit
            partition <- roc_data$partition
            is_pct <- roc_unit == "percent"
            
            # Calculate Training ROC curve
            r_tr <- pROC::roc(roc_data$y, roc_data$train_prob, quiet = TRUE)
            tr_df <- data.frame(
                sens = r_tr$sensitivities,
                spec = r_tr$specificities
            )
            auc_tr_val <- pROC::auc(r_tr)
            auc_tr_str <- if (is_pct) paste0(round(auc_tr_val * 100, 1), "%") else round(auc_tr_val, 3)
            tr_df$Dataset <- paste0(.("Training (AUC = "), auc_tr_str, ")")
            
            get_optimal <- function(r_obj) {
                coords <- pROC::coords(r_obj, "best", best.method = "youden", quiet = TRUE)
                if (is.matrix(coords)) coords <- coords[1, ]
                data.frame(
                    sens = coords["sensitivity"],
                    spec = coords["specificity"],
                    threshold = coords["threshold"]
                )
            }
            
            opt_tr <- get_optimal(r_tr)
            opt_tr$Dataset <- tr_df$Dataset[1]
            
            df_all_list <- list(tr_df)
            df_opt_list <- list(opt_tr)
            colors_list <- c("#3366B2")
            
            # Add validation or cross-validation curves depending on partition method
            if (partition == "holdout" && !is.null(roc_data$val_prob)) {
                r_va <- pROC::roc(roc_data$val_y, roc_data$val_prob, quiet = TRUE)
                va_df <- data.frame(
                    sens = r_va$sensitivities,
                    spec = r_va$specificities
                )
                auc_va_val <- pROC::auc(r_va)
                auc_va_str <- if (is_pct) paste0(round(auc_va_val * 100, 1), "%") else round(auc_va_val, 3)
                va_df$Dataset <- paste0(.("Hold-out Validation (AUC = "), auc_va_str, ")")
                df_all_list[[2]] <- va_df
                
                opt_va <- get_optimal(r_va)
                opt_va$Dataset <- va_df$Dataset[1]
                df_opt_list[[2]] <- opt_va
                
                colors_list <- c("#3366B2", "#E54028") # Blue and Red
            } else if (partition == "kfold" && !is.null(roc_data$cv_prob)) {
                r_cv <- pROC::roc(roc_data$y, roc_data$cv_prob, quiet = TRUE)
                cv_df <- data.frame(
                    sens = r_cv$sensitivities,
                    spec = r_cv$specificities
                )
                auc_cv_val <- pROC::auc(r_cv)
                auc_cv_str <- if (is_pct) paste0(round(auc_cv_val * 100, 1), "%") else round(auc_cv_val, 3)
                cv_df$Dataset <- paste0(.("K-Fold Cross-Validation (AUC = "), auc_cv_str, ")")
                df_all_list[[2]] <- cv_df
                
                opt_cv <- get_optimal(r_cv)
                opt_cv$Dataset <- cv_df$Dataset[1]
                df_opt_list[[2]] <- opt_cv
                
                colors_list <- c("#3366B2", "#46B233") # Blue and Green
            }
            
            df_all <- do.call(rbind, df_all_list)
            df_opt <- do.call(rbind, df_opt_list)
            
            # Scaling for axis choice and units
            is_pct <- roc_unit == "percent"
            mult <- ifelse(is_pct, 100, 1)
            unit_label <- ifelse(is_pct, " (%)", "")
            
            if (roc_x == "1spec") {
                df_all$x_val <- (1 - df_all$spec) * mult
                df_opt$x_val <- (1 - df_opt$spec) * mult
                x_title <- .("1 - Specificity")
            } else {
                df_all$x_val <- df_all$spec * mult
                df_opt$x_val <- df_opt$spec * mult
                x_title <- .("Specificity")
            }
            
            df_all$y_val <- df_all$sens * mult
            df_opt$y_val <- df_opt$sens * mult
            
            # Coordinates for optimal cutoff labels
            df_opt$label <- paste0(
                "Cut-off = ", round(df_opt$threshold, 2), "\n",
                "Se = ", round(df_opt$sens * mult, 1), unit_label, "\n",
                "Sp = ", round(df_opt$spec * mult, 1), unit_label
            )
            
            # Create Plot
            p <- ggplot2::ggplot(df_all, ggplot2::aes(x = x_val, y = y_val, color = Dataset)) +
                ggplot2::geom_line(linewidth = 1.2) +
                ggplot2::geom_point(data = df_opt, ggplot2::aes(x = x_val, y = y_val), size = 3, shape = 21, stroke = 1.5, bg = "white") +
                ggrepel::geom_label_repel(
                    data = df_opt, 
                    ggplot2::aes(x = x_val, y = y_val, label = label),
                    size = 3.2, 
                    fontface = "bold", 
                    show.legend = FALSE,
                    box.padding = 0.5,
                    max.overlaps = Inf
                ) +
                ggplot2::scale_color_manual(values = colors_list) +
                ggplot2::labs(
                    title = .("ROC Analysis Comparison"),
                    x = paste0(x_title, unit_label),
                    y = paste0(.("Sensitivity"), unit_label)
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "bottom") +
                ggplot2::guides(color = ggplot2::guide_legend(ncol = 1, byrow = TRUE))
                
            # Formatting limits
            if (is_pct) {
                p <- p + ggplot2::xlim(ifelse(roc_x == "1spec", 0, 100), ifelse(roc_x == "1spec", 100, 0)) +
                    ggplot2::ylim(0, 100)
            } else {
                p <- p + ggplot2::xlim(ifelse(roc_x == "1spec", 0, 1), ifelse(roc_x == "1spec", 1, 0)) +
                    ggplot2::ylim(0, 1)
            }
            
            # Draw diagonal chance line
            if (roc_x == "1spec") {
                p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60")
            } else {
                p <- p + ggplot2::geom_abline(slope = -1, intercept = mult, linetype = "dashed", color = "gray60")
            }
            
            print(p)
            return(TRUE)
        },

        .fit_mlp = function(X, y, hidden_sizes, hidden_activation, out_activation, decay, maxit, rang, lr = 0.01) {
            N <- nrow(X)
            D <- ncol(X)
            
            layer_sizes <- c(D, hidden_sizes, 1)
            L <- length(layer_sizes) - 1
            
            activations <- c(rep(hidden_activation, L-1), out_activation)
            
            set.seed(42)
            W <- list()
            b <- list()
            
            for (l in 1:L) {
                n_in <- layer_sizes[l]
                n_out <- layer_sizes[l+1]
                
                act_l <- activations[l]
                
                if (act_l == "relu") {
                    W[[l]] <- matrix(rnorm(n_in * n_out, mean = 0, sd = sqrt(2 / n_in)), nrow = n_in, ncol = n_out)
                } else {
                    W[[l]] <- matrix(runif(n_in * n_out, min = -rang, max = rang), nrow = n_in, ncol = n_out)
                }
                b[[l]] <- matrix(0, nrow = 1, ncol = n_out)
            }
            
            sigmoid <- function(x) 1 / (1 + exp(-pmin(pmax(x, -50), 50)))
            tanh_act <- function(x) tanh(x)
            relu <- function(x) { x[x < 0] <- 0; return(x) }
            identity_act <- function(x) x
            softmax <- function(x) {
                if (ncol(x) == 1) {
                    return(1 / (1 + exp(-pmin(pmax(x, -50), 50))))
                }
                max_x <- apply(x, 1, max)
                exps <- exp(x - max_x)
                return(exps / rowSums(exps))
            }
            
            act_fun <- function(x, name) {
                if (name == "sigmoid") return(sigmoid(x))
                if (name == "tanh") return(tanh_act(x))
                if (name == "relu") return(relu(x))
                if (name == "identity") return(identity_act(x))
                if (name == "softmax") return(softmax(x))
                return(relu(x))
            }
            
            act_deriv <- function(a, z, name) {
                if (name == "sigmoid") return(a * (1 - a))
                if (name == "tanh") return(1 - a^2)
                if (name == "relu") {
                    res <- z
                    res[z > 0] <- 1
                    res[z <= 0] <- 0
                    return(res)
                }
                if (name == "identity") {
                    res <- z
                    res[] <- 1
                    return(res)
                }
                if (name == "softmax") {
                    return(a * (1 - a))
                }
                res <- z
                res[z > 0] <- 1
                res[z <= 0] <- 0
                return(res)
            }
            
            beta1 <- 0.9
            beta2 <- 0.999
            eps <- 1e-8
            
            mW <- lapply(W, function(w) matrix(0, nrow = nrow(w), ncol = ncol(w)))
            mb <- lapply(b, function(bi) matrix(0, nrow = nrow(bi), ncol = ncol(bi)))
            vW <- mW
            vb <- mb
            
            loss_history <- numeric(maxit)
            
            for (epoch in 1:maxit) {
                A <- list()
                Z <- list()
                
                A[[1]] <- X
                for (l in 1:L) {
                    Z[[l]] <- A[[l]] %*% W[[l]] + matrix(1, nrow = N, ncol = 1) %*% b[[l]]
                    A[[l+1]] <- act_fun(Z[[l]], activations[l])
                }
                
                p_out <- A[[L+1]]
                p_out <- pmin(pmax(p_out, 1e-15), 1 - 1e-15)
                
                bce_loss <- mean(- (y * log(p_out) + (1 - y) * log(1 - p_out)))
                reg_loss <- 0
                if (decay > 0) {
                    reg_loss <- (decay / (2 * N)) * sum(sapply(W, function(w) sum(w^2)))
                }
                loss_history[epoch] <- bce_loss + reg_loss
                
                dW <- list()
                db <- list()
                dZ <- list()
                
                dZ[[L]] <- A[[L+1]] - y
                dW[[L]] <- (t(A[[L]]) %*% dZ[[L]]) / N
                db[[L]] <- colMeans(dZ[[L]], na.rm = TRUE)
                dim(db[[L]]) <- c(1, length(db[[L]]))
                
                if (L > 1) {
                    for (l in (L-1):1) {
                        da <- dZ[[l+1]] %*% t(W[[l+1]])
                        dZ[[l]] <- da * act_deriv(A[[l+1]], Z[[l]], activations[l])
                        dW[[l]] <- (t(A[[l]]) %*% dZ[[l]]) / N
                        db[[l]] <- colMeans(dZ[[l]], na.rm = TRUE)
                        dim(db[[l]]) <- c(1, length(db[[l]]))
                    }
                }
                
                if (decay > 0) {
                    for (l in 1:L) {
                        dW[[l]] <- dW[[l]] + (decay / N) * W[[l]]
                    }
                }
                
                for (l in 1:L) {
                    mW[[l]] <- beta1 * mW[[l]] + (1 - beta1) * dW[[l]]
                    vW[[l]] <- beta2 * vW[[l]] + (1 - beta2) * (dW[[l]]^2)
                    mW_hat <- mW[[l]] / (1 - beta1^epoch)
                    vW_hat <- vW[[l]] / (1 - beta2^epoch)
                    W[[l]] <- W[[l]] - lr * mW_hat / (sqrt(vW_hat) + eps)
                    
                    mb[[l]] <- beta1 * mb[[l]] + (1 - beta1) * db[[l]]
                    vb[[l]] <- beta2 * vb[[l]] + (1 - beta2) * (db[[l]]^2)
                    mb_hat <- mb[[l]] / (1 - beta1^epoch)
                    vb_hat <- vb[[l]] / (1 - beta2^epoch)
                    b[[l]] <- b[[l]] - lr * mb_hat / (sqrt(vb_hat) + eps)
                }
            }
            
            list(
                W = W,
                b = b,
                hidden_sizes = hidden_sizes,
                hidden_activation = hidden_activation,
                out_activation = out_activation,
                activations = activations,
                loss_history = loss_history,
                layer_sizes = layer_sizes,
                L = L
            )
        },

        .predict_mlp = function(model, X) {
            N <- nrow(X)
            L <- model$L
            W <- model$W
            b <- model$b
            activations <- model$activations
            
            sigmoid <- function(x) 1 / (1 + exp(-pmin(pmax(x, -50), 50)))
            tanh_act <- function(x) tanh(x)
            relu <- function(x) { x[x < 0] <- 0; return(x) }
            identity_act <- function(x) x
            softmax <- function(x) {
                if (ncol(x) == 1) {
                    return(1 / (1 + exp(-pmin(pmax(x, -50), 50))))
                }
                max_x <- apply(x, 1, max)
                exps <- exp(x - max_x)
                return(exps / rowSums(exps))
            }
            
            act_fun <- function(x, name) {
                if (name == "sigmoid") return(sigmoid(x))
                if (name == "tanh") return(tanh_act(x))
                if (name == "relu") return(relu(x))
                if (name == "identity") return(identity_act(x))
                if (name == "softmax") return(softmax(x))
                return(relu(x))
            }
            
            A <- X
            for (l in 1:L) {
                Z <- A %*% W[[l]] + matrix(1, nrow = N, ncol = 1) %*% b[[l]]
                A <- act_fun(Z, activations[l])
            }
            return(A)
        }
    )
)
