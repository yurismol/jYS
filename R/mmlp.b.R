# This file is a generated template, your changes will not be overwritten

mMLPClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mMLPClass",
    inherit = mMLPBase,
    private = list(
        .init = function() {
            private$.initOutputs()
        },
        
        .initOutputs = function() {
            # Predict Class
            keys <- "predClass"
            titles <- .("Predicted Class")
            descriptions <- .("MLP Classifier Predicted Class")
            measureTypes <- "factor"
            self$results$predClass$set(keys, titles, descriptions, measureTypes)
            
            # Predict Prob
            keys <- "predProb"
            titles <- .("Predicted Probability")
            descriptions <- .("MLP Classifier Predicted Probability")
            measureTypes <- "continuous"
            self$results$predProb$set(keys, titles, descriptions, measureTypes)
        },
        
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

            if (length(y_levels) < 2) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("The dependent variable must have at least 2 levels."))
                return()
            }

            C <- length(y_levels)
            
            encode_target <- function(y_factor, num_classes) {
                if (num_classes == 2) {
                    return(ifelse(y_factor == levels(y_factor)[2], 1, 0))
                } else {
                    y_int <- as.integer(y_factor)
                    one_hot <- matrix(0, nrow = length(y_int), ncol = num_classes)
                    for (i in seq_along(y_int)) {
                        one_hot[i, y_int[i]] <- 1
                    }
                    colnames(one_hot) <- levels(y_factor)
                    return(one_hot)
                }
            }
            
            y_num <- encode_target(y, C)

            # Standardize Continuous Covariates
            if (length(covs) > 0) {
                for (cov in covs) {
                    clean_data[[cov]] <- as.numeric(scale(clean_data[[cov]]))
                }
            }

            # Use composeFormula to build formula and model matrix safely
            formula_str <- jmvcore::composeFormula(NULL, c(covs, factors))
            X_matrix <- model.matrix(as.formula(formula_str), data = clean_data)
            X_matrix <- X_matrix[, -1, drop = FALSE] # Drop intercept column
            colnames(X_matrix) <- gsub("^`|`$", "", colnames(X_matrix))
            colnames(X_matrix) <- gsub("`|\\\\", "", colnames(X_matrix))
            colnames(X_matrix) <- gsub('["\']', "", colnames(X_matrix))

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

            # Helpers for predictions and accuracies
            get_predictions <- function(prob, num_classes) {
                if (num_classes == 2) {
                    return(ifelse(prob >= 0.5, 1, 0))
                } else {
                    return(apply(prob, 1, which.max))
                }
            }

            get_accuracy <- function(pred, y_target, num_classes) {
                if (num_classes == 2) {
                    return(mean(pred == y_target) * 100)
                } else {
                    return(mean(pred == as.integer(y_target)) * 100)
                }
            }

            subset_target <- function(y_target, idx) {
                if (is.matrix(y_target)) {
                    return(y_target[idx, , drop = FALSE])
                } else {
                    return(y_target[idx])
                }
            }

            # Baseline Training Performance
            train_prob <- private$.predict_mlp(fit_full, X_matrix)
            train_pred <- get_predictions(train_prob, C)
            train_acc <- get_accuracy(train_pred, if (C == 2) y_num else y, C)

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
            set.seed(self$options$seed)
            test_idx_list <- list()
            for (c_idx in seq_along(y_levels)) {
                idx_c <- which(as.integer(y) == c_idx)
                n_test_c <- max(1, round(length(idx_c) * val_split))
                test_idx_list[[c_idx]] <- sample(idx_c, n_test_c)
            }
            test_idx <- unlist(test_idx_list)
            train_idx <- setdiff(1:nrow(X_matrix), test_idx)

            fit_val <- try(private$.fit_mlp(
                X = X_matrix[train_idx, , drop = FALSE], 
                y = subset_target(y_num, train_idx), 
                hidden_sizes = hidden_sizes, 
                hidden_activation = hidden_activation, 
                out_activation = out_activation, 
                decay = decay, 
                maxit = maxit, 
                rang = rang
            ), silent = TRUE)

            if (!inherits(fit_val, "try-error")) {
                val_prob <- private$.predict_mlp(fit_val, X_matrix[test_idx, , drop = FALSE])
                val_pred <- get_predictions(val_prob, C)
                if (partition == "holdout") {
                    val_acc <- get_accuracy(val_pred, if (C == 2) y_num[test_idx] else y[test_idx], C)
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

                folds <- create_stratified_folds(y, cv_folds)
                cv_pred_probs <- if (C == 2) numeric(nrow(X_matrix)) else matrix(0, nrow = nrow(X_matrix), ncol = C)
                cv_preds <- numeric(nrow(X_matrix))
                cv_errors <- 0
                
                for (i in 1:cv_folds) {
                    t_idx <- folds[[i]]
                    tr_idx <- setdiff(1:nrow(X_matrix), t_idx)
                    
                    fit_cv <- try(private$.fit_mlp(
                        X = X_matrix[tr_idx, , drop = FALSE], 
                        y = subset_target(y_num, tr_idx), 
                        hidden_sizes = hidden_sizes, 
                        hidden_activation = hidden_activation, 
                        out_activation = out_activation, 
                        decay = decay, 
                        maxit = maxit, 
                        rang = rang
                    ), silent = TRUE)

                    if (!inherits(fit_cv, "try-error")) {
                        p_prob <- private$.predict_mlp(fit_cv, X_matrix[t_idx, , drop = FALSE])
                        if (C == 2) {
                            cv_pred_probs[t_idx] <- p_prob
                        } else {
                            cv_pred_probs[t_idx, ] <- p_prob
                        }
                        cv_preds[t_idx] <- get_predictions(p_prob, C)
                    } else {
                        cv_errors <- cv_errors + 1
                    }
                }

                if (cv_errors == 0) {
                    cv_acc <- get_accuracy(cv_preds, if (C == 2) y_num else y, C)
                } else {
                    cv_acc <- NA
                }
            } else {
                cv_errors <- 1 # CV not run for holdout
            }

            # Write Model Info Table
            info_table <- self$results$infoTable
            
            fmt_pct <- function(val) {
                if (is.null(val) || is.na(val)) return("-")
                return(sprintf("%.2f%%", val))
            }
            
            metrics <- c(
                .("Hidden Layers"),
                .("Weight Decay"),
                .("Max Iterations"),
                .("Training Accuracy (%)"),
                .("Hold-out Accuracy (%)"),
                .("K-Fold CV Accuracy (%)")
            )
            
            values <- c(
                paste0(hidden_str, " (", hidden_activation, " / ", if (C > 2) "softmax" else out_activation, ")"),
                as.character(decay),
                as.character(maxit),
                fmt_pct(train_acc),
                if (partition == "holdout") fmt_pct(val_acc) else "-",
                if (partition == "kfold") fmt_pct(cv_acc) else "-"
            )
            
            for (i in 1:6) {
                info_table$setRow(rowNo = i, values = list(
                    metric = metrics[i],
                    value = values[i]
                ))
            }
            
            info_table$setNote("n1", .("Hidden Layers: network architecture (hidden layer sizes, hidden / output activation functions)"))
            info_table$setNote("n2", .("Weight Decay: L2 regularization penalty parameter."))
            info_table$setNote("n3", .("Max Iterations: maximum number of training epochs."))
            info_table$setNote("n4", .("Training Accuracy: classification accuracy on the training dataset."))
            if (partition == "holdout") {
                info_table$setNote("n5", .("Hold-out Accuracy: classification accuracy on the hold-out validation dataset."))
            } else {
                info_table$setNote("n5", NULL)
            }
            if (partition == "kfold") {
                info_table$setNote("n6", .("K-Fold CV Accuracy: average classification accuracy across cross-validation folds."))
            } else {
                info_table$setNote("n6", NULL)
            }

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
                    shuf_pred <- get_predictions(shuf_prob, C)
                    shuf_acc <- get_accuracy(shuf_pred, if (C == 2) y_num else y, C)
                    
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
                    output_names = if (C == 2) y_levels[2] else y_levels,
                    activations = fit_full$activations
                )
                image_topo <- self$results$topoPlot
                image_topo$setState(topo_data)
            }

            # 6. Learning Curve Calculation
            if (self$options$show_curve && !inherits(fit_val, "try-error")) {
                iter_seq <- seq(10, maxit, length.out = min(10, max(2, maxit / 10)))
                iter_seq <- unique(round(iter_seq))
                
                compute_cross_entropy <- function(y_target, p_pred) {
                    p_pred <- pmin(pmax(p_pred, 1e-15), 1 - 1e-15)
                    if (is.matrix(y_target) && ncol(y_target) > 1) {
                        mean(-rowSums(y_target * log(p_pred)))
                    } else {
                        mean(- (y_target * log(p_pred) + (1 - y_target) * log(1 - p_pred)))
                    }
                }
                
                train_losses <- numeric(length(iter_seq))
                val_losses <- numeric(length(iter_seq))
                
                for (it_idx in seq_along(iter_seq)) {
                    fit_temp <- try(private$.fit_mlp(
                        X = X_matrix[train_idx, , drop = FALSE], 
                        y = subset_target(y_num, train_idx), 
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
                        train_losses[it_idx] <- compute_cross_entropy(subset_target(y_num, train_idx), p_tr)
                        val_losses[it_idx] <- compute_cross_entropy(subset_target(y_num, test_idx), p_va)
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
                    actual_val <- if (C == 2) y_num else as.integer(y)
                    pred_val <- cv_preds
                    can_run_matrix <- TRUE
                } else if (partition == "holdout" && !is.null(val_pred)) {
                    actual_val <- if (C == 2) y_num[test_idx] else as.integer(y)[test_idx]
                    pred_val <- val_pred
                    can_run_matrix <- TRUE
                }

                if (can_run_matrix) {
                    actual_idx <- if (C == 2) actual_val + 1 else actual_val
                    pred_idx <- if (C == 2) pred_val + 1 else pred_val
                    
                    actual_labels <- y_levels[actual_idx]
                    pred_labels <- y_levels[pred_idx]
                    
                    cm_table <- table(
                        Actual = factor(actual_labels, levels = y_levels), 
                        Predicted = factor(pred_labels, levels = y_levels)
                    )
                    
                    matrix_table <- self$results$matrixTable
                    
                    row_idx <- 1
                    for (act_lev in y_levels) {
                        for (pred_lev in y_levels) {
                            cnt <- cm_table[act_lev, pred_lev]
                            matrix_table$addRow(rowKey = row_idx, values = list(
                                actual = act_lev,
                                predicted = pred_lev,
                                count = cnt
                            ))
                            row_idx <- row_idx + 1
                        }
                    }
                    
                    # Performance stats table
                    if (C == 2) {
                        neg_neg <- cm_table[y_levels[1], y_levels[1]]
                        neg_pos <- cm_table[y_levels[1], y_levels[2]]
                        pos_neg <- cm_table[y_levels[2], y_levels[1]]
                        pos_pos <- cm_table[y_levels[2], y_levels[2]]
                        
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
                    } else {
                        calc_metrics_multiclass <- function(actual, predicted, num_classes) {
                            acc <- mean(actual == predicted)
                            
                            se_list <- numeric(num_classes)
                            sp_list <- numeric(num_classes)
                            pr_list <- numeric(num_classes)
                            f1_list <- numeric(num_classes)
                            npv_list <- numeric(num_classes)
                            
                            for (c in 1:num_classes) {
                                tp <- sum(actual == c & predicted == c)
                                fp <- sum(actual != c & predicted == c)
                                fn <- sum(actual == c & predicted != c)
                                tn <- sum(actual != c & predicted != c)
                                
                                se_list[c] <- if ((tp + fn) > 0) tp / (tp + fn) else 0
                                sp_list[c] <- if ((tn + fp) > 0) tn / (tn + fp) else 0
                                pr_list[c] <- if ((tp + fp) > 0) tp / (tp + fp) else 0
                                f1_list[c] <- if ((se_list[c] + pr_list[c]) > 0) 2 * (se_list[c] * pr_list[c]) / (se_list[c] + pr_list[c]) else 0
                                npv_list[c] <- if ((tn + fn) > 0) tn / (tn + fn) else 0
                            }
                            
                            list(
                                acc = acc,
                                sen = mean(se_list),
                                spe = mean(sp_list),
                                prec = mean(pr_list),
                                f1 = mean(f1_list),
                                npv = mean(npv_list)
                            )
                        }
                        
                        tr_metrics <- calc_metrics_multiclass(as.integer(y), train_pred, C)
                        val_metrics_calculated <- calc_metrics_multiclass(actual_val, pred_val, C)
                    }
                    
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

                image_roc <- self$results$rocPlot
                image_roc$clear()
                parent_state <- list()

                if (can_run_roc) {
                    multiclass_roc_type <- self$options$multiclass_roc_type
                    
                    if (C == 2) {
                        # Binary Case: 1 plot showing both Training and Validation
                        roc_data <- list(
                            type = "binary",
                            C = C,
                            y_levels = y_levels,
                            y = y_num,
                            train_prob = train_prob,
                            val_prob = val_prob,
                            val_y = if (is.null(val_prob)) NULL else y_num[test_idx],
                            cv_prob = cv_pred_probs,
                            partition = partition,
                            roc_x = self$options$roc_x,
                            roc_unit = self$options$roc_unit,
                            show_roc_cut = self$options$show_roc_cut
                        )
                        image_roc$addItem(key = "binary")
                        image_item <- image_roc$get(key = "binary")
                        image_item$setTitle(.("ROC Analysis Comparison"))
                        image_item$setState(roc_data)
                        parent_state[["binary"]] <- roc_data
                        
                    } else if (multiclass_roc_type == "combined") {
                        # Multiclass Combined Case: 2 separate combined plots: Training and Validation
                        
                        # Plot 1: Combined Training
                        roc_data_tr <- list(
                            type = "combined_training",
                            C = C,
                            y_levels = y_levels,
                            y = as.integer(y),
                            train_prob = train_prob,
                            partition = partition,
                            roc_x = self$options$roc_x,
                            roc_unit = self$options$roc_unit,
                            show_roc_cut = self$options$show_roc_cut
                        )
                        image_roc$addItem(key = "combined_training")
                        image_item_tr <- image_roc$get(key = "combined_training")
                        image_item_tr$setTitle(.("Combined ROC - Training"))
                        image_item_tr$setState(roc_data_tr)
                        parent_state[["combined_training"]] <- roc_data_tr
                        
                        # Plot 2: Combined Validation/CV
                        roc_data_va <- list(
                            type = "combined_validation",
                            C = C,
                            y_levels = y_levels,
                            val_prob = val_prob,
                            val_y = if (is.null(val_prob)) NULL else as.integer(y)[test_idx],
                            cv_prob = cv_pred_probs,
                            cv_y = as.integer(y),
                            partition = partition,
                            roc_x = self$options$roc_x,
                            roc_unit = self$options$roc_unit,
                            show_roc_cut = self$options$show_roc_cut
                        )
                        image_roc$addItem(key = "combined_validation")
                        image_item_va <- image_roc$get(key = "combined_validation")
                        val_title <- if (partition == "kfold") .("Combined ROC - Cross-Validation") else .("Combined ROC - Validation")
                        image_item_va$setTitle(val_title)
                        image_item_va$setState(roc_data_va)
                        parent_state[["combined_validation"]] <- roc_data_va
                        
                    } else {
                        # Multiclass Separate Case: C plots, one for each class
                        for (c in 1:C) {
                            lev <- y_levels[c]
                            roc_data_c <- list(
                                type = "separate",
                                C = C,
                                class_name = lev,
                                train_y = as.numeric(as.integer(y) == c),
                                train_prob = train_prob[, c],
                                val_y = if (is.null(val_prob)) NULL else as.numeric(as.integer(y)[test_idx] == c),
                                val_prob = if (is.null(val_prob)) NULL else val_prob[, c],
                                cv_y = if (is.null(cv_pred_probs)) NULL else as.numeric(as.integer(y) == c),
                                cv_prob = if (is.null(cv_pred_probs)) NULL else cv_pred_probs[, c],
                                partition = partition,
                                roc_x = self$options$roc_x,
                                roc_unit = self$options$roc_unit,
                                show_roc_cut = self$options$show_roc_cut
                            )
                            image_roc$addItem(key = lev)
                            image_item <- image_roc$get(key = lev)
                            image_item$setTitle(jmvcore::format(.("ROC Analysis for {class}"), class = lev))
                            image_item$setState(roc_data_c)
                            parent_state[[lev]] <- roc_data_c
                        }
                    }
                    image_roc$setState(parent_state)
                    
                    # Populate ROC Table
                    if (self$options$show_roc_table && requireNamespace("pROC", quietly = TRUE)) {
                        rocTable <- self$results$rocTable
                        rocTable$deleteRows()
                        
                        is_pct <- self$options$roc_unit == "percent"
                        mult <- if (is_pct) 100 else 1
                        
                        add_roc_row <- function(row_name, y_true, y_prob) {
                            tryCatch({
                                if (is.null(y_true) || is.null(y_prob)) return()
                                
                                # Aggressively coerce to plain numeric vector
                                y_true <- as.double(as.vector(drop(unlist(y_true))))
                                y_prob <- as.double(as.vector(drop(unlist(y_prob))))
                                
                                if (length(y_true) != length(y_prob)) return()
                                complete_idx <- !is.na(y_true) & !is.na(y_prob)
                                if (sum(complete_idx) < 2) return()
                                y_t <- y_true[complete_idx]
                                y_p <- y_prob[complete_idx]
                                
                                r <- try(pROC::roc(y_t, y_p, quiet = TRUE), silent = TRUE)
                                if (inherits(r, "try-error")) return()
                                
                                auc_val <- as.numeric(pROC::auc(r)) * mult
                                
                                coords <- try(pROC::coords(r, x="best", best.method="youden", ret=c("threshold", "specificity", "sensitivity")), silent=TRUE)
                                if (inherits(coords, "try-error")) return()
                                
                                # pROC::coords returns data.frame in newer versions, matrix in older
                                if (is.data.frame(coords)) {
                                    cutoff <- as.numeric(coords[1, "threshold"])
                                    sp     <- as.numeric(coords[1, "specificity"]) * mult
                                    se     <- as.numeric(coords[1, "sensitivity"]) * mult
                                } else {
                                    if (is.matrix(coords)) {
                                        coords <- coords[1, ]
                                    }
                                    cutoff <- as.numeric(coords["threshold"])
                                    sp     <- as.numeric(coords["specificity"]) * mult
                                    se     <- as.numeric(coords["sensitivity"]) * mult
                                }
                                
                                direction <- as.character(r$direction)
                                auc_val <- as.numeric(auc_val)
                                
                                rocTable$addRow(rowKey = row_name, values = list(
                                    part = as.character(row_name),
                                    auc = auc_val,
                                    cutoff = cutoff,
                                    se = se,
                                    sp = sp,
                                    direction = direction
                                ))
                            }, error = function(e) {
                                # Silently skip if ROC table row fails
                            })
                        }
                        
                        if (C == 2) {
                            add_roc_row(.("Training"), y_num, train_prob)
                            if (partition == "holdout" && !is.null(val_prob)) {
                                add_roc_row(.("Hold-out Validation"), y_num[test_idx], val_prob)
                            } else if (partition == "kfold" && !is.null(cv_pred_probs)) {
                                add_roc_row(.("K-Fold Cross-Validation"), y_num, cv_pred_probs)
                            }
                        } else {
                            multiclass_roc_type <- self$options$multiclass_roc_type
                            if (multiclass_roc_type == "combined") {
                                for (c in 1:C) {
                                    lev <- y_levels[c]
                                    add_roc_row(paste0(lev, " - ", .("Train")), as.numeric(as.integer(y) == c), train_prob[, c])
                                }
                                if (partition == "holdout" && !is.null(val_prob)) {
                                    for (c in 1:C) {
                                        lev <- y_levels[c]
                                        add_roc_row(paste0(lev, " - ", .("Val")), as.numeric(as.integer(y)[test_idx] == c), val_prob[, c])
                                    }
                                } else if (partition == "kfold" && !is.null(cv_pred_probs)) {
                                    for (c in 1:C) {
                                        lev <- y_levels[c]
                                        add_roc_row(paste0(lev, " - ", .("CV")), as.numeric(as.integer(y) == c), cv_pred_probs[, c])
                                    }
                                }
                            } else {
                                for (c in 1:C) {
                                    lev <- y_levels[c]
                                    add_roc_row(paste0(lev, " - ", .("Train")), as.numeric(as.integer(y) == c), train_prob[, c])
                                    if (partition == "holdout" && !is.null(val_prob)) {
                                        add_roc_row(paste0(lev, " - ", .("Val")), as.numeric(as.integer(y)[test_idx] == c), val_prob[, c])
                                    } else if (partition == "kfold" && !is.null(cv_pred_probs)) {
                                        add_roc_row(paste0(lev, " - ", .("CV")), as.numeric(as.integer(y) == c), cv_pred_probs[, c])
                                    }
                                }
                            }
                        }
                    } else {
                        self$results$rocTable$deleteRows()
                    }
                }
            } else {
                self$results$rocPlot$clear()
                self$results$rocTable$deleteRows()
            }

            # 9. Save Predictions on Demand
            if ((self$options$predClass && self$results$predClass$isNotFilled()) ||
                (self$options$predProb && self$results$predProb$isNotFilled())) {
                
                pred_prob_col <- rep(NA, nrow(self$data))
                pred_class_col <- rep(NA, nrow(self$data))
                
                full_probs <- private$.predict_mlp(fit_full, X_matrix)
                full_preds_idx <- get_predictions(full_probs, C)
                
                if (C == 2) {
                    full_classes <- ifelse(full_preds_idx == 1, y_levels[2], y_levels[1])
                    full_prob_val <- as.numeric(full_probs)
                } else {
                    full_classes <- y_levels[full_preds_idx]
                    full_prob_val <- apply(full_probs, 1, max)
                }
                
                complete_case_indices <- as.integer(rownames(clean_data))
                pred_prob_col[complete_case_indices] <- full_prob_val
                pred_class_col[complete_case_indices] <- full_classes
                
                if (self$options$predClass && self$results$predClass$isNotFilled()) {
                    self$results$predClass$setRowNums(rownames(self$data))
                    self$results$predClass$setValues(index = 1, factor(pred_class_col, levels = y_levels))
                }
                
                if (self$options$predProb && self$results$predProb$isNotFilled()) {
                    self$results$predProb$setRowNums(rownames(self$data))
                    self$results$predProb$setValues(index = 1, pred_prob_col)
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
                y_l <- seq(0.9, 0.1, length.out = size_l)
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
            plot(1, type = "n", xlim = c(0.5, L + 2.0), ylim = c(-0.1, 1.1), axes = FALSE, xlab = "", ylab = "", main = .("Neural Network Architecture"))
            
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
                cex = 0.9,
                inset = 0.05
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
                C <- roc_data$C
                show_roc_cut <- isTRUE(roc_data$show_roc_cut)
                
                if (roc_data$type == "binary") {
                    # ----------------------------------------------------
                    # Binary Plotting Logic (C == 2)
                    # ----------------------------------------------------
                    y_val_bin <- if (!is.null(roc_data$y)) as.numeric(roc_data$y) else NULL
                    train_prob_bin <- if (!is.null(roc_data$train_prob)) as.numeric(roc_data$train_prob) else NULL
                    val_prob_bin <- if (!is.null(roc_data$val_prob)) as.numeric(roc_data$val_prob) else NULL
                    val_y_bin <- if (!is.null(roc_data$val_y)) as.numeric(roc_data$val_y) else NULL
                    cv_prob_bin <- if (!is.null(roc_data$cv_prob)) as.numeric(roc_data$cv_prob) else NULL
                    cv_y_bin <- y_val_bin
                    
                    r_tr <- pROC::roc(y_val_bin, train_prob_bin, percent = is_pct, quiet = TRUE)
                    cols <- c("#3366B2") # Training is always Blue
                    if (self$options$palBrewer != "none") {
                        cols <- RColorBrewer::brewer.pal(n=3, name=self$options$palBrewer)
                    }
                    active_cols <- c(cols[1])
                    ltys <- c(1)         # Training is solid
                    auc_tr_val <- as.numeric(pROC::auc(r_tr))
                    auc_tr_str <- if (is_pct) paste0(round(auc_tr_val, 1), "%") else round(auc_tr_val, 3)
                    leg_labels <- c(paste0(.("Training (AUC = "), auc_tr_str, ")"))
                    
                    thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
                    
                    p <- pROC::plot.roc(r_tr, col=cols[1],
                        main=.("ROC Analysis Comparison"), cex.main=1.3,
                        percent=is_pct,
                        cex.lab=1.5, cex.axis=1.3, lwd=3, lty=1,
                        legacy.axes=legacy_axes,
                        xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                        ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                        print.thres=show_roc_cut,
                        print.thres.col=cols[1],
                        print.thres.pch=19, print.thres.cex=1.3,
                        print.thres.best.method="youden",
                        print.thres.pattern=thres_pattern,
                        grid=TRUE, add=FALSE
                    )
                    
                    if (partition == "holdout" && !is.null(val_prob_bin)) {
                        r_va <- pROC::roc(val_y_bin, val_prob_bin, percent = is_pct, quiet = TRUE)
                        if (self$options$palBrewer == "none") {
                            cols <- c(cols, "#E54028") # Holdout is Red
                        }
                        active_cols <- c(active_cols, cols[2])
                        ltys <- c(ltys, 1)         # Holdout is solid
                        auc_va_val <- as.numeric(pROC::auc(r_va))
                        auc_va_str <- if (is_pct) paste0(round(auc_va_val, 1), "%") else round(auc_va_val, 3)
                        leg_labels <- c(leg_labels, paste0(.("Hold-out Validation (AUC = "), auc_va_str, ")"))
                        
                        pROC::plot.roc(r_va, col=cols[2],
                            percent=is_pct,
                            lwd=3, lty=1,
                            legacy.axes=legacy_axes,
                            print.thres=show_roc_cut,
                            print.thres.col=cols[2],
                            print.thres.pch=19, print.thres.cex=1.3,
                            print.thres.best.method="youden",
                            print.thres.pattern=thres_pattern,
                            add=TRUE
                        )
                    } else if (partition == "kfold" && !is.null(cv_prob_bin)) {
                        r_cv <- pROC::roc(cv_y_bin, cv_prob_bin, percent = is_pct, quiet = TRUE)
                        if (self$options$palBrewer == "none") {
                            cols <- c(cols, "#46B233") # CV is Green
                        }
                        active_cols <- c(active_cols, cols[2])
                        ltys <- c(ltys, 2)         # CV is dashed
                        auc_cv_val <- as.numeric(pROC::auc(r_cv))
                        auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                        leg_labels <- c(leg_labels, paste0(.("K-Fold Cross-Validation (AUC = "), auc_cv_str, ")"))
                        
                        pROC::plot.roc(r_cv, col=cols[2],
                            percent=is_pct,
                            lwd=3, lty=2,
                            legacy.axes=legacy_axes,
                            print.thres=show_roc_cut,
                            print.thres.col=cols[2],
                            print.thres.pch=19, print.thres.cex=1.3,
                            print.thres.best.method="youden",
                            print.thres.pattern=thres_pattern,
                            add=TRUE
                        )
                    }
                    
                    legend("bottomright",
                        cex=1.1, lwd=3, col=active_cols,
                        lty=ltys,
                        bg="white", box.lwd=1,
                        legend=leg_labels
                    )
                    
                } else if (roc_data$type == "combined_training") {
                    # ----------------------------------------------------
                    # Multiclass Combined Training Plotting Logic
                    # ----------------------------------------------------
                    y_val <- roc_data$y
                    train_prob <- roc_data$train_prob
                    y_levels <- roc_data$y_levels
                    
                    base_colors <- c("#3366B2", "#E54028", "#46B233", "#8A2BE2", "#FF8C00", "#C71585", "#008080", "#8B4513")
                    if (self$options$palBrewer != "none") {
                        max_n <- switch(self$options$palBrewer,
                            "Accent" = 8, "Dark2" = 8, "Paired" = 12, "Pastel1" = 9,
                            "Set1" = 9, "Set2" = 8, "Set3" = 12, 8)
                        req_n <- max(3, min(C, max_n))
                        base_colors <- RColorBrewer::brewer.pal(n=req_n, name=self$options$palBrewer)
                        if (C > max_n) {
                            base_colors <- grDevices::colorRampPalette(base_colors)(C)
                        }
                    }
                    if (C > length(base_colors)) {
                        cols <- grDevices::rainbow(C)
                    } else {
                        cols <- base_colors[1:C]
                    }
                    
                    leg_labels <- c()
                    thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
                    
                    for (c in 1:C) {
                        lev <- y_levels[c]
                        y_tr_c <- as.numeric(y_val == c)
                        prob_tr_c <- train_prob[, c]
                        r_tr <- pROC::roc(y_tr_c, prob_tr_c, percent = is_pct, quiet = TRUE)
                        auc_tr <- as.numeric(pROC::auc(r_tr))
                        auc_tr_str <- if (is_pct) paste0(round(auc_tr, 1), "%") else round(auc_tr, 3)
                        leg_labels <- c(leg_labels, paste0(lev, " (AUC = ", auc_tr_str, ")"))
                        
                        p <- pROC::plot.roc(r_tr, col=cols[c],
                            main=.("Combined ROC - Training"), cex.main=1.3,
                            percent=is_pct,
                            cex.lab=1.5, cex.axis=1.3, lwd=3,
                            legacy.axes=legacy_axes,
                            xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                            ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                            print.thres=show_roc_cut,
                            print.thres.col=cols[c],
                            print.thres.pch=19, print.thres.cex=1.3,
                            print.thres.best.method="youden",
                            print.thres.pattern=thres_pattern,
                            grid=(c == 1), add=(c > 1)
                        )
                    }
                    
                    legend("bottomright",
                        cex=1.1, lwd=3, col=cols,
                        bg="white", box.lwd=1,
                        legend=leg_labels
                    )
                    
                } else if (roc_data$type == "combined_validation") {
                    # ----------------------------------------------------
                    # Multiclass Combined Validation/CV Plotting Logic
                    # ----------------------------------------------------
                    val_prob <- roc_data$val_prob
                    val_y <- roc_data$val_y
                    cv_prob <- roc_data$cv_prob
                    cv_y <- roc_data$cv_y
                    y_levels <- roc_data$y_levels
                    
                    base_colors <- c("#3366B2", "#E54028", "#46B233", "#8A2BE2", "#FF8C00", "#C71585", "#008080", "#8B4513")
                    if (self$options$palBrewer != "none") {
                        max_n <- switch(self$options$palBrewer,
                            "Accent" = 8, "Dark2" = 8, "Paired" = 12, "Pastel1" = 9,
                            "Set1" = 9, "Set2" = 8, "Set3" = 12, 8)
                        req_n <- max(3, min(C, max_n))
                        base_colors <- RColorBrewer::brewer.pal(n=req_n, name=self$options$palBrewer)
                        if (C > max_n) {
                            base_colors <- grDevices::colorRampPalette(base_colors)(C)
                        }
                    }
                    if (C > length(base_colors)) {
                        cols <- grDevices::rainbow(C)
                    } else {
                        cols <- base_colors[1:C]
                    }
                    
                    leg_labels <- c()
                    thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
                    plot_title <- if (partition == "kfold") .("Combined ROC - Cross-Validation") else .("Combined ROC - Validation")
                    
                    for (c in 1:C) {
                        lev <- y_levels[c]
                        if (partition == "holdout" && !is.null(val_prob)) {
                            y_va_c <- as.numeric(val_y == c)
                            prob_va_c <- val_prob[, c]
                            r_va <- pROC::roc(y_va_c, prob_va_c, percent = is_pct, quiet = TRUE)
                            auc_va <- as.numeric(pROC::auc(r_va))
                            auc_va_str <- if (is_pct) paste0(round(auc_va, 1), "%") else round(auc_va, 3)
                            leg_labels <- c(leg_labels, paste0(lev, " (AUC = ", auc_va_str, ")"))
                            
                            p <- pROC::plot.roc(r_va, col=cols[c],
                                main=plot_title, cex.main=1.3,
                                percent=is_pct,
                                cex.lab=1.5, cex.axis=1.3, lwd=3,
                                legacy.axes=legacy_axes,
                                xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                                ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                                print.thres=show_roc_cut,
                                print.thres.col=cols[c],
                                print.thres.pch=19, print.thres.cex=1.3,
                                print.thres.best.method="youden",
                                print.thres.pattern=thres_pattern,
                                grid=(c == 1), add=(c > 1)
                            )
                        } else if (partition == "kfold" && !is.null(cv_prob)) {
                            y_cv_c <- as.numeric(cv_y == c)
                            prob_cv_c <- cv_prob[, c]
                            r_cv <- pROC::roc(y_cv_c, prob_cv_c, percent = is_pct, quiet = TRUE)
                            auc_cv <- as.numeric(pROC::auc(r_cv))
                            auc_cv_str <- if (is_pct) paste0(round(auc_cv, 1), "%") else round(auc_cv, 3)
                            leg_labels <- c(leg_labels, paste0(lev, " (AUC = ", auc_cv_str, ")"))
                            
                            p <- pROC::plot.roc(r_cv, col=cols[c],
                                main=plot_title, cex.main=1.3,
                                percent=is_pct,
                                cex.lab=1.5, cex.axis=1.3, lwd=3, lty=2,
                                legacy.axes=legacy_axes,
                                xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                                ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                                print.thres=show_roc_cut,
                                print.thres.col=cols[c],
                                print.thres.pch=19, print.thres.cex=1.3,
                                print.thres.best.method="youden",
                                print.thres.pattern=thres_pattern,
                                grid=(c == 1), add=(c > 1)
                            )
                        }
                    }
                    
                    legend("bottomright",
                        cex=1.1, lwd=3, col=cols,
                        lty=ifelse(partition == "kfold", 2, 1),
                        bg="white", box.lwd=1,
                        legend=leg_labels
                    )
                    
                } else {
                    # ----------------------------------------------------
                    # Multiclass Separate Class Plotting Logic
                    # ----------------------------------------------------
                    class_name <- roc_data$class_name
                    y_val_bin <- roc_data$train_y
                    train_prob_bin <- roc_data$train_prob
                    val_prob_bin <- roc_data$val_prob
                    val_y_bin <- roc_data$val_y
                    cv_prob_bin <- roc_data$cv_prob
                    cv_y_bin <- roc_data$cv_y
                    title_text <- jmvcore::format(.("ROC Analysis for {class}"), class = class_name)
                    
                    r_tr <- pROC::roc(y_val_bin, train_prob_bin, percent = is_pct, quiet = TRUE)
                    cols <- c("#3366B2") # Training is always Blue
                    if (self$options$palBrewer != "none") {
                        cols <- RColorBrewer::brewer.pal(n=3, name=self$options$palBrewer)
                    }
                    active_cols <- c(cols[1])
                    ltys <- c(1)         # Training is solid
                    auc_tr_val <- as.numeric(pROC::auc(r_tr))
                    auc_tr_str <- if (is_pct) paste0(round(auc_tr_val, 1), "%") else round(auc_tr_val, 3)
                    leg_labels <- c(paste0(.("Training (AUC = "), auc_tr_str, ")"))
                    
                    thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
                    
                    p <- pROC::plot.roc(r_tr, col=cols[1],
                        main=title_text, cex.main=1.3,
                        percent=is_pct,
                        cex.lab=1.5, cex.axis=1.3, lwd=3, lty=1,
                        legacy.axes=legacy_axes,
                        xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                        ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                        print.thres=show_roc_cut,
                        print.thres.col=cols[1],
                        print.thres.pch=19, print.thres.cex=1.3,
                        print.thres.best.method="youden",
                        print.thres.pattern=thres_pattern,
                        grid=TRUE, add=FALSE
                    )
                    
                    if (partition == "holdout" && !is.null(val_prob_bin)) {
                        r_va <- pROC::roc(val_y_bin, val_prob_bin, percent = is_pct, quiet = TRUE)
                        if (self$options$palBrewer == "none") {
                            cols <- c(cols, "#E54028") # Holdout is Red
                        }
                        active_cols <- c(active_cols, cols[2])
                        ltys <- c(ltys, 1)         # Holdout is solid
                        auc_va_val <- as.numeric(pROC::auc(r_va))
                        auc_va_str <- if (is_pct) paste0(round(auc_va_val, 1), "%") else round(auc_va_val, 3)
                        leg_labels <- c(leg_labels, paste0(.("Hold-out Validation (AUC = "), auc_va_str, ")"))
                        
                        pROC::plot.roc(r_va, col=cols[2],
                            percent=is_pct,
                            lwd=3, lty=1,
                            legacy.axes=legacy_axes,
                            print.thres=show_roc_cut,
                            print.thres.col=cols[2],
                            print.thres.pch=19, print.thres.cex=1.3,
                            print.thres.best.method="youden",
                            print.thres.pattern=thres_pattern,
                            add=TRUE
                        )
                    } else if (partition == "kfold" && !is.null(cv_prob_bin)) {
                        r_cv <- pROC::roc(cv_y_bin, cv_prob_bin, percent = is_pct, quiet = TRUE)
                        if (self$options$palBrewer == "none") {
                            cols <- c(cols, "#46B233") # CV is Green
                        }
                        active_cols <- c(active_cols, cols[2])
                        ltys <- c(ltys, 2)         # CV is dashed
                        auc_cv_val <- as.numeric(pROC::auc(r_cv))
                        auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                        leg_labels <- c(leg_labels, paste0(.("K-Fold Cross-Validation (AUC = "), auc_cv_str, ")"))
                        
                        pROC::plot.roc(r_cv, col=cols[2],
                            percent=is_pct,
                            lwd=3, lty=2,
                            legacy.axes=legacy_axes,
                            print.thres=show_roc_cut,
                            print.thres.col=cols[2],
                            print.thres.pch=19, print.thres.cex=1.3,
                            print.thres.best.method="youden",
                            print.thres.pattern=thres_pattern,
                            add=TRUE
                        )
                    }
                    
                    legend("bottomright",
                        cex=1.1, lwd=3, col=active_cols,
                        lty=ltys,
                        bg="white", box.lwd=1,
                        legend=leg_labels
                    )
                }
                
                print(p)
                return(TRUE)
            }, error = function(e) {
                return(FALSE)
            })
            return(result)
        },

        .fit_mlp = function(X, y, hidden_sizes, hidden_activation, out_activation, decay, maxit, rang, lr = 0.01) {
            N <- nrow(X)
            D <- ncol(X)
            
            C_out <- if (is.matrix(y)) ncol(y) else 1
            layer_sizes <- c(D, hidden_sizes, C_out)
            L <- length(layer_sizes) - 1
            
            activations <- c(rep(hidden_activation, L-1), out_activation)
            if (C_out > 1) {
                activations[L] <- "softmax"
            }
            
            set.seed(self$options$seed)
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
                
                if (C_out > 1) {
                    bce_loss <- mean(-rowSums(y * log(p_out)))
                } else {
                    bce_loss <- mean(- (y * log(p_out) + (1 - y) * log(1 - p_out)))
                }
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
