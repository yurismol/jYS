# This file is a generated template, your changes will not be overwritten

mLRClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mLRClass",
    inherit = mLRBase,
    private = list(
        
        .init = function() {
            private$.initOutputs()
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
                self$results$text$setContent(.("Please select at least one predictor variable."))
                return()
            }
            
            # Prepare Data
            data_vars <- c(dep, covs, factors)
            
            # Restrict dataset by group
            dat <- self$data
            subs <- ""
            if (!is.null(self$options$group) && !is.null(self$options$selgroup) && self$options$selgroup != "") {
                subs <- paste(self$options$group, " == \"", self$options$selgroup, "\"", sep="")
                self$results$text$setContent(paste0("<h4>", jmvcore::.("Dataset restricted to group: "), subs, "</h4>"))
                self$results$text$setVisible(TRUE)
                dat <- dat[dat[[self$options$group]] == self$options$selgroup, , drop=FALSE]
            } else {
                self$results$text$setVisible(FALSE)
            }
            
            clean_data <- na.omit(dat[, data_vars, drop=FALSE])
            
            if (nrow(clean_data) < 15) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("The dataset has too few complete cases to perform logistic regression (minimum 15 cases required)."))
                return()
            }
            
            y <- droplevels(as.factor(clean_data[[dep]]))
            y_levels <- levels(y)
            
            refLevel <- self$options$refLevel
            if (!is.null(refLevel) && refLevel != "" && refLevel %in% y_levels) {
                y <- relevel(y, ref = refLevel)
                y_levels <- levels(y)
            }
            
            num_levels <- length(y_levels)
            
            if (num_levels < 2) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("Dependent variable must have at least two levels."))
                return()
            }
            
            is_multinomial <- num_levels > 2
            
            if (!is_multinomial) {
                pos_class_label <- y_levels[2]
                neg_class_label <- y_levels[1]
                y_num <- ifelse(y == pos_class_label, 1, 0)
                y_target <- y_num
            } else {
                pos_class_label <- NULL
                neg_class_label <- y_levels[1]
                y_target <- y
            }
            
            # Build full model matrix X (intercept column dropped)
            formula_str <- jmvcore::composeFormula(NULL, c(covs, factors))
            X_all <- model.matrix(as.formula(formula_str), data = clean_data)
            X_all <- X_all[, -1, drop = FALSE] # Drop intercept
            colnames(X_all) <- gsub("^`|`$", "", colnames(X_all)) # Clean backticks
            
            method <- self$options$method
            alpha <- ifelse(method == "elastic", as.numeric(self$options$elastic_alpha), 1.0)
            lasso_choice <- self$options$lasso_penalty
            
            # Perform Feature Selection on Full Dataset
            selected_features <- private$.select_active_features(
                X = X_all,
                y = y_target,
                clean_data = clean_data,
                dep = dep,
                covs = covs,
                factors = factors,
                method = method,
                alpha = alpha,
                lasso_choice = lasso_choice
            )
            
            # Fit final Refitted / Post-Selection Model
            X_selected <- X_all[, selected_features, drop = FALSE]
            
            if (is_multinomial) {
                fit_full <- private$.fit_multinom(X_selected, y_target)
            } else {
                fit_full <- private$.fit_standard_glm(X_selected, y_num)
            }
            
            # Populate Model Coefficients Table
            private$.populate_coefficients_table(fit_full, X_selected, is_multinomial, y_levels)
            
            # Generate Forest Plot data if requested
            if (self$options$showForest) {
                forest_data <- private$.prepare_forest_data(fit_full, X_selected, is_multinomial, y_levels)
                self$results$forestPlot$setState(forest_data)
            }
            
            # Generate and render mathematical formula
            if (self$options$show_formula) {
                formula_html <- private$.generate_formula_html(fit_full, is_multinomial, y_levels)
                self$results$formulaHtml$setContent(formula_html)
                self$results$formulaHtml$setVisible(TRUE)
            } else {
                self$results$formulaHtml$setVisible(FALSE)
            }
            
            # ----------------------------------------------------
            # Validation & Cross-Validation Loops
            # ----------------------------------------------------
            partition <- self$options$partition
            val_split <- as.numeric(self$options$val_split) / 100
            cv_folds <- as.integer(self$options$cv_folds)
            cv_repeats <- as.integer(self$options$cv_repeats)
            
            # Performance Metric Helpers
            get_predictions <- function(prob) ifelse(prob >= 0.5, 1, 0)
            get_accuracy <- function(pred, target) mean(pred == target) * 100
            
            # Calculate Baseline Training Performance
            if (is_multinomial) {
                train_prob <- predict(fit_full, type = "probs")
                train_pred <- predict(fit_full, type = "class")
                train_acc <- mean(train_pred == y_target) * 100
                
                r_train_roc <- try(pROC::multiclass.roc(y_target, train_prob, quiet = TRUE), silent = TRUE)
                train_auc <- if (inherits(r_train_roc, "try-error")) NA else as.numeric(pROC::auc(r_train_roc))
            } else {
                train_prob <- predict(fit_full, type = "response")
                train_pred <- get_predictions(train_prob)
                train_acc <- get_accuracy(train_pred, y_num)
                
                r_train_roc <- try(pROC::roc(y_num, train_prob, quiet = TRUE), silent = TRUE)
                train_auc <- if (inherits(r_train_roc, "try-error")) NA else as.numeric(pROC::auc(r_train_roc))
            }
            
            val_acc <- NA
            val_auc <- NA
            val_prob <- NULL
            val_y <- NULL
            test_idx <- NULL
            
            cv_acc <- NA
            cv_auc <- NA
            cv_prob <- NULL
            cv_y <- NULL
            
            set.seed(self$options$seed)
            
            if (partition == "holdout") {
                # Stratified Train-Test Split (Hold-out)
                if (is_multinomial) {
                    test_idx <- c()
                    for (lvl in y_levels) {
                        idx_lvl <- which(y_target == lvl)
                        n_lvl_test <- max(1, round(length(idx_lvl) * val_split))
                        if (length(idx_lvl) > 1) {
                            test_idx <- c(test_idx, sample(idx_lvl, n_lvl_test))
                        } else {
                            test_idx <- c(test_idx, idx_lvl)
                        }
                    }
                } else {
                    idx_pos <- which(y_num == 1)
                    idx_neg <- which(y_num == 0)
                    
                    n_pos_test <- max(1, round(length(idx_pos) * val_split))
                    n_neg_test <- max(1, round(length(idx_neg) * val_split))
                    
                    test_pos <- sample(idx_pos, n_pos_test)
                    test_neg <- sample(idx_neg, n_neg_test)
                    
                    test_idx <- c(test_pos, test_neg)
                }
                
                train_idx <- setdiff(1:nrow(X_all), test_idx)
                
                # Fit model on training fold (including feature selection on training partition!)
                X_train_full <- X_all[train_idx, , drop = FALSE]
                y_train <- y_target[train_idx]
                clean_data_train <- clean_data[train_idx, , drop = FALSE]
                
                train_selected <- private$.select_active_features(
                    X = X_train_full,
                    y = y_train,
                    clean_data = clean_data_train,
                    dep = dep,
                    covs = covs,
                    factors = factors,
                    method = method,
                    alpha = alpha,
                    lasso_choice = lasso_choice
                )
                
                X_train_selected <- X_train_full[, train_selected, drop = FALSE]
                X_test_selected <- X_all[test_idx, train_selected, drop = FALSE]
                
                if (is_multinomial) {
                    fit_train <- private$.fit_multinom(X_train_selected, y_train)
                    val_prob <- predict(fit_train, newdata = as.data.frame(X_test_selected), type = "probs")
                    val_y <- y_target[test_idx]
                    val_pred <- predict(fit_train, newdata = as.data.frame(X_test_selected), type = "class")
                    val_acc <- mean(val_pred == val_y) * 100
                    
                    r_val_roc <- try(pROC::multiclass.roc(val_y, val_prob, quiet = TRUE), silent = TRUE)
                    val_auc <- if (inherits(r_val_roc, "try-error")) NA else as.numeric(pROC::auc(r_val_roc))
                } else {
                    fit_train <- private$.fit_standard_glm(X_train_selected, y_train)
                    
                    # Predict on test split
                    val_prob <- predict(fit_train, newdata = as.data.frame(X_test_selected), type = "response")
                    val_y <- y_num[test_idx]
                    val_pred <- get_predictions(val_prob)
                    val_acc <- get_accuracy(val_pred, val_y)
                    
                    r_val_roc <- try(pROC::roc(val_y, val_prob, quiet = TRUE), silent = TRUE)
                    val_auc <- if (inherits(r_val_roc, "try-error")) NA else as.numeric(pROC::auc(r_val_roc))
                }
                
            } else if (partition == "kfold") {
                # Stratified K-Fold Cross-Validation
                folds <- private$.create_stratified_folds(y_target, cv_folds)
                
                if (is_multinomial) {
                    cv_pred_probs <- matrix(NA, nrow = nrow(X_all), ncol = num_levels)
                    colnames(cv_pred_probs) <- y_levels
                    cv_pred_classes <- factor(rep(NA, nrow(X_all)), levels = y_levels)
                    cv_errors <- 0
                    
                    for (k in 1:cv_folds) {
                        t_idx <- folds[[k]]
                        tr_idx <- setdiff(1:nrow(X_all), t_idx)
                        
                        X_tr_full <- X_all[tr_idx, , drop = FALSE]
                        y_tr <- y_target[tr_idx]
                        clean_data_tr <- clean_data[tr_idx, , drop = FALSE]
                        
                        # Feature selection on fold training data
                        tr_selected <- private$.select_active_features(
                            X = X_tr_full,
                            y = y_tr,
                            clean_data = clean_data_tr,
                            dep = dep,
                            covs = covs,
                            factors = factors,
                            method = method,
                            alpha = alpha,
                            lasso_choice = lasso_choice
                        )
                        
                        X_tr_selected <- X_tr_full[, tr_selected, drop = FALSE]
                        X_te_selected <- X_all[t_idx, tr_selected, drop = FALSE]
                        
                        fit_cv_fold <- try(private$.fit_multinom(X_tr_selected, y_tr), silent = TRUE)
                        
                        if (!inherits(fit_cv_fold, "try-error")) {
                            p_prob <- predict(fit_cv_fold, newdata = as.data.frame(X_te_selected), type = "probs")
                            p_class <- predict(fit_cv_fold, newdata = as.data.frame(X_te_selected), type = "class")
                            if (length(t_idx) == 1) {
                                cv_pred_probs[t_idx, ] <- p_prob
                            } else {
                                cv_pred_probs[t_idx, ] <- p_prob
                            }
                            cv_pred_classes[t_idx] <- p_class
                        } else {
                            cv_errors <- cv_errors + 1
                        }
                    }
                    
                    if (cv_errors == 0) {
                        cv_prob <- cv_pred_probs
                        cv_y <- y_target
                        cv_acc <- mean(cv_pred_classes == cv_y) * 100
                        
                        r_cv_roc <- try(pROC::multiclass.roc(cv_y, cv_prob, quiet = TRUE), silent = TRUE)
                        cv_auc <- if (inherits(r_cv_roc, "try-error")) NA else as.numeric(pROC::auc(r_cv_roc))
                    }
                } else {
                    cv_pred_probs <- numeric(nrow(X_all))
                    cv_errors <- 0
                    
                    for (k in 1:cv_folds) {
                        t_idx <- folds[[k]]
                        tr_idx <- setdiff(1:nrow(X_all), t_idx)
                        
                        X_tr_full <- X_all[tr_idx, , drop = FALSE]
                        y_tr <- y_num[tr_idx]
                        clean_data_tr <- clean_data[tr_idx, , drop = FALSE]
                        
                        # Feature selection on fold training data
                        tr_selected <- private$.select_active_features(
                            X = X_tr_full,
                            y = y_tr,
                            clean_data = clean_data_tr,
                            dep = dep,
                            covs = covs,
                            factors = factors,
                            method = method,
                            alpha = alpha,
                            lasso_choice = lasso_choice
                        )
                        
                        X_tr_selected <- X_tr_full[, tr_selected, drop = FALSE]
                        X_te_selected <- X_all[t_idx, tr_selected, drop = FALSE]
                        
                        fit_cv_fold <- try(private$.fit_standard_glm(X_tr_selected, y_tr), silent = TRUE)
                        
                        if (!inherits(fit_cv_fold, "try-error")) {
                            p_prob <- predict(fit_cv_fold, newdata = as.data.frame(X_te_selected), type = "response")
                            cv_pred_probs[t_idx] <- p_prob
                        } else {
                            cv_errors <- cv_errors + 1
                        }
                    }
                    
                    if (cv_errors == 0) {
                        cv_prob <- cv_pred_probs
                        cv_y <- y_num
                        cv_pred <- get_predictions(cv_prob)
                        cv_acc <- get_accuracy(cv_pred, cv_y)
                        
                        r_cv_roc <- try(pROC::roc(cv_y, cv_prob, quiet = TRUE), silent = TRUE)
                        cv_auc <- if (inherits(r_cv_roc, "try-error")) NA else as.numeric(pROC::auc(r_cv_roc))
                    }
                }
                
            } else if (partition == "repeated_kfold") {
                # Repeated Stratified K-Fold CV
                if (is_multinomial) {
                    rep_accs <- numeric(cv_repeats)
                    rep_aucs <- numeric(cv_repeats)
                    subject_probs <- matrix(0, nrow = nrow(X_all), ncol = num_levels)
                    colnames(subject_probs) <- y_levels
                    cv_errors <- 0
                    
                    for (r in 1:cv_repeats) {
                        folds <- private$.create_stratified_folds(y_target, cv_folds)
                        fold_classes <- factor(rep(NA, nrow(X_all)), levels = y_levels)
                        fold_probs <- matrix(0, nrow = nrow(X_all), ncol = num_levels)
                        colnames(fold_probs) <- y_levels
                        
                        for (k in 1:cv_folds) {
                            t_idx <- folds[[k]]
                            tr_idx <- setdiff(1:nrow(X_all), t_idx)
                            
                            X_tr_full <- X_all[tr_idx, , drop = FALSE]
                            y_tr <- y_target[tr_idx]
                            clean_data_tr <- clean_data[tr_idx, , drop = FALSE]
                            
                            tr_selected <- private$.select_active_features(
                                X = X_tr_full,
                                y = y_tr,
                                clean_data = clean_data_tr,
                                dep = dep,
                                covs = covs,
                                factors = factors,
                                method = method,
                                alpha = alpha,
                                lasso_choice = lasso_choice
                            )
                            
                            X_tr_selected <- X_tr_full[, tr_selected, drop = FALSE]
                            X_te_selected <- X_all[t_idx, tr_selected, drop = FALSE]
                            
                            fit_cv_fold <- try(private$.fit_multinom(X_tr_selected, y_tr), silent = TRUE)
                            
                            if (!inherits(fit_cv_fold, "try-error")) {
                                p_prob <- predict(fit_cv_fold, newdata = as.data.frame(X_te_selected), type = "probs")
                                p_class <- predict(fit_cv_fold, newdata = as.data.frame(X_te_selected), type = "class")
                                fold_probs[t_idx, ] <- p_prob
                                fold_classes[t_idx] <- p_class
                                subject_probs[t_idx, ] <- subject_probs[t_idx, ] + p_prob
                            } else {
                                cv_errors <- cv_errors + 1
                            }
                        }
                        rep_accs[r] <- mean(fold_classes == y_target) * 100
                        r_rep_roc <- try(pROC::multiclass.roc(y_target, fold_probs, quiet = TRUE), silent = TRUE)
                        rep_aucs[r] <- if (inherits(r_rep_roc, "try-error")) NA else as.numeric(pROC::auc(r_rep_roc))
                    }
                    
                    if (cv_errors == 0) {
                        cv_acc <- mean(rep_accs, na.rm = TRUE)
                        cv_auc <- mean(rep_aucs, na.rm = TRUE)
                        cv_prob <- subject_probs / cv_repeats
                        cv_y <- y_target
                    }
                } else {
                    rep_accs <- numeric(cv_repeats)
                    rep_aucs <- numeric(cv_repeats)
                    subject_probs <- numeric(nrow(X_all))
                    cv_errors <- 0
                    
                    for (r in 1:cv_repeats) {
                        folds <- private$.create_stratified_folds(y_num, cv_folds)
                        fold_probs <- numeric(nrow(X_all))
                        
                        for (k in 1:cv_folds) {
                            t_idx <- folds[[k]]
                            tr_idx <- setdiff(1:nrow(X_all), t_idx)
                            
                            X_tr_full <- X_all[tr_idx, , drop = FALSE]
                            y_tr <- y_num[tr_idx]
                            clean_data_tr <- clean_data[tr_idx, , drop = FALSE]
                            
                            tr_selected <- private$.select_active_features(
                                X = X_tr_full,
                                y = y_tr,
                                clean_data = clean_data_tr,
                                dep = dep,
                                covs = covs,
                                factors = factors,
                                method = method,
                                alpha = alpha,
                                lasso_choice = lasso_choice
                            )
                            
                            X_tr_selected <- X_tr_full[, tr_selected, drop = FALSE]
                            X_te_selected <- X_all[t_idx, tr_selected, drop = FALSE]
                            
                            fit_cv_fold <- try(private$.fit_standard_glm(X_tr_selected, y_tr), silent = TRUE)
                            
                            if (!inherits(fit_cv_fold, "try-error")) {
                                p_prob <- predict(fit_cv_fold, newdata = as.data.frame(X_te_selected), type = "response")
                                fold_probs[t_idx] <- p_prob
                                subject_probs[t_idx] <- subject_probs[t_idx] + p_prob
                            } else {
                                cv_errors <- cv_errors + 1
                            }
                        }
                        r_pred <- get_predictions(fold_probs)
                        rep_accs[r] <- get_accuracy(r_pred, y_num)
                        
                        r_rep_roc <- try(pROC::roc(y_num, fold_probs, quiet = TRUE), silent = TRUE)
                        rep_aucs[r] <- if (inherits(r_rep_roc, "try-error")) NA else as.numeric(pROC::auc(r_rep_roc))
                    }
                    
                    if (cv_errors == 0) {
                        cv_acc <- mean(rep_accs, na.rm = TRUE)
                        cv_auc <- mean(rep_aucs, na.rm = TRUE)
                        cv_prob <- subject_probs / cv_repeats
                        cv_y <- y_num
                    }
                }
            }
            
            # Write Model Fit / Info Table
            private$.populate_info_table(
                method = method,
                alpha = alpha,
                lasso_choice = lasso_choice,
                partition = partition,
                val_split = val_split * 100,
                cv_folds = cv_folds,
                cv_repeats = cv_repeats,
                train_acc = train_acc,
                train_auc = train_auc,
                val_acc = val_acc,
                val_auc = val_auc,
                cv_acc = cv_acc,
                cv_auc = cv_auc,
                X_selected = X_selected,
                covs = covs,
                factors = factors,
                ref_level = y_levels[1]
            )
            
            # ----------------------------------------------------
            # Save Predictions on Demand
            # ----------------------------------------------------
            if ((self$options$predClass && self$results$predClass$isNotFilled()) ||
                (self$options$predProb && self$results$predProb$isNotFilled())) {
                
                # Fill predictions for all complete cases (keep missing rows as NA)
                pred_prob_col <- rep(NA, nrow(self$data))
                pred_class_col <- rep(NA, nrow(self$data))
                
                complete_case_indices <- as.integer(rownames(clean_data))
                
                if (is_multinomial) {
                    full_probs <- predict(fit_full, newdata = as.data.frame(X_selected), type = "probs")
                    full_classes <- predict(fit_full, newdata = as.data.frame(X_selected), type = "class")
                    max_probs <- apply(full_probs, 1, max)
                    pred_prob_col[complete_case_indices] <- max_probs
                    pred_class_col[complete_case_indices] <- as.character(full_classes)
                } else {
                    full_probs <- predict(fit_full, newdata = as.data.frame(X_selected), type = "response")
                    full_classes <- ifelse(full_probs >= 0.5, pos_class_label, neg_class_label)
                    pred_prob_col[complete_case_indices] <- full_probs
                    pred_class_col[complete_case_indices] <- full_classes
                }
                
                # Write to the spreadsheet
                if (self$options$predClass && self$results$predClass$isNotFilled()) {
                    self$results$predClass$setRowNums(rownames(self$data))
                    self$results$predClass$setValues(index = 1, factor(pred_class_col, levels = y_levels))
                }
                
                if (self$options$predProb && self$results$predProb$isNotFilled()) {
                    self$results$predProb$setRowNums(rownames(self$data))
                    self$results$predProb$setValues(index = 1, pred_prob_col)
                }
            }
            
            # ----------------------------------------------------
            # Save ROC State
            # ----------------------------------------------------
            if (self$options$show_roc) {
                image_roc <- self$results$rocPlot
                image_roc$clear()
                parent_state <- list()
                multiclass_roc_type <- self$options$multiclass_roc_type
                
                if (!is_multinomial) {
                    roc_data <- list(
                        type = "binary",
                        y_levels = y_levels,
                        y = y_target,
                        train_prob = train_prob,
                        val_prob = val_prob,
                        val_y = val_y,
                        cv_prob = cv_prob,
                        cv_y = cv_y,
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
                } else {
                    if (multiclass_roc_type == "combined") {
                        # Plot 1: Combined Training
                        roc_data_tr <- list(
                            type = "combined_training",
                            y_levels = y_levels,
                            y = y_target,
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
                            y_levels = y_levels,
                            val_prob = val_prob,
                            val_y = val_y,
                            cv_prob = cv_prob,
                            cv_y = cv_y,
                            partition = partition,
                            roc_x = self$options$roc_x,
                            roc_unit = self$options$roc_unit,
                            show_roc_cut = self$options$show_roc_cut
                        )
                        image_roc$addItem(key = "combined_validation")
                        image_item_va <- image_roc$get(key = "combined_validation")
                        val_title <- if (partition %in% c("kfold", "repeated_kfold")) .("Combined ROC - Cross-Validation") else .("Combined ROC - Validation")
                        image_item_va$setTitle(val_title)
                        image_item_va$setState(roc_data_va)
                        parent_state[["combined_validation"]] <- roc_data_va
                    } else {
                        num_classes <- length(y_levels)
                        for (i in 1:num_classes) {
                            lev <- y_levels[i]
                            roc_data_c <- list(
                                type = "separate",
                                class_name = lev,
                                train_y = as.numeric(y_target == lev),
                                train_prob = train_prob[, lev],
                                val_y = if (is.null(val_prob)) NULL else as.numeric(val_y == lev),
                                val_prob = if (is.null(val_prob)) NULL else val_prob[, lev],
                                cv_y = if (is.null(cv_prob)) NULL else as.numeric(cv_y == lev),
                                cv_prob = if (is.null(cv_prob)) NULL else cv_prob[, lev],
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
                    
                    if (!is_multinomial) {
                        add_roc_row(.("Training"), y_target, train_prob)
                        if (partition == "holdout" && !is.null(val_prob)) {
                            add_roc_row(.("Hold-out Validation"), val_y, val_prob)
                        } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob)) {
                            lbl <- if (partition == "kfold") .("K-Fold CV") else .("Repeated Stratified CV")
                            add_roc_row(lbl, cv_y, cv_prob)
                        }
                    } else {
                        num_classes <- length(y_levels)
                        for (i in 1:num_classes) {
                            class_label <- y_levels[i]
                            
                            # Training
                            y_bin <- ifelse(y_target == class_label, 1, 0)
                            prob_bin <- train_prob[, class_label]
                            add_roc_row(paste0(class_label, " vs Rest - ", .("Train")), y_bin, prob_bin)
                            
                            # Validation / CV
                            if (partition == "holdout" && !is.null(val_prob) && !is.null(val_y)) {
                                if ((is.matrix(val_prob) || is.data.frame(val_prob)) && class_label %in% colnames(val_prob)) {
                                    val_y_bin <- ifelse(val_y == class_label, 1, 0)
                                    val_prob_bin <- val_prob[, class_label]
                                    add_roc_row(paste0(class_label, " vs Rest - ", .("Val")), val_y_bin, val_prob_bin)
                                }
                            } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob) && !is.null(cv_y)) {
                                if ((is.matrix(cv_prob) || is.data.frame(cv_prob)) && class_label %in% colnames(cv_prob)) {
                                    cv_y_bin <- ifelse(cv_y == class_label, 1, 0)
                                    cv_prob_bin <- cv_prob[, class_label]
                                    lbl <- if (partition == "kfold") .("CV") else .("Repeated CV")
                                    add_roc_row(paste0(class_label, " vs Rest - ", lbl), cv_y_bin, cv_prob_bin)
                                }
                            }
                        }
                    }
                } else {
                    self$results$rocTable$deleteRows()
                }
            }
        },
        
        # ----------------------------------------------------
        # Helper: Feature Selection Algorithms
        # ----------------------------------------------------
        .select_active_features = function(X, y, clean_data, dep, covs, factors, method, alpha = 1.0, lasso_choice = "min") {
            all_features <- colnames(X)
            
            if (length(all_features) == 0) return(character(0))
            
            if (method == "manual") {
                return(all_features)
            }
            
            is_multi <- is.factor(y) && length(levels(y)) > 2
            
            if (method %in% c("forward", "backward")) {
                # Formula-based Stepwise Selection
                pred_terms <- c(covs, factors)
                null_formula <- as.formula(paste(jmvcore::composeFormula(dep, character(0)), "1"))
                full_formula <- as.formula(jmvcore::composeFormula(dep, pred_terms))
                
                # Fit temporary models
                if (is_multi) {
                    null_model <- try(nnet::multinom(null_formula, data = clean_data, trace = FALSE), silent = TRUE)
                    full_model <- try(nnet::multinom(full_formula, data = clean_data, trace = FALSE), silent = TRUE)
                } else {
                    null_model <- try(glm(null_formula, data = clean_data, family = binomial), silent = TRUE)
                    full_model <- try(glm(full_formula, data = clean_data, family = binomial), silent = TRUE)
                }
                
                if (inherits(null_model, "try-error") || inherits(full_model, "try-error")) {
                    return(all_features) # Fallback to all
                }
                
                # Perform stepwise selection
                step_direction <- if (method == "forward") "forward" else "backward"
                fit_step <- try(stats::step(
                    object = if (method == "forward") null_model else full_model,
                    scope = list(lower = null_model, upper = full_model),
                    direction = step_direction,
                    trace = 0
                ), silent = TRUE)
                
                if (inherits(fit_step, "try-error")) {
                    return(all_features) # Fallback to all
                }
                
                # Extract selected terms and reconstruct active columns in model matrix
                selected_terms <- attr(terms(fit_step), "term.labels")
                
                # Find all columns of model matrix X that correspond to the selected terms
                active_cols <- c()
                for (term in selected_terms) {
                    # Handle factor dummy variable prefixes or direct matching
                    matches <- grep(paste0("^", term), all_features, value = TRUE)
                    # If direct match or prefix dummy match
                    if (length(matches) > 0) {
                        active_cols <- c(active_cols, matches)
                    }
                }
                
                active_cols <- unique(active_cols)
                if (length(active_cols) == 0) return(all_features) # Fallback to all if empty
                return(active_cols)
            }
            
            if (method %in% c("lasso", "elastic")) {
                # Regularized Selection using glmnet
                # Alpha = 1 is Lasso, Alpha = [0, 1] is ElasticNet
                # Minimum 3 folds for cross-validation
                n_folds <- max(3, min(10, floor(length(y) / 5)))
                
                fit_cv <- try(glmnet::cv.glmnet(
                    x = X,
                    y = y,
                    family = if (is_multi) "multinomial" else "binomial",
                    alpha = alpha,
                    nfolds = n_folds
                ), silent = TRUE)
                
                if (inherits(fit_cv, "try-error")) {
                    return(all_features) # Fallback to all
                }
                
                # Choose optimal lambda
                lambda_opt <- if (lasso_choice == "min") fit_cv$lambda.min else fit_cv$lambda.1se
                
                # Extract active coefficients
                if (is_multi) {
                    coef_list <- coef(fit_cv, s = lambda_opt)
                    active_coef_names <- c()
                    for (class_coef in coef_list) {
                        coefs_matrix <- as.matrix(class_coef)
                        active <- rownames(coefs_matrix)[coefs_matrix[, 1] != 0]
                        active_coef_names <- union(active_coef_names, active)
                    }
                    active_coef_names <- setdiff(active_coef_names, "(Intercept)")
                } else {
                    coefs <- as.matrix(coef(fit_cv, s = lambda_opt))
                    active_coef_names <- rownames(coefs)[coefs[, 1] != 0]
                    active_coef_names <- setdiff(active_coef_names, "(Intercept)")
                }
                
                if (length(active_coef_names) == 0) {
                    if (is_multi) {
                        coef_list <- coef(fit_cv, s = fit_cv$lambda.min)
                        for (class_coef in coef_list) {
                            raw_coefs <- as.matrix(class_coef)
                            mag_coefs <- abs(raw_coefs[-1, 1, drop = FALSE])
                            largest_var <- rownames(mag_coefs)[which.max(mag_coefs)]
                            if (length(largest_var) > 0) active_coef_names <- union(active_coef_names, largest_var)
                        }
                        active_coef_names <- setdiff(active_coef_names, "(Intercept)")
                        if (length(active_coef_names) == 0) return(all_features)
                    } else {
                        raw_coefs <- as.matrix(coef(fit_cv, s = fit_cv$lambda.min))
                        mag_coefs <- abs(raw_coefs[-1, 1, drop = FALSE])
                        largest_var <- rownames(mag_coefs)[which.max(mag_coefs)]
                        if (length(largest_var) > 0) active_coef_names <- largest_var
                        else return(all_features)
                    }
                }
                
                return(active_coef_names)
            }
            
            return(all_features)
        },
        
        # ----------------------------------------------------
        # Helper: Fit Standard Binomial GLM
        # ----------------------------------------------------
        .fit_standard_glm = function(X_selected, y_num) {
            df_fit <- as.data.frame(X_selected)
            df_fit$y_num <- y_num
            
            if (ncol(X_selected) == 0) {
                # Intercept-only null model formula using composeFormula
                formula_str <- paste(jmvcore::composeFormula("y_num", character(0)), "1")
            } else {
                # Standard refitted model formula using composeFormula
                formula_str <- jmvcore::composeFormula("y_num", colnames(X_selected))
            }
            
            fit <- glm(as.formula(formula_str), data = df_fit, family = binomial)
            return(fit)
        },
        
        # ----------------------------------------------------
        # Helper: Fit Multinomial Model
        # ----------------------------------------------------
        .fit_multinom = function(X_selected, y) {
            df_fit <- as.data.frame(X_selected)
            df_fit$y <- y
            
            if (ncol(X_selected) == 0) {
                formula_str <- "y ~ 1"
            } else {
                formula_str <- jmvcore::composeFormula("y", colnames(X_selected))
            }
            
            fit <- nnet::multinom(as.formula(formula_str), data = df_fit, trace = FALSE)
            return(fit)
        },
        
        # ----------------------------------------------------
        # Helper: Create Stratified K-Folds (multiclass)
        # ----------------------------------------------------
        .create_stratified_folds = function(y_vec, k) {
            if (is.factor(y_vec) && length(levels(y_vec)) > 2) {
                shuf_by_lvl <- list()
                for (lvl in levels(y_vec)) {
                    shuf_by_lvl[[lvl]] <- sample(which(y_vec == lvl))
                }
                
                groups_by_lvl <- list()
                for (lvl in levels(y_vec)) {
                    groups_by_lvl[[lvl]] <- cut(seq_along(shuf_by_lvl[[lvl]]), breaks = k, labels = FALSE)
                }
                
                folds <- vector("list", k)
                for (i in 1:k) {
                    fold_i <- c()
                    for (lvl in levels(y_vec)) {
                        fold_i <- c(fold_i, shuf_by_lvl[[lvl]][groups_by_lvl[[lvl]] == i])
                    }
                    folds[[i]] <- fold_i
                }
                return(folds)
            } else {
                idx_pos <- which(y_vec == 1)
                idx_neg <- which(y_vec == 0)
                
                shuf_pos <- sample(idx_pos)
                shuf_neg <- sample(idx_neg)
                
                groups_pos <- cut(seq_along(shuf_pos), breaks = k, labels = FALSE)
                groups_neg <- cut(seq_along(shuf_neg), breaks = k, labels = FALSE)
                
                folds <- vector("list", k)
                for (i in 1:k) {
                    folds[[i]] <- c(shuf_pos[groups_pos == i], shuf_neg[groups_neg == i])
                }
                return(folds)
            }
        },
        
        # ----------------------------------------------------
        # Helper: Populate Coefficient Table (multiclass)
        # ----------------------------------------------------
        .populate_coefficients_table = function(fit, X_selected, is_multinomial, y_levels) {
            coeff_table <- self$results$coeffTable
            ref_level <- y_levels[1]
            ref_msg <- jmvcore::format(.("Reference level: {ref}"), ref = ref_level)
            
            if (is_multinomial) {
                sum_fit <- summary(fit)
                coef_matrix <- sum_fit$coefficients      # (K-1) x P matrix
                se_matrix <- sum_fit$standard.errors     # (K-1) x P matrix
                
                comparison_levels <- rownames(coef_matrix)
                terms <- colnames(coef_matrix)
                
                for (level in comparison_levels) {
                    for (term in terms) {
                        est <- coef_matrix[level, term]
                        se <- se_matrix[level, term]
                        z <- est / se
                        p <- 2 * (1 - pnorm(abs(z)))
                        
                        odds_ratio <- exp(est)
                        ci_lower <- exp(est - 1.96 * se)
                        ci_upper <- exp(est + 1.96 * se)
                        
                        if (term == "(Intercept)") {
                            std_beta <- NA
                            disp_name <- .("(Intercept)")
                        } else {
                            clean_col_name <- gsub("^`|`$", "", term)
                            if (clean_col_name %in% colnames(X_selected)) {
                                std_beta <- est * sd(X_selected[, clean_col_name], na.rm = TRUE)
                                disp_name <- clean_col_name
                            } else {
                                disp_name <- term
                                std_beta <- NA
                                for (col in colnames(X_selected)) {
                                    if (grepl(col, term, fixed = TRUE)) {
                                        std_beta <- est * sd(X_selected[, col], na.rm = TRUE)
                                        break
                                    }
                                }
                            }
                        }
                        
                        row_key <- paste0(level, "_", term)
                        coeff_table$addRow(rowKey = row_key, values = list(
                            level = level,
                            term = disp_name,
                            est = est,
                            se = se,
                            z = z,
                            p = p,
                            or = odds_ratio,
                            ciLower = ci_lower,
                            ciUpper = ci_upper,
                            stdBeta = std_beta
                        ))
                    }
                }
            } else {
                sum_fit <- summary(fit)
                coef_matrix <- sum_fit$coefficients
                terms <- rownames(coef_matrix)
                
                for (i in seq_along(terms)) {
                    term_name <- terms[i]
                    est <- coef_matrix[term_name, "Estimate"]
                    se <- coef_matrix[term_name, "Std. Error"]
                    z <- coef_matrix[term_name, "z value"]
                    p <- coef_matrix[term_name, "Pr(>|z|)"]
                    
                    odds_ratio <- exp(est)
                    ci_lower <- exp(est - 1.96 * se)
                    ci_upper <- exp(est + 1.96 * se)
                    
                    if (term_name == "(Intercept)") {
                        std_beta <- NA
                        disp_name <- .("(Intercept)")
                    } else {
                        clean_col_name <- gsub("^`|`$", "", term_name)
                        if (clean_col_name %in% colnames(X_selected)) {
                            std_beta <- est * sd(X_selected[, clean_col_name], na.rm = TRUE)
                            disp_name <- clean_col_name
                        } else {
                            disp_name <- term_name
                            std_beta <- NA
                            for (col in colnames(X_selected)) {
                                if (grepl(col, term_name, fixed = TRUE)) {
                                    std_beta <- est * sd(X_selected[, col], na.rm = TRUE)
                                    break
                                }
                            }
                        }
                    }
                    
                    coeff_table$addRow(rowKey = term_name, values = list(
                        level = y_levels[2],
                        term = disp_name,
                        est = est,
                        se = se,
                        z = z,
                        p = p,
                        or = odds_ratio,
                        ciLower = ci_lower,
                        ciUpper = ci_upper,
                        stdBeta = std_beta
                    ))
                }
            }
            
            try(coeff_table$addFootnote(col = "level", note = ref_msg, rowNo = 1), silent = TRUE)
        },
        
        # ----------------------------------------------------
        # Helper: Generate Model Info Table
        # ----------------------------------------------------
        .populate_info_table = function(method, alpha, lasso_choice, partition, val_split, cv_folds, cv_repeats,
                                        train_acc, train_auc, val_acc, val_auc, cv_acc, cv_auc,
                                        X_selected, covs, factors, ref_level) {
            method_lbl <- switch(
                method,
                "manual" = .("Manual"),
                "forward" = .("Forward (AIC)"),
                "backward" = .("Backward (AIC)"),
                "lasso" = jmvcore::format(.("LASSO ({penalty})"), penalty = toupper(lasso_choice)),
                "elastic" = jmvcore::format(.("ElasticNet (α={alpha}, {penalty})"), alpha = alpha, penalty = toupper(lasso_choice))
            )
            
            val_lbl <- switch(
                partition,
                "holdout" = jmvcore::format(.("Hold-out ({split}%)"), split = val_split),
                "kfold" = jmvcore::format(.("{k}-Fold CV"), k = cv_folds),
                "repeated_kfold" = jmvcore::format(.("Rep {k}-Fold CV (x{r})"), k = cv_folds, r = cv_repeats)
            )
            
            info_table <- self$results$infoTable
            
            # Show holdout or cross-validation correctly in vertical table
            if (partition == "holdout") {
                v_acc <- val_acc
                c_acc <- NA
                v_auc <- val_auc
            } else {
                v_acc <- NA
                c_acc <- cv_acc
                v_auc <- cv_auc
            }
            
            metrics <- c(
                .("Selection"),
                .("Validation"),
                .("Train Acc (%)"),
                .("Hold-out Acc (%)"),
                .("CV Acc (%)"),
                .("Train AUC"),
                .("Val / CV AUC")
            )
            
            fmt_val <- function(val, is_pct = FALSE, is_auc = FALSE) {
                if (is.null(val) || is.na(val)) return("-")
                if (is.numeric(val)) {
                    if (is_pct) {
                        return(sprintf("%.1f%%", val))
                    } else if (is_auc) {
                        return(sprintf("%.3f", val))
                    }
                    return(as.character(val))
                }
                return(as.character(val))
            }
            
            values <- c(
                method_lbl,
                val_lbl,
                fmt_val(train_acc, is_pct = TRUE),
                fmt_val(v_acc, is_pct = TRUE),
                fmt_val(c_acc, is_pct = TRUE),
                fmt_val(train_auc, is_auc = TRUE),
                fmt_val(v_auc, is_auc = TRUE)
            )
            
            for (i in 1:7) {
                info_table$setRow(rowNo = i, values = list(
                    metric = metrics[i],
                    value = values[i]
                ))
            }
            
            # Find selected factors and covariates
            selected_covs <- c()
            for (cov in covs) {
                if (cov %in% colnames(X_selected)) {
                    selected_covs <- c(selected_covs, cov)
                }
            }
            
            selected_factors <- c()
            for (fac in factors) {
                if (any(startsWith(colnames(X_selected), fac))) {
                    selected_factors <- c(selected_factors, fac)
                }
            }
            
            factors_str <- if (length(selected_factors) > 0) paste(selected_factors, collapse = ", ") else .("None")
            covs_str <- if (length(selected_covs) > 0) paste(selected_covs, collapse = ", ") else .("None")
            
            msg <- jmvcore::format(.("Selected Factors: {factors}; Selected Covariates: {covs}"), factors = factors_str, covs = covs_str)
            info_table$addFootnote(col = "metric", note = msg, rowNo = 1)
        },
        
        # ----------------------------------------------------
        # Helper: Dynamic HTML Formula Construction
        # ----------------------------------------------------
        .generate_formula_html = function(fit, is_multinomial, y_levels) {
            if (is_multinomial) {
                coef_matrix <- coef(fit)
                comparison_levels <- rownames(coef_matrix)
                ref_level <- y_levels[1]
                
                html_blocks <- c()
                for (level in comparison_levels) {
                    coefs <- coef_matrix[level, ]
                    terms <- c()
                    for (i in seq_along(coefs)) {
                        name <- names(coefs)[i]
                        val <- coefs[i]
                        if (is.na(val)) next
                        
                        sign_str <- if (val >= 0) " + " else " - "
                        val_abs <- abs(val)
                        val_fmt <- sprintf("%.4f", val_abs)
                        
                        if (name == "(Intercept)") {
                            terms <- c(terms, paste0(
                                ifelse(val >= 0, "", "-"), 
                                "<span style='color: #2563EB; font-weight: 600;'>", val_fmt, "</span>"
                            ))
                        } else {
                            # Clean variable name (strip backticks and backslashes)
                            clean_name <- name
                            clean_name <- gsub("`|\\\\", "", clean_name)
                            
                            terms <- c(terms, paste0(
                                sign_str, 
                                "<span style='color: #2563EB; font-weight: 600;'>", val_fmt, "</span>",
                                " · <span style='color: #475569; font-style: italic; font-weight: 500;'>", clean_name, "</span>"
                            ))
                        }
                    }
                    logit_formula <- paste(terms, collapse = "")
                    
                    # Clean up starting double signs
                    if (startsWith(logit_formula, " + ")) {
                        logit_formula <- substring(logit_formula, 4)
                    }
                    
                    html_blocks <- c(html_blocks, paste0(
                        "  <div style='font-family: monospace; font-size: 13px; color: #334155; background-color: #FFFFFF; padding: 8px 12px; border: 1px solid #E2E8F0; border-radius: 4px; line-height: 1.4; word-wrap: break-word; white-space: pre-wrap; margin-bottom: 6px;'>",
                        "    <span style='color: #0F172A; font-weight: 600;'>ln( P(Y = '<span style='color: #2563EB;'>", level, "</span>') / P(Y = '<span style='color: #64748B;'>", ref_level, "</span>') )</span> = ", logit_formula, "",
                        "  </div>"
                    ))
                }
                
                probs_formula_blocks <- c()
                denom_terms <- c("1")
                for (level in comparison_levels) {
                    denom_terms <- c(denom_terms, paste0("exp( Logit_", level, " )"))
                }
                denom_str <- paste(denom_terms, collapse = " + ")
                
                for (level in comparison_levels) {
                    probs_formula_blocks <- c(probs_formula_blocks, paste0(
                        "  <div style='font-family: monospace; font-size: 13px; color: #334155; background-color: #FFFFFF; padding: 8px 12px; border: 1px solid #E2E8F0; border-radius: 4px; line-height: 1.4; word-wrap: break-word; white-space: pre-wrap; margin-bottom: 6px;'>",
                        "    <span style='color: #0F172A; font-weight: 600;'>P( Y = '<span style='color: #10B981;'>", level, "</span>' )</span> = exp( Logit_", level, " ) / [ ", denom_str, " ]",
                        "  </div>"
                    ))
                }
                probs_formula_blocks <- c(probs_formula_blocks, paste0(
                    "  <div style='font-family: monospace; font-size: 13px; color: #334155; background-color: #FFFFFF; padding: 8px 12px; border: 1px solid #E2E8F0; border-radius: 4px; line-height: 1.4; word-wrap: break-word; white-space: pre-wrap; margin-bottom: 6px;'>",
                    "    <span style='color: #0F172A; font-weight: 600;'>P( Y = '<span style='color: #64748B;'>", ref_level, "</span>' )</span> = 1 / [ ", denom_str, " ]",
                    "  </div>"
                ))
                
                html <- paste0(
                    "<div style='font-family: system-ui, -apple-system, sans-serif; padding: 12px; background-color: #F8FAFC; border: 1px solid #E2E8F0; border-left: 5px solid #2563EB; border-radius: 6px; margin-top: 15px;'>",
                    "  <div style='color: #1E293B; font-weight: bold; font-size: 14px; margin-bottom: 8px;'>",
                    "    <b style='background-color: #2563EB; color: white; padding: 1px 6px; border-radius: 3px; font-size: 10px; margin-right: 6px; text-transform: uppercase;'>Logits</b>",
                    "    ", .("Multinomial Log-odds Equations"),
                    "  </div>",
                    paste(html_blocks, collapse = ""),
                    "  <div style='color: #1E293B; font-weight: bold; font-size: 14px; margin-top: 14px; margin-bottom: 8px;'>",
                    "    <b style='background-color: #10B981; color: white; padding: 1px 6px; border-radius: 3px; font-size: 10px; margin-right: 6px; text-transform: uppercase;'>Probabilities</b>",
                    "    ", .("Probability Models"),
                    "  </div>",
                    paste(probs_formula_blocks, collapse = ""),
                    "</div>"
                )
                return(html)
            } else {
                coefs <- coef(fit)
                pos_class_label <- y_levels[2]
                terms <- c()
                for (i in seq_along(coefs)) {
                    name <- names(coefs)[i]
                    val <- coefs[i]
                    if (is.na(val)) next
                    
                    sign_str <- if (val >= 0) " + " else " - "
                    val_abs <- abs(val)
                    val_fmt <- sprintf("%.4f", val_abs)
                    
                    if (name == "(Intercept)") {
                        terms <- c(terms, paste0(
                            ifelse(val >= 0, "", "-"), 
                            "<span style='color: #2563EB; font-weight: 600;'>", val_fmt, "</span>"
                        ))
                    } else {
                        # Clean variable name (strip backticks and backslashes)
                        clean_name <- name
                        clean_name <- gsub("`|\\\\", "", clean_name)
                        
                        terms <- c(terms, paste0(
                            sign_str, 
                            "<span style='color: #2563EB; font-weight: 600;'>", val_fmt, "</span>",
                            " · <span style='color: #475569; font-style: italic; font-weight: 500;'>", clean_name, "</span>"
                        ))
                    }
                }
                logit_formula <- paste(terms, collapse = "")
                
                # Clean up starting double signs
                if (startsWith(logit_formula, " + ")) {
                    logit_formula <- substring(logit_formula, 4)
                }
                
                html <- paste0(
                    "<div style='font-family: system-ui, -apple-system, sans-serif; padding: 12px; background-color: #F8FAFC; border: 1px solid #E2E8F0; border-left: 5px solid #2563EB; border-radius: 6px; margin-top: 15px;'>",
                    "  <div style='color: #1E293B; font-weight: bold; font-size: 14px; margin-bottom: 8px;'>",
                    "    <b style='background-color: #2563EB; color: white; padding: 1px 6px; border-radius: 3px; font-size: 10px; margin-right: 6px; text-transform: uppercase;'>Logit</b>",
                    "    ", .("Logistic Regression Model (Log-odds)"),
                    "  </div>",
                    "  <div style='font-family: monospace; font-size: 13px; color: #334155; background-color: #FFFFFF; padding: 8px 12px; border: 1px solid #E2E8F0; border-radius: 4px; line-height: 1.4; word-wrap: break-word; white-space: pre-wrap;'>",
                    "    <span style='color: #0F172A; font-weight: 600;'>ln( P / (1 - P) )</span> = ", logit_formula, "",
                    "  </div>",
                    "  <div style='color: #1E293B; font-weight: bold; font-size: 14px; margin-top: 14px; margin-bottom: 8px;'>",
                    "    <b style='background-color: #10B981; color: white; padding: 1px 6px; border-radius: 3px; font-size: 10px; margin-right: 6px; text-transform: uppercase;'>Probability</b>",
                    "    ", .("Probability Model"),
                    "  </div>",
                    "  <div style='font-family: monospace; font-size: 13px; color: #334155; background-color: #FFFFFF; padding: 8px 12px; border: 1px solid #E2E8F0; border-radius: 4px; line-height: 1.4; word-wrap: break-word; white-space: pre-wrap;'>",
                    "    <span style='color: #0F172A; font-weight: 600;'>P( Y = '<span style='color: #059669;'>", pos_class_label, "</span>' )</span> = 1 / ( 1 + exp[ -( ", logit_formula, " ) ] )",
                    "  </div>",
                    "</div>"
                )
                return(html)
            }
        },
        
        # ----------------------------------------------------
        # Save Predicted Class / Prob Initializer
        # ----------------------------------------------------
        .initOutputs = function() {
            # Predict Class
            keys <- "predClass"
            titles <- .("Predicted Class")
            descriptions <- .("Logistic Regression Predicted Class")
            measureTypes <- "factor"
            self$results$predClass$set(keys, titles, descriptions, measureTypes)
            
            # Predict Prob
            keys <- "predProb"
            titles <- .("Predicted Probability")
            descriptions <- .("Logistic Regression Predicted Probability")
            measureTypes <- "continuous"
            self$results$predProb$set(keys, titles, descriptions, measureTypes)
        },
        
        # ----------------------------------------------------
        # Plot: ROC Curves
        # ----------------------------------------------------
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
                show_roc_cut <- isTRUE(roc_data$show_roc_cut)
                
                # Determine number of classes
                y_levels <- roc_data$y_levels
                C <- if (!is.null(roc_data$C)) roc_data$C else length(y_levels)
                
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
                        main=.("ROC Curves"), cex.main=1.3,
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
                    } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob_bin)) {
                        r_cv <- pROC::roc(cv_y_bin, cv_prob_bin, percent = is_pct, quiet = TRUE)
                        if (self$options$palBrewer == "none") {
                            cols <- c(cols, "#46B233") # CV is Green
                        }
                        active_cols <- c(active_cols, cols[2])
                        ltys <- c(ltys, 2)         # CV is dashed
                        auc_cv_val <- as.numeric(pROC::auc(r_cv))
                        auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                        lbl <- if (partition == "kfold") .("K-Fold CV (AUC = ") else .("Repeated Stratified CV (AUC = ")
                        leg_labels <- c(leg_labels, paste0(lbl, auc_cv_str, ")"))
                        
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
                        y_tr_c <- as.numeric(y_val == lev)
                        prob_tr_c <- if (!is.null(colnames(train_prob)) && lev %in% colnames(train_prob)) train_prob[, lev] else train_prob[, c]
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
                    plot_title <- if (partition %in% c("kfold", "repeated_kfold")) .("Combined ROC - Cross-Validation") else .("Combined ROC - Validation")
                    
                    for (c in 1:C) {
                        lev <- y_levels[c]
                        if (partition == "holdout" && !is.null(val_prob) && !is.null(val_y)) {
                            y_va_c <- as.numeric(val_y == lev)
                            prob_va_c <- if (!is.null(colnames(val_prob)) && lev %in% colnames(val_prob)) val_prob[, lev] else val_prob[, c]
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
                        } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob) && !is.null(cv_y)) {
                            y_cv_c <- as.numeric(cv_y == lev)
                            prob_cv_c <- if (!is.null(colnames(cv_prob)) && lev %in% colnames(cv_prob)) cv_prob[, lev] else cv_prob[, c]
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
                        lty=ifelse(partition %in% c("kfold", "repeated_kfold"), 2, 1),
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
                    } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob_bin)) {
                        r_cv <- pROC::roc(cv_y_bin, cv_prob_bin, percent = is_pct, quiet = TRUE)
                        if (self$options$palBrewer == "none") {
                            cols <- c(cols, "#46B233") # CV is Green
                        }
                        active_cols <- c(active_cols, cols[2])
                        ltys <- c(ltys, 2)         # CV is dashed
                        auc_cv_val <- as.numeric(pROC::auc(r_cv))
                        auc_cv_str <- if (is_pct) paste0(round(auc_cv_val, 1), "%") else round(auc_cv_val, 3)
                        lbl <- if (partition == "kfold") .("K-Fold CV (AUC = ") else .("Repeated Stratified CV (AUC = ")
                        leg_labels <- c(leg_labels, paste0(lbl, auc_cv_str, ")"))
                        
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
                
                if (exists("p") && !is.null(p)) {
                    print(p)
                }
                return(TRUE)
            }, error = function(e) {
                return(FALSE)
            })
            return(result)
        },
        
        .forestPlot = function(image, ggtheme, theme, ...) {
            forest_list <- image$state
            if (is.null(forest_list) || length(forest_list) == 0) return(FALSE)
            
            df <- do.call(rbind, lapply(forest_list, as.data.frame))
            if (nrow(df) == 0) return(FALSE)
            
            if ("level" %in% colnames(df) && !all(is.na(df$level))) {
                df$term_label <- paste0(df$level, ": ", df$term)
                df$Group <- as.factor(df$level)
            } else {
                df$term_label <- df$term
                df$Group <- "Predictor"
            }
            
            df$term_label <- factor(df$term_label, levels = rev(df$term_label))
            
            # Setup plot coordinates/clipping
            df$lower_plot <- df$lower
            df$upper_plot <- df$upper
            df$or_plot <- df$or
            df$clip_upper <- FALSE
            df$clip_lower <- FALSE
            
            show_forest_log <- isTRUE(self$options$forestLog)
            show_forest_clip <- FALSE
            if ("forestClip" %in% names(self$options)) {
                show_forest_clip <- isTRUE(self$options$forestClip)
            }
            
            clip_threshold <- 10
            if ("forestClipThreshold" %in% names(self$options)) {
                clip_threshold <- self$options$forestClipThreshold
                if (is.null(clip_threshold) || is.na(clip_threshold)) {
                    clip_threshold <- 10
                }
            }
            
            if (show_forest_clip && !show_forest_log) {
                lower_threshold <- 1 / clip_threshold
                
                df$clip_upper <- df$upper > clip_threshold
                df$clip_lower <- df$lower < lower_threshold
                
                df$upper_plot <- ifelse(df$clip_upper, clip_threshold, df$upper)
                df$lower_plot <- ifelse(df$clip_lower, lower_threshold, df$lower)
                
                # Make sure the point estimate itself is clipped if it goes outside the bounds
                df$or_plot <- ifelse(df$or_plot > clip_threshold, clip_threshold, df$or_plot)
                df$or_plot <- ifelse(df$or_plot < lower_threshold, lower_threshold, df$or_plot)
            }
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x = term_label, y = or_plot)) +
                ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "#E54028", linewidth = 0.8)
                
            df_unclipped <- df[!df$clip_lower & !df$clip_upper, ]
            df_clip_up <- df[!df$clip_lower & df$clip_upper, ]
            df_clip_low <- df[df$clip_lower & !df$clip_upper, ]
            df_clip_both <- df[df$clip_lower & df$clip_upper, ]
            
            # 1. Unclipped
            if (nrow(df_unclipped) > 0) {
                p <- p + ggplot2::geom_errorbar(
                    data = df_unclipped,
                    ggplot2::aes(ymin = lower_plot, ymax = upper_plot, color = Group),
                    width = 0.2,
                    linewidth = 0.8
                )
            }
            
            # 2. Clipped at upper end only (arrow on the right, cap on the left)
            if (nrow(df_clip_up) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = df_clip_up,
                    ggplot2::aes(x = term_label, xend = term_label, y = lower_plot, yend = upper_plot, color = Group),
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "inches"), ends = "last", type = "closed"),
                    linewidth = 0.8
                )
                p <- p + ggplot2::geom_segment(
                    data = df_clip_up,
                    ggplot2::aes(
                        x = as.numeric(term_label) - 0.1,
                        xend = as.numeric(term_label) + 0.1,
                        y = lower_plot,
                        yend = lower_plot,
                        color = Group
                    ),
                    linewidth = 0.8
                )
            }
            
            # 3. Clipped at lower end only (arrow on the left, cap on the right)
            if (nrow(df_clip_low) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = df_clip_low,
                    ggplot2::aes(x = term_label, xend = term_label, y = lower_plot, yend = upper_plot, color = Group),
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "inches"), ends = "first", type = "closed"),
                    linewidth = 0.8
                )
                p <- p + ggplot2::geom_segment(
                    data = df_clip_low,
                    ggplot2::aes(
                        x = as.numeric(term_label) - 0.1,
                        xend = as.numeric(term_label) + 0.1,
                        y = upper_plot,
                        yend = upper_plot,
                        color = Group
                    ),
                    linewidth = 0.8
                )
            }
            
            # 4. Clipped at both ends (arrows on both sides)
            if (nrow(df_clip_both) > 0) {
                p <- p + ggplot2::geom_segment(
                    data = df_clip_both,
                    ggplot2::aes(x = term_label, xend = term_label, y = lower_plot, yend = upper_plot, color = Group),
                    arrow = ggplot2::arrow(length = ggplot2::unit(0.12, "inches"), ends = "both", type = "closed"),
                    linewidth = 0.8
                )
            }
            
            p <- p + ggplot2::geom_point(ggplot2::aes(fill = Group), color = "white", shape = 21, size = 4, stroke = 0.5) +
                ggplot2::coord_flip()
                
            # Set axis labels and title with logarithmic indicator if applicable
            y_label <- if (show_forest_log) {
                .("Log(OR)")
            } else {
                .("Odds Ratio (OR)")
            }
            
            p <- p + ggplot2::labs(
                    x = "",
                    y = y_label,
                    title = .("Forest Plot of Odds Ratios")
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
                    axis.title = ggplot2::element_text(size = 14),
                    axis.text = ggplot2::element_text(size = 12),
                    legend.text = ggplot2::element_text(size = 12),
                    panel.grid.major = ggplot2::element_line(linetype = "dashed", color = "#e0e0e0"),
                    panel.grid.minor = ggplot2::element_blank()
                )
            
            if (show_forest_log) {
                p <- p + ggplot2::scale_y_log10(labels = function(x) sprintf("%g", x))
            } else {
                p <- p + ggplot2::scale_y_continuous(labels = function(x) sprintf("%g", x))
            }
            
            n_groups <- length(unique(df$Group))
            pal <- self$options$palBrewer
            if (n_groups > 1) {
                if (pal != "none") {
                    cols <- try(RColorBrewer::brewer.pal(n = max(3, n_groups), name = pal)[1:n_groups], silent = TRUE)
                    if (!inherits(cols, "try-error") && !any(is.na(cols))) {
                        p <- p + ggplot2::scale_color_manual(values = cols) + ggplot2::scale_fill_manual(values = cols)
                    }
                }
                p <- p + ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
            } else {
                p <- p + ggplot2::scale_color_manual(values = "#3366B2") + ggplot2::scale_fill_manual(values = "#3366B2") +
                    ggplot2::theme(legend.position = "none")
            }
            
            print(p)
            return(TRUE)
        },
        
        .prepare_forest_data = function(fit, X_selected, is_multinomial, y_levels) {
            forest_list <- list()
            
            if (is_multinomial) {
                sum_fit <- summary(fit)
                coef_matrix <- sum_fit$coefficients
                se_matrix <- sum_fit$standard.errors
                
                comparison_levels <- rownames(coef_matrix)
                terms <- colnames(coef_matrix)
                
                for (level in comparison_levels) {
                    for (term in terms) {
                        if (term == "(Intercept)") next
                        
                        est <- coef_matrix[level, term]
                        se <- se_matrix[level, term]
                        
                        odds_ratio <- exp(est)
                        ci_lower <- exp(est - 1.96 * se)
                        ci_upper <- exp(est + 1.96 * se)
                        
                        clean_col_name <- gsub("^`|`$", "", term)
                        if (clean_col_name %in% colnames(X_selected)) {
                            disp_name <- clean_col_name
                        } else {
                            disp_name <- term
                        }
                        
                        forest_list[[length(forest_list) + 1]] <- list(
                            level = level,
                            term = disp_name,
                            or = odds_ratio,
                            lower = ci_lower,
                            upper = ci_upper
                        )
                    }
                }
            } else {
                sum_fit <- summary(fit)
                coef_matrix <- sum_fit$coefficients
                terms <- rownames(coef_matrix)
                
                for (i in seq_along(terms)) {
                    term_name <- terms[i]
                    if (term_name == "(Intercept)") next
                    
                    est <- coef_matrix[term_name, "Estimate"]
                    se <- coef_matrix[term_name, "Std. Error"]
                    
                    odds_ratio <- exp(est)
                    ci_lower <- exp(est - 1.96 * se)
                    ci_upper <- exp(est + 1.96 * se)
                    
                    clean_col_name <- gsub("^`|`$", "", term_name)
                    if (clean_col_name %in% colnames(X_selected)) {
                        disp_name <- clean_col_name
                    } else {
                        disp_name <- term_name
                    }
                    
                    forest_list[[length(forest_list) + 1]] <- list(
                        level = NA,
                        term = disp_name,
                        or = odds_ratio,
                        lower = ci_lower,
                        upper = ci_upper
                    )
                }
            }
            return(forest_list)
        }
    )
)
