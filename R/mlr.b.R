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
                self$results$text$setContent(paste0("<h4>", jmvcore::.("Full dataset used in analysis"), "</h4>"))
                self$results$text$setVisible(TRUE)
            }
            
            clean_data <- na.omit(dat[, data_vars, drop=FALSE])
            
            if (nrow(clean_data) < 15) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("The dataset has too few complete cases to perform logistic regression (minimum 15 cases required)."))
                return()
            }
            
            y <- as.factor(clean_data[[dep]])
            y_levels <- levels(y)
            
            if (length(y_levels) != 2) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("Dependent variable must have exactly two levels for binary logistic regression."))
                return()
            }
            
            # Encode target as 0 and 1
            # y_num will be 1 for the second level (positive class) and 0 for the first
            pos_class_label <- y_levels[2]
            neg_class_label <- y_levels[1]
            y_num <- ifelse(y == pos_class_label, 1, 0)
            
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
                y = y_num,
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
            
            # Fit standard glm on selected features
            fit_full <- private$.fit_standard_glm(X_selected, y_num)
            
            # Populate Model Coefficients Table
            private$.populate_coefficients_table(fit_full, X_selected)
            
            # Generate and render mathematical formula
            if (self$options$show_formula) {
                coefs <- coef(fit_full)
                formula_html <- private$.generate_formula_html(coefs, pos_class_label)
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
            train_prob <- predict(fit_full, type = "response")
            train_pred <- get_predictions(train_prob)
            train_acc <- get_accuracy(train_pred, y_num)
            
            r_train_roc <- try(pROC::roc(y_num, train_prob, quiet = TRUE), silent = TRUE)
            train_auc <- if (inherits(r_train_roc, "try-error")) NA else as.numeric(pROC::auc(r_train_roc))
            
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
                idx_pos <- which(y_num == 1)
                idx_neg <- which(y_num == 0)
                
                n_pos_test <- max(1, round(length(idx_pos) * val_split))
                n_neg_test <- max(1, round(length(idx_neg) * val_split))
                
                test_pos <- sample(idx_pos, n_pos_test)
                test_neg <- sample(idx_neg, n_neg_test)
                
                test_idx <- c(test_pos, test_neg)
                train_idx <- setdiff(1:nrow(X_all), test_idx)
                
                # Fit model on training fold (including feature selection on training partition!)
                X_train_full <- X_all[train_idx, , drop = FALSE]
                y_train <- y_num[train_idx]
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
                
                fit_train <- private$.fit_standard_glm(X_train_selected, y_train)
                
                # Predict on test split
                val_prob <- predict(fit_train, newdata = as.data.frame(X_test_selected), type = "response")
                val_y <- y_num[test_idx]
                val_pred <- get_predictions(val_prob)
                val_acc <- get_accuracy(val_pred, val_y)
                
                r_val_roc <- try(pROC::roc(val_y, val_prob, quiet = TRUE), silent = TRUE)
                val_auc <- if (inherits(r_val_roc, "try-error")) NA else as.numeric(pROC::auc(r_val_roc))
                
            } else if (partition == "kfold") {
                # Stratified K-Fold Cross-Validation
                folds <- private$.create_stratified_folds(y_num, cv_folds)
                
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
                
            } else if (partition == "repeated_kfold") {
                # Repeated Stratified K-Fold CV (Modern additions!)
                # Run the stratified folds multiple times and average performance metrics
                rep_accs <- numeric(cv_repeats)
                rep_aucs <- numeric(cv_repeats)
                
                # To generate a single clean ROC curve and out-of-fold predictions,
                # we average the predicted probabilities for each subject across all repeats.
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
                        
                        # Selection inside fold
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
                    
                    # Averaged out-of-fold probability for plotting
                    cv_prob <- subject_probs / cv_repeats
                    cv_y <- y_num
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
                factors = factors
            )
            
            # ----------------------------------------------------
            # Save Predictions on Demand
            # ----------------------------------------------------
            if ((self$options$predClass && self$results$predClass$isNotFilled()) ||
                (self$options$predProb && self$results$predProb$isNotFilled())) {
                
                # Fill predictions for all complete cases (keep missing rows as NA)
                pred_prob_col <- rep(NA, nrow(self$data))
                pred_class_col <- rep(NA, nrow(self$data))
                
                # Compute predictions on full model for the complete cases
                full_probs <- predict(fit_full, newdata = as.data.frame(X_selected), type = "response")
                full_classes <- ifelse(full_probs >= 0.5, pos_class_label, neg_class_label)
                
                complete_case_indices <- as.integer(rownames(clean_data))
                pred_prob_col[complete_case_indices] <- full_probs
                pred_class_col[complete_case_indices] <- full_classes
                
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
                roc_data <- list(
                    y_num = y_num,
                    train_prob = train_prob,
                    val_y = val_y,
                    val_prob = val_prob,
                    cv_y = cv_y,
                    cv_prob = cv_prob,
                    partition = partition,
                    roc_x = self$options$roc_x,
                    roc_unit = self$options$roc_unit
                )
                image_roc$setState(roc_data)
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
            
            if (method %in% c("forward", "backward")) {
                # Formula-based Stepwise Selection
                pred_terms <- c(covs, factors)
                null_formula <- as.formula(paste(jmvcore::composeFormula(dep, character(0)), "1"))
                full_formula <- as.formula(jmvcore::composeFormula(dep, pred_terms))

                
                # Fit temporary models
                null_model <- try(glm(null_formula, data = clean_data, family = binomial), silent = TRUE)
                full_model <- try(glm(full_formula, data = clean_data, family = binomial), silent = TRUE)
                
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
                    family = "binomial",
                    alpha = alpha,
                    nfolds = n_folds
                ), silent = TRUE)
                
                if (inherits(fit_cv, "try-error")) {
                    return(all_features) # Fallback to all
                }
                
                # Choose optimal lambda
                lambda_opt <- if (lasso_choice == "min") fit_cv$lambda.min else fit_cv$lambda.1se
                
                # Extract active coefficients
                coefs <- as.matrix(coef(fit_cv, s = lambda_opt))
                active_coef_names <- rownames(coefs)[coefs[, 1] != 0]
                active_coef_names <- setdiff(active_coef_names, "(Intercept)")
                
                if (length(active_coef_names) == 0) {
                    # If empty, fallback to the single predictor with the largest magnitude (or all)
                    raw_coefs <- as.matrix(coef(fit_cv, s = fit_cv$lambda.min))
                    mag_coefs <- abs(raw_coefs[-1, 1, drop = FALSE])
                    largest_var <- rownames(mag_coefs)[which.max(mag_coefs)]
                    if (length(largest_var) > 0) active_coef_names <- largest_var
                    else return(all_features)
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
        # Helper: Create Stratified K-Folds
        # ----------------------------------------------------
        .create_stratified_folds = function(y_vec, k) {
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
        },
        
        # ----------------------------------------------------
        # Helper: Populate Coefficient Table
        # ----------------------------------------------------
        .populate_coefficients_table = function(fit, X_selected) {
            sum_fit <- summary(fit)
            coef_matrix <- sum_fit$coefficients
            
            coeff_table <- self$results$coeffTable
            
            terms <- rownames(coef_matrix)
            
            for (i in seq_along(terms)) {
                term_name <- terms[i]
                est <- coef_matrix[term_name, "Estimate"]
                se <- coef_matrix[term_name, "Std. Error"]
                z <- coef_matrix[term_name, "z value"]
                p <- coef_matrix[term_name, "Pr(>|z|)"]
                
                # Odds Ratio
                odds_ratio <- exp(est)
                
                # Odds Ratio Confidence Intervals
                ci_lower <- exp(est - 1.96 * se)
                ci_upper <- exp(est + 1.96 * se)
                
                # Standardized Beta calculation
                if (term_name == "(Intercept)") {
                    std_beta <- NA
                    disp_name <- .("(Intercept)")
                } else {
                    # Map the term name back to X_selected column
                    clean_col_name <- gsub("^`|`$", "", term_name) # Handle quoted terms
                    
                    # If name has direct match in model matrix columns
                    if (clean_col_name %in% colnames(X_selected)) {
                        std_beta <- est * sd(X_selected[, clean_col_name], na.rm = TRUE)
                        disp_name <- clean_col_name
                    } else {
                        # Strip standard R dummy coding prefixes if needed, but display raw term
                        disp_name <- term_name
                        std_beta <- NA
                        
                        # Search if it matches any column in X_selected
                        for (col in colnames(X_selected)) {
                            if (grepl(col, term_name, fixed = TRUE)) {
                                std_beta <- est * sd(X_selected[, col], na.rm = TRUE)
                                break
                            }
                        }
                    }
                }
                
                coeff_table$addRow(rowKey = term_name, values = list(
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
        },
        
        # ----------------------------------------------------
        # Helper: Generate Model Info Table
        # ----------------------------------------------------
        .populate_info_table = function(method, alpha, lasso_choice, partition, val_split, cv_folds, cv_repeats,
                                        train_acc, train_auc, val_acc, val_auc, cv_acc, cv_auc,
                                        X_selected, covs, factors) {
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
        .generate_formula_html = function(coefs, target_level) {
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
                "    <span style='color: #0F172A; font-weight: 600;'>P( Y = '<span style='color: #059669;'>", target_level, "</span>' )</span> = 1 / ( 1 + exp[ -( ", logit_formula, " ) ] )",
                "  </div>",
                "</div>"
            )
            return(html)
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
            if (is.null(roc_data)) return(FALSE)
            
            result <- tryCatch({
                roc_x <- roc_data$roc_x
                roc_unit <- roc_data$roc_unit
                partition <- roc_data$partition
                is_pct <- roc_unit == "percent"
                legacy_axes <- (roc_x == "1spec")
                
                # Fetch raw prediction data from state
                y_num <- roc_data$y_num
                train_prob <- roc_data$train_prob
                val_y <- roc_data$val_y
                val_prob <- roc_data$val_prob
                cv_y <- roc_data$cv_y
                cv_prob <- roc_data$cv_prob
                
                # We will plot sequentially:
                # 1. Training curve
                r_tr <- pROC::roc(y_num, train_prob, percent = is_pct, quiet = TRUE)
                
                # Determine colors and legend names
                cols <- c("#3366B2") # Training is always Blue
                if (self$options$palBrewer != "none") {
                    cols <- RColorBrewer::brewer.pal(n=3, name=self$options$palBrewer)
                }
                active_cols <- c(cols[1])
                ltys <- c(1)         # Training is solid
                auc_tr_val <- as.numeric(pROC::auc(r_tr))
                auc_tr_str <- if (is_pct) paste0(round(auc_tr_val, 1), "%") else round(auc_tr_val, 3)
                leg_labels <- c(paste0(.("Training (AUC = "), auc_tr_str, ")"))
                
                # Format threshold pattern
                thres_pattern <- ifelse(is_pct, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.3f, %.3f)")
                
                # Plot Training Curve
                p <- pROC::plot.roc(r_tr, col=cols[1],
                    main=.("ROC Curves"), cex.main=1.3,
                    percent=is_pct,
                    cex.lab=1.5, cex.axis=1.3, lwd=3, lty=1,
                    legacy.axes=legacy_axes,
                    xlab=ifelse(is_pct, ifelse(legacy_axes, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(legacy_axes, .("1 - Specificity"), .("Specificity"))),
                    ylab=ifelse(is_pct, .("Sensitivity (%)"), .("Sensitivity")),
                    print.thres=TRUE,
                    print.thres.col=cols[1],
                    print.thres.pch=19, print.thres.cex=1.3,
                    print.thres.best.method="youden",
                    print.thres.pattern=thres_pattern,
                    grid=TRUE, add=FALSE
                )
                
                # Plot Validation Curve if holdout partition
                if (partition == "holdout" && !is.null(val_prob) && !is.null(val_y)) {
                    r_va <- pROC::roc(val_y, val_prob, percent = is_pct, quiet = TRUE)
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
                        print.thres=TRUE,
                        print.thres.col=cols[2],
                        print.thres.pch=19, print.thres.cex=1.3,
                        print.thres.best.method="youden",
                        print.thres.pattern=thres_pattern,
                        add=TRUE
                    )
                } else if (partition %in% c("kfold", "repeated_kfold") && !is.null(cv_prob) && !is.null(cv_y)) {
                    r_cv <- pROC::roc(cv_y, cv_prob, percent = is_pct, quiet = TRUE)
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
                        print.thres=TRUE,
                        print.thres.col=cols[2],
                        print.thres.pch=19, print.thres.cex=1.3,
                        print.thres.best.method="youden",
                        print.thres.pattern=thres_pattern,
                        add=TRUE
                    )
                }
                
                # Draw legend
                legend("bottomright",
                    cex=1.1, lwd=3, col=active_cols,
                    lty=ltys,
                    bg="white", box.lwd=1,
                    legend=leg_labels
                )
                
                print(p)
                return(TRUE)
            }, error = function(e) {
                return(FALSE)
            })
            return(result)
        }
    )
)
