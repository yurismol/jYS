# This file is a generated template, your changes will not be overwritten

mPWRClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mPWRClass",
    inherit = mPWRBase,
    private = list(
        
        .run = function() {

            if (!requireNamespace("pwrss", quietly = TRUE)) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(.("The 'pwrss' package is not installed. Please install it in R."))
                return()
            }

            calc      <- self$options$calc
            design    <- self$options$design
            es        <- self$options$es
            power     <- self$options$power
            n_g1      <- self$options$n
            alt       <- self$options$alt
            alpha     <- as.numeric(self$options$alpha)
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio



            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912 # Asymptotic Relative Efficiency for Spearman
            
            built_args <- private$.buildArgs(calc, design, es, power, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = TRUE)
            args_list <- built_args$args_list
            func_to_call <- built_args$func_to_call
            es_arg <- built_args$es_arg
            n_arg <- built_args$n_arg
            n_mult <- built_args$n_mult

            if (design == "survival") {
                # Survival / Log-Rank calculation (Schoenfeld formula)
                if (alt %in% c("two.sided", "equivalent")) {
                    z_crit <- qnorm(1 - alpha / 2)
                } else {
                    z_crit <- qnorm(1 - alpha)
                }
                
                p1_alloc <- 1 / (1 + n_ratio)
                p2_alloc <- n_ratio / (1 + n_ratio)
                p1_p2 <- p1_alloc * p2_alloc
                
                if (calc == "n") {
                    z_beta <- qnorm(power)
                    if (abs(log(es)) < 1e-4) {
                        res <- try(stop("Hazard Ratio cannot be 1 when calculating sample size/events."), silent = TRUE)
                    } else {
                        d_events <- (z_crit + z_beta)^2 / (p1_p2 * (log(es))^2)
                        res_n1 <- ceiling(d_events)
                        res_n2 <- NA
                        res_pw <- power
                        res_es <- es
                        
                        res <- list(
                            n = res_n1,
                            power = res_pw,
                            parms = list(
                                alpha = alpha,
                                alternative = alt,
                                es = res_es
                            )
                        )
                        class(res) <- "pwrss"
                    }
                } else if (calc == "power") {
                    d_events <- n_g1
                    z_beta <- sqrt(d_events * p1_p2) * abs(log(es)) - z_crit
                    res_pw <- pnorm(z_beta)
                    res_n1 <- n_g1
                    res_n2 <- NA
                    res_es <- es
                    
                    res <- list(
                        n = res_n1,
                        power = res_pw,
                        parms = list(
                            alpha = alpha,
                            alternative = alt,
                            es = res_es
                        )
                    )
                    class(res) <- "pwrss"
                } else if (calc == "es") {
                    d_events <- n_g1
                    z_beta <- qnorm(power)
                    log_hr <- - (z_crit + z_beta) / sqrt(d_events * p1_p2)
                    res_es <- exp(log_hr)
                    res_n1 <- n_g1
                    res_n2 <- NA
                    res_pw <- power
                    
                    res <- list(
                        n = res_n1,
                        power = res_pw,
                        parms = list(
                            alpha = alpha,
                            alternative = alt,
                            es = res_es
                        )
                    )
                    class(res) <- "pwrss"
                }
            } else if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w", "prob", "p1", "prob1")) {
                    interval <- c(1e-4, 0.99)
                } else if (es_arg == "odds.ratio") {
                    interval <- c(1.0001, 10)
                } else {
                    interval <- c(1e-4, 5)
                }
                
                opt <- try(uniroot(get_pwr, interval = interval, extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) {
                    res <- opt
                } else {
                    args_list[[es_arg]] <- opt$root
                    args_list$power <- NULL
                    res <- try(do.call(func_to_call, args_list), silent = TRUE)
                }
            } else if (calc == "n") {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        tmp_args <- args_list
                        if (n_arg == "n2") {
                            tmp_args$n2 <- max(2, round(n_val * n_mult))
                        } else if (is_cor) {
                            eff_n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                            tmp_args$n <- eff_n
                        } else {
                            tmp_args[[n_arg]] <- max(4, round(n_val))
                        }
                        tmp_args$power <- NULL
                        tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99) 
                        return(tmp$power - power)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    
                    if (inherits(opt, "try-error")) {
                        # Keep the original res (the initial do.call error) to make troubleshooting easier
                    } else {
                        if (n_arg == "n2") {
                            args_list$n2 <- max(2, round(opt$root * n_mult))
                        } else if (is_cor) {
                            calc_n <- max(4, round(opt$root))
                            args_list$n <- if (is_spearman) max(4, round(calc_n * are_spearman)) else calc_n
                        } else {
                            args_list[[n_arg]] <- max(4, round(opt$root))
                        }
                        args_list$power <- NULL
                        res <- try(do.call(func_to_call, args_list), silent = TRUE)
                        if (!inherits(res, "try-error") && is_spearman) res$n <- calc_n
                    }
                } else {
                    if (is_spearman) res$n <- ceiling(res$n / are_spearman)
                }
            } else {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
            }

            if (inherits(res, "try-error")) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(paste0(.("Calculation error:"), "\n", res))
            } else {
                table <- self$results$powerTable
                
                show_n2 <- !(design %in% c("one.sample", "one.sample_np", "onecor", "onecor_np", "linear", "logistic", "chisq", "anova", "survival"))
                
                table$getColumn("n1")$setVisible(calc == "n")
                table$getColumn("n2")$setVisible(calc == "n" && show_n2)
                table$getColumn("power")$setVisible(calc == "power")
                table$getColumn("es")$setVisible(calc == "es")
                
                table$getColumn("n1_user")$setVisible(calc != "n")
                table$getColumn("n2_user")$setVisible(calc != "n" && show_n2)
                table$getColumn("es_user")$setVisible(calc != "es")
                table$getColumn("power_user")$setVisible(calc != "power")
                
                res_n1 <- if (n_arg == "n.total") res$n.total else res$n
                if (length(res_n1) > 0) res_n1 <- res_n1[1]
                if (is_spearman && calc != "n") res_n1 <- n_g1
                
                if (design %in% c("independent", "welch", "independent_np")) {
                    res_n2 <- if (length(res$n) > 1) res$n[2] else res_n1
                } else if (design %in% c("paired", "paired_np")) {
                    res_n2 <- if (length(res$n) > 1) res$n[2] else res_n1
                } else {
                    res_n2 <- NA 
                }
                
                # CRT calculation for table values
                if (self$options$crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                    de <- 1 + (self$options$cluster_size - 1) * self$options$icc
                    if (calc == "n") {
                        k1 <- ceiling(res_n1 * de / self$options$cluster_size)
                        res_n1 <- k1 * self$options$cluster_size
                        if (!is.na(res_n2)) {
                            k2 <- ceiling(res_n2 * de / self$options$cluster_size)
                            res_n2 <- k2 * self$options$cluster_size
                        } else {
                            k2 <- NA
                        }
                    } else {
                        k1 <- n_g1
                        k2 <- if (!is.na(res_n2)) round(n_g1 * n_ratio) else NA
                        res_n1 <- k1 * self$options$cluster_size
                        if (!is.na(res_n2)) res_n2 <- k2 * self$options$cluster_size
                    }
                } else {
                    k1 <- NA
                    k2 <- NA
                }
                
                res_es <- if (!is.null(res$parms[[es_arg]])) res$parms[[es_arg]] else es
                res_pw <- if (!is.null(res$power)) res$power else power
                res_a  <- if (!is.null(res$parms$alpha)) res$parms$alpha else alpha
                
                res_al_raw <- if (!is.null(res$parms$alternative)) res$parms$alternative else alt
                alt_map <- list(
                    "two.sided" = .("two-tailed"),
                    "one.sided" = .("one-tailed"),
                    "two.one.sided" = .("two one-tailed"),
                    "equivalent" = .("equivalence"),
                    "non-inferior" = .("non-inferiority"),
                    "superior" = .("superiority")
                )
                res_al <- if (!is.null(alt_map[[res_al_raw]])) alt_map[[res_al_raw]] else res_al_raw

                table$setRow(rowNo = 1, values = list(
                    n1         = res_n1,
                    n2         = res_n2,
                    k1         = k1,
                    k2         = k2,
                    power      = res_pw,
                    es         = res_es,
                    n1_user    = res_n1,
                    n2_user    = res_n2,
                    es_user    = res_es,
                    power_user = res_pw,
                    alpha      = res_a,
                    alt        = res_al
                ))
                
                calc_map <- list(
                    n = .("Sample Size (N)"), 
                    power = .("Power"), 
                    es = .("Effect size")
                )
                
                design_text <- switch(design,
                    independent = .("Independent Samples (Student)"),
                    welch = .("Independent Samples (Welch)"),
                    independent_np = .("Independent Samples (Wilcoxon)"),
                    paired = .("Paired Samples (Student)"),
                    paired_np = .("Paired Samples (Wilcoxon)"),
                    one.sample = .("One Sample (Student)"),
                    one.sample_np = .("One Sample (Wilcoxon)"),
                    onecor = .("One Sample Correlation (Pearson)"),
                    onecor_np = .("One Sample Correlation (Spearman)"),
                    linear = .("Linear Regression (F-Test)"),
                    logistic = .("Logistic Regression (Wald Z-Test)"),
                    chisq = .("Chi-square Goodness-of-Fit / Independence (Chi-square Test)"),
                    anova = .("ANOVA / ANCOVA (F-Test)"),
                    survival = .("Survival / Log-Rank Test")
                )
                
                table$setNote("calc_note", paste0(.("<b>Calculated parameter</b>:"), " ", calc_map[[calc]]))
                table$setNote("design_note", paste0(.("<b>Design</b>:"), " ", design_text))
                
                es_measure_text <- switch(es_arg,
                    d = .("<b>Effect size measure</b>: Cohen's d"),
                    rho = .("<b>Effect size measure</b>: correlation coefficient (r)"),
                    r.squared.change = .("<b>Effect size measure</b>: R-squared change (f\u00B2)"),
                    odds.ratio = .("<b>Effect size measure</b>: Odds Ratio"),
                    w = .("<b>Effect size measure</b>: Cohen's w"),
                    eta.squared = .("<b>Effect size measure</b>: Eta-squared (\u03B7\u00B2)"),
                    es = .("<b>Effect size measure</b>: Hazard Ratio (HR)")
                )
                table$setNote("es_measure_note", es_measure_text)
                
                if (is_spearman) {
                    table$setNote("spearman", .("Power calculation for Spearman's correlation is a penalized asymptotic approximation based on Fisher's Z-transformation (ARE = 0.912)."))
                }
                
                if (design == "survival") {
                    table$setNote("survival_note", .("Calculations for survival log-rank test are based on Schoenfeld's asymptotic approximation. N1 represents the total number of events required."))
                }
                
                part1 <- if (design == "survival") {
                    .("<b>Events (N1)</b> represents the required number of target events (e.g., deaths or failures) to achieve the desired power. In survival analysis, statistical power is driven by the number of events rather than the total number of subjects.")
                } else if (self$options$crt) {
                    .("<b>Clusters (k1/k2)</b> represent the required number of clusters (e.g., clinics or schools) per group. This is adjusted for similarity among subjects within the same cluster using the Intraclass Correlation Coefficient (<b>ICC</b>) and average cluster size via the Design Effect.")
                } else if (show_n2) {
                    .("<b>N1</b> and <b>N2</b> represent the required number of subjects in Group 1 and Group 2. The ratio of their sizes is determined by the specified allocation ratio.")
                } else {
                    .("<b>N1</b> represents the required sample size (total number of subjects in the single group, or total number of pairs for paired comparisons).")
                }
                
                part2 <- .("<b>Power</b> is the probability of correctly detecting a statistically significant difference when a true difference exists in the population (1 - Type II error rate, or the probability of avoiding a false negative).")
                
                part3 <- if (design == "survival") {
                    .("<b>Hazard Ratio (HR)</b> represents the ratio of event rates between the two groups. A Hazard Ratio of 1 indicates no difference in risk, while values further from 1 represent stronger effects.")

                } else {
                    .("<b>Effect Size</b> measures the standardized magnitude of the difference or association (e.g., Cohen's d for means, or correlation coefficient). Unlike p-values, it is independent of the sample size.")
                }
                
                part4 <- .("<b>Alpha (α)</b> is the significance level, representing the maximum acceptable probability of committing a Type I error (detecting a false positive difference when none actually exists).")
                
                table$setNote("metrics_discussion", paste(part1, part2, part3, part4, sep = " "))
            }
        },
        
        .plot = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("pwrss", quietly = TRUE)) return(FALSE)
            if (!self$options$plot_err) return(FALSE)
            
            calc      <- self$options$calc
            design    <- self$options$design
            es        <- self$options$es
            power     <- self$options$power
            n_g1      <- self$options$n
            alt       <- self$options$alt
            alpha     <- as.numeric(self$options$alpha)
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            built_args <- private$.buildArgs(calc, design, es, power, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = TRUE)
            args_list <- built_args$args_list
            func_to_call <- built_args$func_to_call
            es_arg <- built_args$es_arg
            n_arg <- built_args$n_arg
            n_mult <- built_args$n_mult

            if (design == "survival") {
                # Survival / Log-Rank calculation (Schoenfeld formula)
                if (alt %in% c("two.sided", "equivalent")) {
                    z_crit <- qnorm(1 - alpha / 2)
                } else {
                    z_crit <- qnorm(1 - alpha)
                }
                
                p1_alloc <- 1 / (1 + n_ratio)
                p2_alloc <- n_ratio / (1 + n_ratio)
                p1_p2 <- p1_alloc * p2_alloc
                
                if (calc == "n") {
                    z_beta <- qnorm(power)
                    if (abs(log(es)) < 1e-4) {
                        res <- try(stop("Hazard Ratio cannot be 1 when calculating sample size/events."), silent = TRUE)
                    } else {
                        d_events <- (z_crit + z_beta)^2 / (p1_p2 * (log(es))^2)
                        res_n1 <- ceiling(d_events)
                        res_n2 <- NA
                        res_pw <- power
                        res_es <- es
                        
                        res <- list(
                            n = res_n1,
                            power = res_pw,
                            parms = list(
                                alpha = alpha,
                                alternative = alt,
                                es = res_es
                            )
                        )
                        class(res) <- "pwrss"
                    }
                } else if (calc == "power") {
                    d_events <- n_g1
                    z_beta <- sqrt(d_events * p1_p2) * abs(log(es)) - z_crit
                    res_pw <- pnorm(z_beta)
                    res_n1 <- n_g1
                    res_n2 <- NA
                    res_es <- es
                    
                    res <- list(
                        n = res_n1,
                        power = res_pw,
                        parms = list(
                            alpha = alpha,
                            alternative = alt,
                            es = res_es
                        )
                    )
                    class(res) <- "pwrss"
                } else if (calc == "es") {
                    d_events <- n_g1
                    z_beta <- qnorm(power)
                    log_hr <- - (z_crit + z_beta) / sqrt(d_events * p1_p2)
                    res_es <- exp(log_hr)
                    res_n1 <- n_g1
                    res_n2 <- NA
                    res_pw <- power
                    
                    res <- list(
                        n = res_n1,
                        power = res_pw,
                        parms = list(
                            alpha = alpha,
                            alternative = alt,
                            es = res_es
                        )
                    )
                    class(res) <- "pwrss"
                }
            } else if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w", "prob", "p1", "prob1")) {
                    interval <- c(1e-4, 0.99)
                } else if (es_arg == "odds.ratio") {
                    interval <- c(1.0001, 10)
                } else {
                    interval <- c(1e-4, 5)
                }
                
                opt <- try(uniroot(get_pwr, interval = interval, extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                
                args_list[[es_arg]] <- opt$root
                args_list$power <- NULL
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                
            } else if (calc == "n") {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        tmp_args <- args_list
                        if (n_arg == "n2") {
                            tmp_args$n2 <- max(2, round(n_val * n_mult))
                        } else if (is_cor) {
                            eff_n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                            tmp_args$n <- eff_n
                        } else {
                            tmp_args[[n_arg]] <- max(4, round(n_val))
                        }
                        tmp_args$power <- NULL
                        tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    
                    if (n_arg == "n2") {
                        args_list$n2 <- max(2, round(opt$root * n_mult))
                    } else if (is_cor) {
                        calc_n <- max(4, round(opt$root))
                        args_list$n <- if (is_spearman) max(4, round(calc_n * are_spearman)) else calc_n
                    } else {
                        args_list[[n_arg]] <- max(4, round(opt$root))
                    }
                    args_list$power <- NULL
                    res <- try(do.call(func_to_call, args_list), silent = TRUE)
                } else {
                    if (is_spearman) res$n <- ceiling(res$n / are_spearman)
                }
            } else {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
            }

            if (!inherits(res, "try-error")) {
                if (design == "survival") {
                    p1_alloc <- 1 / (1 + n_ratio)
                    p2_alloc <- n_ratio / (1 + n_ratio)
                    p1_p2 <- p1_alloc * p2_alloc
                    
                    pwrss::power.z.test(
                        mean        = log(res$parms$es) * sqrt(res$n * p1_p2),
                        sd          = 1,
                        null.mean   = 0,
                        null.sd     = 1,
                        alpha       = res$parms$alpha,
                        alternative = ifelse(res$parms$alternative %in% c("two.sided", "equivalent"), "two.sided", "one.sided"),
                        plot        = TRUE,
                        verbose     = FALSE
                    )
                } else if (!is.null(res$df1) && !is.null(res$df2)) {
                    pwrss::power.f.test(
                        ncp         = res$ncp,
                        null.ncp    = if (!is.null(res$null.ncp)) res$null.ncp else 0,
                        df1         = res$df1,
                        df2         = res$df2,
                        alpha       = res$parms$alpha,
                        plot        = TRUE,
                        verbose     = FALSE
                    )
                } else if (!is.null(res$df) && res$test == "Chi-square Test") {
                    pwrss::power.chisq.test(
                        ncp         = res$ncp,
                        null.ncp    = if (!is.null(res$null.ncp)) res$null.ncp else 0,
                        df          = res$df,
                        alpha       = res$parms$alpha,
                        plot        = TRUE,
                        verbose     = FALSE
                    )
                } else if (!is.null(res$df)) {
                    pwrss::power.t.test(
                        ncp         = res$ncp,
                        null.ncp    = if (!is.null(res$null.ncp)) res$null.ncp else 0,
                        df          = res$df,
                        alpha       = res$parms$alpha,
                        alternative = res$parms$alternative,
                        plot        = TRUE,
                        verbose     = FALSE
                    )
                } else {
                    pwrss::power.z.test(
                        mean        = res$mean,
                        sd          = res$sd,
                        null.mean   = res$null.mean,
                        null.sd     = res$null.sd,
                        alpha       = res$parms$alpha,
                        alternative = res$parms$alternative,
                        plot        = TRUE,
                        verbose     = FALSE
                    )
                }
                return(TRUE)
            } else {
                return(FALSE)
            }
        },
        
        .powerCurve = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("pwrss", quietly = TRUE)) return(FALSE)
            if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)
            if (!self$options$power_curve) return(FALSE)
            
            calc      <- self$options$calc
            design    <- self$options$design
            es        <- self$options$es
            power_val <- self$options$power
            n_g1      <- self$options$n
            alt       <- self$options$alt
            alpha     <- as.numeric(self$options$alpha)
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            built_args <- private$.buildArgs(calc, design, es, power_val, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = FALSE)
            args_list <- built_args$args_list
            func_to_call <- built_args$func_to_call
            es_arg <- built_args$es_arg
            n_arg <- built_args$n_arg
            n_mult <- built_args$n_mult

            # 1. Determine arg_d (minimally interesting ES or HR)
            if (design == "survival") {
                if (calc == "es") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(power_val)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    log_hr <- - (z_crit + z_beta) / sqrt(n_g1 * p1_p2)
                    arg_d <- exp(log_hr)
                } else {
                    arg_d <- es
                }
            } else if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    if (n_arg == "n2") {
                        tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    } else if (is_cor) {
                        tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                    } else {
                        tmp_args[[n_arg]] <- max(4, round(n_g1))
                    }
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w", "prob", "p1", "prob1")) {
                    interval <- c(1e-4, 0.99)
                } else if (es_arg == "odds.ratio") {
                    interval <- c(1.0001, 10)
                } else {
                    interval <- c(1e-4, 5)
                }
                
                opt <- try(uniroot(get_pwr, interval = interval, extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            # 2. Determine target_n1 (effective individual sample size / event count)
            if (design == "survival") {
                target_n1 <- if (calc == "n") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(power_val)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    ceiling((z_crit + z_beta)^2 / (p1_p2 * (log(arg_d))^2))
                } else {
                    n_g1
                }
            } else if (calc == "n") {
                tmp_args <- args_list
                tmp_args[[es_arg]] <- arg_d
                tmp_args$power <- power_val
                if (n_arg == "n2") tmp_args$n2 <- NULL else if (is_cor) tmp_args$n <- NULL else tmp_args[[n_arg]] <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        t_args[[es_arg]] <- arg_d
                        if (n_arg == "n2") {
                            t_args$n2 <- max(2, round(n_val * n_mult))
                        } else if (is_cor) {
                            t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            t_args[[n_arg]] <- max(4, round(n_val))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else {
                    res_n1 <- if (n_arg == "n.total") res$n.total else res$n
                    if (length(res_n1) > 0) res_n1 <- res_n1[1]
                    target_n1 <- if(is_spearman) ceiling(res_n1 / are_spearman) else res_n1
                }
            } else { target_n1 <- n_g1 }
            
            # 3. Determine target_pwr
            if (design == "survival") {
                if (calc == "power") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    z_beta <- sqrt(target_n1 * p1_p2) * abs(log(arg_d)) - z_crit
                    target_pwr <- pnorm(z_beta)
                } else {
                    target_pwr <- power_val
                }
            } else if (calc == "power") {
                tmp_args <- args_list
                tmp_args[[es_arg]] <- arg_d
                if (n_arg == "n2") {
                    tmp_args$n2 <- max(2, round(target_n1 * n_mult))
                } else if (is_cor) {
                    tmp_args$n <- if (is_spearman) max(4, round(target_n1 * are_spearman)) else max(4, round(target_n1))
                } else {
                    tmp_args[[n_arg]] <- max(4, round(target_n1))
                }
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            # 4. CRT and Sequence preparation
            crt <- self$options$crt
            cluster_size <- self$options$cluster_size
            icc <- self$options$icc
            de <- 1 + (cluster_size - 1) * icc
            
            if (crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                if (calc == "n") {
                    target_plot_n <- ceiling(target_n1 * de / cluster_size)
                } else {
                    target_plot_n <- target_n1
                }
                x_label <- .("Number of Clusters (k1)")
            } else {
                target_plot_n <- target_n1
                x_label <- if (design == "survival") .("Number of Events") else .("Sample Size (N1)")
            }
            
            n_seq <- seq(if(is_cor) 4 else 2, max(50, ceiling(target_plot_n * 2.5)), length.out = 50)
            pwr_seq <- numeric(length(n_seq))
            
            for (i in seq_along(n_seq)) {
                if (design == "survival") {
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- sqrt(n_seq[i] * p1_p2) * abs(log(arg_d)) - z_crit
                    pwr_seq[i] <- pnorm(z_beta)
                } else {
                    t_args <- args_list
                    t_args[[es_arg]] <- arg_d
                    
                    # Convert cluster count to effective N if CRT
                    if (crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                        eff_n_i <- (n_seq[i] * cluster_size) / de
                    } else {
                        eff_n_i <- n_seq[i]
                    }
                    
                    if (n_arg == "n2") {
                        t_args$n2 <- max(2, round(eff_n_i * n_mult))
                    } else if (is_cor) {
                        t_args$n <- if (is_spearman) max(4, round(eff_n_i * are_spearman)) else max(4, round(eff_n_i))
                    } else {
                        t_args[[n_arg]] <- max(4, round(eff_n_i))
                    }
                    t_args$power <- NULL
                    tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                    pwr_seq[i] <- if (!inherits(tmp, "try-error")) tmp$power else NA
                }
            }
            
            df_plot <- na.omit(data.frame(N = n_seq, Power = pwr_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = N, y = Power)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_pwr, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = target_plot_n, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("Power Curve"), x = x_label, y = .("Power")) +
                ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) + 
                ggtheme +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "#EAEAEA", linewidth = 0.5),
                    panel.grid.minor = ggplot2::element_line(color = "#F5F5F5", linewidth = 0.25)
                )
            
            print(p)
            return(TRUE)
        },
        
        .powerEsCurve = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("pwrss", quietly = TRUE)) return(FALSE)
            if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)
            if (!self$options$power_es_curve) return(FALSE)
            
            calc      <- self$options$calc
            design    <- self$options$design
            es        <- self$options$es
            power_val <- self$options$power
            n_g1      <- self$options$n
            alt       <- self$options$alt
            alpha     <- as.numeric(self$options$alpha)
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            built_args <- private$.buildArgs(calc, design, es, power_val, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = FALSE)
            args_list <- built_args$args_list
            func_to_call <- built_args$func_to_call
            es_arg <- built_args$es_arg
            n_arg <- built_args$n_arg
            n_mult <- built_args$n_mult

            # 1. Determine arg_d (minimally interesting ES or HR)
            if (design == "survival") {
                if (calc == "es") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(power_val)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    log_hr <- - (z_crit + z_beta) / sqrt(n_g1 * p1_p2)
                    arg_d <- exp(log_hr)
                } else {
                    arg_d <- es
                }
            } else if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    if (n_arg == "n2") {
                        tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    } else if (is_cor) {
                        tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                    } else {
                        tmp_args[[n_arg]] <- max(4, round(n_g1))
                    }
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w", "prob", "p1", "prob1")) {
                    interval <- c(1e-4, 0.99)
                } else if (es_arg == "odds.ratio") {
                    interval <- c(1.0001, 10)
                } else {
                    interval <- c(1e-4, 5)
                }
                
                opt <- try(uniroot(get_pwr, interval = interval, extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            # 2. Determine target_n1 (effective individual sample size / event count)
            if (design == "survival") {
                target_n1 <- if (calc == "n") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(power_val)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    ceiling((z_crit + z_beta)^2 / (p1_p2 * (log(arg_d))^2))
                } else {
                    n_g1
                }
            } else if (calc == "n") {
                tmp_args <- args_list
                tmp_args[[es_arg]] <- arg_d
                tmp_args$power <- power_val
                if (n_arg == "n2") tmp_args$n2 <- NULL else if (is_cor) tmp_args$n <- NULL else tmp_args[[n_arg]] <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        t_args[[es_arg]] <- arg_d
                        if (n_arg == "n2") {
                            t_args$n2 <- max(2, round(n_val * n_mult))
                        } else if (is_cor) {
                            t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            t_args[[n_arg]] <- max(4, round(n_val))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else {
                    res_n1 <- if (n_arg == "n.total") res$n.total else res$n
                    if (length(res_n1) > 0) res_n1 <- res_n1[1]
                    target_n1 <- if(is_spearman) ceiling(res_n1 / are_spearman) else res_n1
                }
            } else { target_n1 <- n_g1 }
            
            # 3. Determine target_pwr
            if (design == "survival") {
                if (calc == "power") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    z_beta <- sqrt(target_n1 * p1_p2) * abs(log(arg_d)) - z_crit
                    target_pwr <- pnorm(z_beta)
                } else {
                    target_pwr <- power_val
                }
            } else if (calc == "power") {
                tmp_args <- args_list
                tmp_args[[es_arg]] <- arg_d
                if (n_arg == "n2") {
                    tmp_args$n2 <- max(2, round(target_n1 * n_mult))
                } else if (is_cor) {
                    tmp_args$n <- if (is_spearman) max(4, round(target_n1 * are_spearman)) else max(4, round(target_n1))
                } else {
                    tmp_args[[n_arg]] <- max(4, round(target_n1))
                }
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            # 4. CRT Adjustment for sample size
            crt <- self$options$crt
            cluster_size <- self$options$cluster_size
            icc <- self$options$icc
            de <- 1 + (cluster_size - 1) * icc
            
            if (crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                if (calc != "n") {
                    target_eff_n <- (target_n1 * cluster_size) / de
                } else {
                    target_eff_n <- target_n1
                }
            } else {
                target_eff_n <- target_n1
            }
            
            if (es_arg %in% c("rho", "r.squared.change", "eta.squared")) {
                d_seq <- seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50)
            } else {
                d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            }
            pwr_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                if (design == "survival") {
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- sqrt(target_eff_n * p1_p2) * abs(log(d_seq[i])) - z_crit
                    pwr_seq[i] <- pnorm(z_beta)
                } else {
                    t_args <- args_list
                    t_args[[es_arg]] <- d_seq[i]
                    if (n_arg == "n2") {
                        t_args$n2 <- max(2, round(target_eff_n * n_mult))
                    } else if (is_cor) {
                        t_args$n <- if (is_spearman) max(4, round(target_eff_n * are_spearman)) else max(4, round(target_eff_n))
                    } else {
                        t_args[[n_arg]] <- max(4, round(target_eff_n))
                    }
                    t_args$power <- NULL
                    tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                    pwr_seq[i] <- if (!inherits(tmp, "try-error")) tmp$power else NA
                }
            }
            
            df_plot <- na.omit(data.frame(ES = d_seq, Power = pwr_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            x_label <- if (design == "survival") .("Hazard Ratio") else .("Effect size")
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = Power)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_pwr, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = arg_d, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("Power by Effect Size"), x = x_label, y = .("Power")) +
                ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) + 
                ggtheme +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "#EAEAEA", linewidth = 0.5),
                    panel.grid.minor = ggplot2::element_line(color = "#F5F5F5", linewidth = 0.25)
                )
            
            print(p)
            return(TRUE)
        },
        
        .nEsCurve = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("pwrss", quietly = TRUE)) return(FALSE)
            if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)
            if (!self$options$n_es_curve) return(FALSE)
            
            calc      <- self$options$calc
            design    <- self$options$design
            es        <- self$options$es
            power_val <- self$options$power
            n_g1      <- self$options$n
            alt       <- self$options$alt
            alpha     <- as.numeric(self$options$alpha)
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            built_args <- private$.buildArgs(calc, design, es, power_val, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = FALSE)
            args_list <- built_args$args_list
            func_to_call <- built_args$func_to_call
            es_arg <- built_args$es_arg
            n_arg <- built_args$n_arg
            n_mult <- built_args$n_mult

            # 1. Determine arg_d
            if (design == "survival") {
                if (calc == "es") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(power_val)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    log_hr <- - (z_crit + z_beta) / sqrt(n_g1 * p1_p2)
                    arg_d <- exp(log_hr)
                } else {
                    arg_d <- es
                }
            } else if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    if (n_arg == "n2") {
                        tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    } else if (is_cor) {
                        tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                    } else {
                        tmp_args[[n_arg]] <- max(4, round(n_g1))
                    }
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w", "prob", "p1", "prob1")) {
                    interval <- c(1e-4, 0.99)
                } else if (es_arg == "odds.ratio") {
                    interval <- c(1.0001, 10)
                } else {
                    interval <- c(1e-4, 5)
                }
                
                opt <- try(uniroot(get_pwr, interval = interval, extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            # 2. Determine target_pwr
            if (design == "survival") {
                if (calc == "power") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    z_beta <- sqrt(n_g1 * p1_p2) * abs(log(arg_d)) - z_crit
                    target_pwr <- pnorm(z_beta)
                } else {
                    target_pwr <- power_val
                }
            } else if (calc == "power") {
                tmp_args <- args_list
                tmp_args[[es_arg]] <- arg_d
                if (n_arg == "n2") {
                    tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                } else if (is_cor) {
                    tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                } else {
                    tmp_args[[n_arg]] <- max(4, round(n_g1))
                }
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            # 3. Determine target_n1
            if (design == "survival") {
                target_n1 <- if (calc == "n") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(target_pwr)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    ceiling((z_crit + z_beta)^2 / (p1_p2 * (log(arg_d))^2))
                } else {
                    n_g1
                }
            } else if (calc == "n") {
                tmp_args <- args_list
                tmp_args[[es_arg]] <- arg_d
                tmp_args$power <- target_pwr
                if (n_arg == "n2") tmp_args$n2 <- NULL else if (is_cor) tmp_args$n <- NULL else tmp_args[[n_arg]] <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        t_args[[es_arg]] <- arg_d
                        if (n_arg == "n2") {
                            t_args$n2 <- max(2, round(n_val * n_mult))
                        } else if (is_cor) {
                            t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            t_args[[n_arg]] <- max(4, round(n_val))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - target_pwr)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else {
                    res_n1 <- if (n_arg == "n.total") res$n.total else res$n
                    if (length(res_n1) > 0) res_n1 <- res_n1[1]
                    target_n1 <- if(is_spearman) ceiling(res_n1 / are_spearman) else res_n1
                }
            } else { target_n1 <- n_g1 }

            # 4. CRT and Sequence preparation
            crt <- self$options$crt
            cluster_size <- self$options$cluster_size
            icc <- self$options$icc
            de <- 1 + (cluster_size - 1) * icc
            
            if (crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                if (calc == "n") {
                    target_plot_n <- ceiling(target_n1 * de / cluster_size)
                } else {
                    target_plot_n <- target_n1
                }
                y_label <- .("Number of Clusters (k1)")
            } else {
                target_plot_n <- target_n1
                y_label <- if (design == "survival") .("Number of Events") else .("Sample Size (N1)")
            }

            if (es_arg %in% c("rho", "r.squared.change", "eta.squared")) {
                d_seq <- seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50)
            } else {
                d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            }
            n_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                if (design == "survival") {
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(target_pwr)
                    if (abs(log(d_seq[i])) < 1e-4) {
                        n_seq[i] <- NA
                    } else {
                        n_seq[i] <- min(5000, (z_crit + z_beta)^2 / (p1_p2 * (log(d_seq[i]))^2))
                    }
                } else {
                    t_args <- args_list
                    t_args[[es_arg]] <- d_seq[i]
                    t_args$power <- target_pwr
                    if (n_arg == "n2") {
                        t_args$n2 <- NULL
                    } else if (is_cor) {
                        t_args$n <- NULL
                    } else {
                        t_args[[n_arg]] <- NULL
                    }
                    tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                    
                    if (!inherits(tmp, "try-error")) {
                        res_n1 <- if (n_arg == "n.total") tmp$n.total else tmp$n
                        if (length(res_n1) > 0) res_n1 <- res_n1[1]
                        eff_n <- if (is_spearman) ceiling(res_n1 / are_spearman) else res_n1
                        
                        if (crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                            n_seq[i] <- ceiling(eff_n * de / cluster_size)
                        } else {
                            n_seq[i] <- eff_n
                        }
                    } else {
                        get_n_loop <- function(n_val) {
                            tt_args <- args_list
                            tt_args[[es_arg]] <- d_seq[i]
                            
                            # Convert search value if CRT
                            if (crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                                eff_search_n <- (n_val * cluster_size) / de
                            } else {
                                eff_search_n <- n_val
                            }
                            
                            if (n_arg == "n2") {
                                tt_args$n2 <- max(2, round(eff_search_n * n_mult))
                            } else if (is_cor) {
                                tt_args$n <- if (is_spearman) max(4, round(eff_search_n * are_spearman)) else max(4, round(eff_search_n))
                            } else {
                                tt_args[[n_arg]] <- max(4, round(eff_search_n))
                            }
                            tt_args$power <- NULL
                            tmp2 <- try(do.call(func_to_call, tt_args), silent = TRUE)
                            if (inherits(tmp2, "try-error")) return(-0.99)
                            return(tmp2$power - target_pwr)
                        }
                        
                        opt <- try(uniroot(get_n_loop, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                        n_seq[i] <- if (!inherits(opt, "try-error")) opt$root else NA
                    }
                }
            }
            
            df_plot <- na.omit(data.frame(ES = d_seq, N = n_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            x_label <- if (design == "survival") .("Hazard Ratio") else .("Effect size")
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = N)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_plot_n, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = arg_d, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("N by Effect Size"), x = x_label, y = y_label) +
                ggtheme +
                ggplot2::theme(
                    panel.grid.major = ggplot2::element_line(color = "#EAEAEA", linewidth = 0.5),
                    panel.grid.minor = ggplot2::element_line(color = "#F5F5F5", linewidth = 0.25)
                )
            
            print(p)
            return(TRUE)
        },
        .powerEsAlphaCurve = function(image, ggtheme, theme, ...) {
            if (!requireNamespace("pwrss", quietly = TRUE)) return(FALSE)
            if (!requireNamespace("ggplot2", quietly = TRUE)) return(FALSE)
            if (!self$options$power_es_alpha_curve) return(FALSE)
            
            calc      <- self$options$calc
            design    <- self$options$design
            es        <- self$options$es
            power_val <- self$options$power
            n_g1      <- self$options$n
            alt       <- self$options$alt
            alpha     <- as.numeric(self$options$alpha)
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            # 1. Get arg_d (minimally interesting effect size)
            if (design == "survival") {
                arg_d <- es
                target_n1 <- if (calc == "n") {
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - alpha / 2) else z_crit <- qnorm(1 - alpha)
                    z_beta <- qnorm(power_val)
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    ceiling((z_crit + z_beta)^2 / (p1_p2 * (log(es))^2))
                } else {
                    n_g1
                }
            } else {
                built_args <- private$.buildArgs(calc, design, es, power_val, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = FALSE)
                args_list <- built_args$args_list
                func_to_call <- built_args$func_to_call
                es_arg <- built_args$es_arg
                n_arg <- built_args$n_arg
                n_mult <- built_args$n_mult

                if (calc == "es") {
                    get_pwr <- function(d_val) {
                        tmp_args <- args_list
                        tmp_args[[es_arg]] <- d_val
                        if (n_arg == "n2") {
                            tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                        } else if (is_cor) {
                            tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                        } else {
                            tmp_args[[n_arg]] <- max(4, round(n_g1))
                        }
                        tmp_args$power <- NULL
                        tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    
                    if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w", "prob", "p1", "prob1")) {
                        interval <- c(1e-4, 0.99)
                    } else if (es_arg == "odds.ratio") {
                        interval <- c(1.0001, 10)
                    } else {
                        interval <- c(1e-4, 5)
                    }
                    
                    opt <- try(uniroot(get_pwr, interval = interval, extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    arg_d <- opt$root
                } else { arg_d <- es }
                
                if (calc == "n") {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- arg_d
                    tmp_args$power <- power_val
                    if (n_arg == "n2") tmp_args$n2 <- NULL else if (is_cor) tmp_args$n <- NULL else tmp_args[[n_arg]] <- NULL
                    res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(res, "try-error")) {
                        get_n <- function(n_val) {
                            t_args <- args_list
                            t_args[[es_arg]] <- arg_d
                            if (n_arg == "n2") {
                                t_args$n2 <- max(2, round(n_val * n_mult))
                            } else if (is_cor) {
                                t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                            } else {
                                t_args[[n_arg]] <- max(4, round(n_val))
                            }
                            t_args$power <- NULL
                            tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                            if (inherits(tmp, "try-error")) return(-0.99)
                            return(tmp$power - power_val)
                        }
                        opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                        if (inherits(opt, "try-error")) return(FALSE)
                        target_n1 <- opt$root
                    } else {
                        res_n1 <- if (n_arg == "n.total") res$n.total else res$n
                        if (length(res_n1) > 0) res_n1 <- res_n1[1]
                        target_n1 <- if(is_spearman) ceiling(res_n1 / are_spearman) else res_n1
                    }
                } else { target_n1 <- n_g1 }
            }

            # 2. Define sequence of effect sizes
            if (design == "survival") {
                d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            } else if (es_arg %in% c("rho", "r.squared.change", "eta.squared")) {
                d_seq <- seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50)
            } else {
                d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            }

            # 3. Calculate power for alpha = 0.05, 0.01, 0.005
            alphas_to_test <- c(0.05, 0.01, 0.005)
            plot_list <- list()

            for (a in alphas_to_test) {
                pwr_seq <- numeric(length(d_seq))
                
                if (design == "survival") {
                    p1_p2 <- (n_ratio) / (1 + n_ratio)^2
                    if (alt %in% c("two.sided", "equivalent")) z_crit <- qnorm(1 - a / 2) else z_crit <- qnorm(1 - a)
                    for (i in seq_along(d_seq)) {
                        z_beta <- sqrt(target_n1 * p1_p2) * abs(log(d_seq[i])) - z_crit
                        pwr_seq[i] <- pnorm(z_beta)
                    }
                } else {
                    built_args_a <- private$.buildArgs(calc, design, es, power_val, target_n1, alt, a, n_ratio, var_ratio, include_n_es_power = FALSE)
                    t_args <- built_args_a$args_list
                    func_to_call <- built_args_a$func_to_call
                    es_arg <- built_args_a$es_arg
                    n_arg <- built_args_a$n_arg
                    n_mult <- built_args_a$n_mult

                    for (i in seq_along(d_seq)) {
                        t_args[[es_arg]] <- d_seq[i]
                        if (n_arg == "n2") {
                            t_args$n2 <- max(2, round(target_n1 * n_mult))
                        } else if (is_cor) {
                            t_args$n <- if (is_spearman) max(4, round(target_n1 * are_spearman)) else max(4, round(target_n1))
                        } else {
                            t_args[[n_arg]] <- max(4, round(target_n1))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        pwr_seq[i] <- if (!inherits(tmp, "try-error")) tmp$power else NA
                    }
                }
                
                a_label <- if (abs(a - 0.05) < 1e-4) "0.05" else if (abs(a - 0.01) < 1e-4) "0.01" else "0.005"
                plot_list[[a_label]] <- data.frame(ES = d_seq, Power = pwr_seq, Alpha = a_label)
            }

            df_plot <- do.call(rbind, plot_list)
            df_plot <- na.omit(df_plot)
            df_plot$Alpha <- factor(df_plot$Alpha, levels = c("0.05", "0.01", "0.005"))
            if (nrow(df_plot) == 0) return(FALSE)
            
            x_label <- if (design == "survival") .("Hazard Ratio") else .("Effect size")
            
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = Power, color = Alpha)) +
                ggplot2::geom_line(linewidth = 1.2) +
                ggplot2::labs(title = .("Power by Effect Size and Alpha"), x = x_label, y = .("Power")) +
                ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
                ggplot2::scale_color_manual(values = c("0.05" = "#3366B2", "0.01" = "#E54028", "0.005" = "#56B4E9")) +
                ggtheme +
                ggplot2::theme(
                    legend.position = "bottom",
                    panel.grid.major = ggplot2::element_line(color = "#EAEAEA", linewidth = 0.5),
                    panel.grid.minor = ggplot2::element_line(color = "#F5F5F5", linewidth = 0.25)
                )
            
            print(p)
            return(TRUE)
        },
        
        .buildArgs = function(calc, design, es, power, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = TRUE) {
            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            # Map alternative hypotheses for pwrss package
            pwrss_alt <- switch(alt,
                two.sided = "two.sided",
                one.sided = "one.sided",
                equivalent = "two.one.sided",
                `non-inferior` = "one.sided",
                superior = "one.sided"
            )
            
            # CRT Adjustment for sample size
            if (self$options$crt && design %in% c("independent", "welch", "independent_np", "paired_np", "one.sample_np", "paired", "one.sample")) {
                de <- 1 + (self$options$cluster_size - 1) * self$options$icc
                if (calc != "n") {
                    n_g1 <- (n_g1 * self$options$cluster_size) / de
                }
            }
            
            arg_d     <- if (calc == "es") NULL else es
            arg_power <- if (calc == "power") NULL else power
            
            es_arg <- "d"
            n_arg  <- "n2"
            
            args_list <- list(
                alpha       = alpha,
                alternative = pwrss_alt,
                verbose     = FALSE
            )
            
            if (include_n_es_power) {
                args_list$power <- arg_power
            }
            
            if (design == "survival") {
                func_to_call <- NULL
                es_arg <- "es"
                n_arg <- "n"
                n_mult <- 1
                args_list <- list()
            } else if (is_cor) {
                n_mult <- 1
                if (include_n_es_power) args_list$rho <- arg_d
                args_list$null.rho <- 0
                func_to_call <- pwrss::power.z.onecor
                es_arg <- "rho"
                n_arg  <- "n"
                if (include_n_es_power) {
                    eff_n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, n_g1)
                    args_list$n <- if (calc == "n") NULL else eff_n
                }
            } else if (design == "linear") {
                n_mult <- 1
                if (include_n_es_power) args_list$r.squared.change <- arg_d
                args_list$k.total  <- self$options$k_total
                args_list$k.tested <- self$options$k_tested
                func_to_call <- pwrss::power.f.regression
                es_arg <- "r.squared.change"
                n_arg  <- "n"
                if (include_n_es_power) {
                    args_list$n <- if (calc == "n") NULL else max(4, n_g1)
                }
            } else if (design == "logistic") {
                n_mult <- 1
                if (include_n_es_power) args_list$odds.ratio <- arg_d
                args_list$base.prob           <- self$options$base_prob
                args_list$r.squared.predictor <- self$options$r2_predictor
                func_to_call <- pwrss::power.z.logistic
                es_arg <- "odds.ratio"
                n_arg  <- "n"
                if (include_n_es_power) {
                    args_list$n <- if (calc == "n") NULL else max(4, n_g1)
                }
            } else if (design == "chisq") {
                n_mult <- 1
                if (include_n_es_power) args_list$w <- arg_d
                args_list$df      <- self$options$df_chisq
                func_to_call <- pwrss::power.chisq.gof
                es_arg <- "w"
                n_arg  <- "n"
                if (include_n_es_power) {
                    args_list$n <- if (calc == "n") NULL else max(4, n_g1)
                }
            } else if (design == "anova") {
                n_mult <- 1
                if (include_n_es_power) args_list$eta.squared <- arg_d
                args_list$factor.levels <- self$options$factor_levels
                args_list$k.covariates  <- self$options$k_covariates
                func_to_call <- pwrss::power.f.ancova
                es_arg <- "eta.squared"
                n_arg  <- "n.total"
                if (include_n_es_power) {
                    args_list$n.total <- if (calc == "n") NULL else max(4, n_g1)
                }
            } else {
                if (include_n_es_power) args_list$d <- arg_d
                if (alt == "equivalent") {
                    args_list$margin <- self$options$margin
                } else if (alt == "non-inferior") {
                    args_list$margin <- -self$options$margin
                } else if (alt == "superior") {
                    args_list$margin <- self$options$margin
                } else {
                    args_list$margin <- 0
                }
                
                if (design == "independent") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    if (include_n_es_power) args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$design <- "independent"
                    func_to_call <- pwrss::power.t.student
                } else if (design == "welch") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    if (include_n_es_power) args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$var.ratio <- var_ratio
                    func_to_call <- pwrss::power.t.welch
                } else if (design == "independent_np") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    if (include_n_es_power) args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$design <- "independent"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else if (design == "paired_np") {
                    n_mult <- 1
                    if (include_n_es_power) args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- "paired"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else if (design == "one.sample_np") {
                    n_mult <- 1
                    if (include_n_es_power) args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- "one.sample"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else {
                    n_mult <- 1
                    if (include_n_es_power) args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- design
                    func_to_call <- pwrss::power.t.student
                }
            }
            
            if (design %in% c("linear", "chisq", "anova")) {
                args_list$alternative <- NULL
            }
            
            return(list(
                args_list    = args_list,
                func_to_call = func_to_call,
                es_arg       = es_arg,
                n_arg        = n_arg,
                n_mult       = n_mult
            ))
        }
    )
)