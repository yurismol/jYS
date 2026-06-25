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

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w")) {
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
                        res <- opt
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
                
                show_n2 <- !(design %in% c("one.sample", "one.sample_np", "onecor", "onecor_np", "linear", "logistic", "chisq", "anova"))
                
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
                
                res_es <- if (!is.null(res$parms[[es_arg]])) res$parms[[es_arg]] else es
                res_pw <- if (!is.null(res$power)) res$power else power
                res_a  <- if (!is.null(res$parms$alpha)) res$parms$alpha else alpha
                
                res_al_raw <- if (!is.null(res$parms$alternative)) res$parms$alternative else alt
                alt_map <- list(
                    "two.sided" = .("two-tailed"),
                    "one.sided" = .("one-tailed"),
                    "two.one.sided" = .("two one-tailed")
                )
                res_al <- if (!is.null(alt_map[[res_al_raw]])) alt_map[[res_al_raw]] else res_al_raw

                table$setRow(rowNo = 1, values = list(
                    n1         = res_n1,
                    n2         = res_n2,
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
                    es = .("Effect Size")
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
                    anova = .("ANOVA / ANCOVA (F-Test)")
                )
                
                table$setNote("calc_note", paste0(.("<b>Calculated parameter</b>: "), calc_map[[calc]]))
                table$setNote("design_note", paste0(.("<b>Design</b>: "), design_text))
                
                es_measure_text <- switch(es_arg,
                    d = .("<b>Effect size measure</b>: Cohen's d"),
                    rho = .("<b>Effect size measure</b>: correlation coefficient (r)"),
                    r.squared.change = .("<b>Effect size measure</b>: R-squared change (f\u00B2)"),
                    odds.ratio = .("<b>Effect size measure</b>: Odds Ratio"),
                    w = .("<b>Effect size measure</b>: Cohen's w"),
                    eta.squared = .("<b>Effect size measure</b>: Eta-squared (\u03B7\u00B2)")
                )
                table$setNote("es_measure_note", es_measure_text)
                
                if (is_spearman) {
                    table$setNote("spearman", .("Power calculation for Spearman's correlation is a penalized asymptotic approximation based on Fisher's Z-transformation (ARE = 0.912)."))
                }
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

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args[[es_arg]] <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w")) {
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
                if (!is.null(res$df1) && !is.null(res$df2)) {
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
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w")) {
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
            
            if (calc == "power") {
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
            
            n_seq <- seq(if(is_cor) 4 else 2, max(50, ceiling(target_n1 * 2.5)), length.out = 50)
            pwr_seq <- numeric(length(n_seq))
            for (i in seq_along(n_seq)) {
                t_args <- args_list
                t_args[[es_arg]] <- arg_d
                if (n_arg == "n2") {
                    t_args$n2 <- max(2, round(n_seq[i] * n_mult))
                } else if (is_cor) {
                    t_args$n <- if (is_spearman) max(4, round(n_seq[i] * are_spearman)) else max(4, round(n_seq[i]))
                } else {
                    t_args[[n_arg]] <- max(4, round(n_seq[i]))
                }
                t_args$power <- NULL
                tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                pwr_seq[i] <- if (!inherits(tmp, "try-error")) tmp$power else NA
            }
            
            df_plot <- na.omit(data.frame(N = n_seq, Power = pwr_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = N, y = Power)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_pwr, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = target_n1, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("Power Curve"), x = .("Sample Size (N1)"), y = .("Power")) +
                ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) + ggtheme
            
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
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w")) {
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
            
            if (calc == "power") {
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
            
            if (es_arg %in% c("rho", "r.squared.change", "eta.squared")) {
                d_seq <- seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50)
            } else {
                d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            }
            pwr_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                t_args <- args_list
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
            
            df_plot <- na.omit(data.frame(ES = d_seq, Power = pwr_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            x_label <- .("Effect Size")
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = Power)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_pwr, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = arg_d, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("Power by Effect Size"), x = x_label, y = .("Power")) +
                ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) + ggtheme
            
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
                
                if (es_arg %in% c("rho", "r.squared.change", "eta.squared", "w")) {
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
            
            if (calc == "power") {
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
            
            if (calc == "n") {
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

            if (es_arg %in% c("rho", "r.squared.change", "eta.squared")) {
                d_seq <- seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50)
            } else {
                d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            }
            n_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
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
                    n_seq[i] <- if (is_spearman) ceiling(res_n1 / are_spearman) else res_n1
                } else {
                    get_n_loop <- function(n_val) {
                        tt_args <- args_list
                        tt_args[[es_arg]] <- d_seq[i]
                        if (n_arg == "n2") {
                            tt_args$n2 <- max(2, round(n_val * n_mult))
                        } else if (is_cor) {
                            tt_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            tt_args[[n_arg]] <- max(4, round(n_val))
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
            
            df_plot <- na.omit(data.frame(ES = d_seq, N = n_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            x_label <- .("Effect Size")
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = N)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_n1, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = arg_d, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("N by Effect Size"), x = x_label, y = .("Sample Size (N1)")) +
                ggtheme
            
            print(p)
            return(TRUE)
        },
        
        .buildArgs = function(calc, design, es, power, n_g1, alt, alpha, n_ratio, var_ratio, include_n_es_power = TRUE) {
            is_cor       <- design %in% c("onecor", "onecor_np")
            is_spearman  <- design == "onecor_np"
            are_spearman <- 0.912
            
            arg_d     <- if (calc == "es") NULL else es
            arg_power <- if (calc == "power") NULL else power
            
            es_arg <- "d"
            n_arg  <- "n2"
            
            args_list <- list(
                alpha       = alpha,
                alternative = alt,
                verbose     = FALSE
            )
            
            if (include_n_es_power) {
                args_list$power <- arg_power
            }
            
            if (is_cor) {
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