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
            
            arg_d     <- if (calc == "es") NULL else es
            arg_power <- if (calc == "power") NULL else power
            
            if (is_cor) {
                n_mult <- 1
                args_list <- list(
                    rho         = arg_d,
                    null.rho    = 0,
                    power       = arg_power,
                    alpha       = alpha,
                    alternative = alt,
                    verbose     = FALSE
                )
                eff_n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, n_g1)
                args_list$n <- if (calc == "n") NULL else eff_n
                func_to_call <- pwrss::power.z.onecor
            } else {
                args_list <- list(
                    d           = arg_d,
                    power       = arg_power,
                    alpha       = alpha,
                    alternative = alt,
                    verbose     = FALSE
                )
                
                if (design == "independent") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$design <- "independent"
                    func_to_call <- pwrss::power.t.student
                } else if (design == "welch") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$var.ratio <- var_ratio
                    func_to_call <- pwrss::power.t.welch
                } else if (design == "independent_np") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$design <- "independent"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else if (design == "paired_np") {
                    n_mult <- 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- "paired"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else if (design == "one.sample_np") {
                    n_mult <- 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- "one.sample"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else {
                    n_mult <- 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- design
                    func_to_call <- pwrss::power.t.student
                }
            }

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    if (is_cor) tmp_args$rho <- d_val else tmp_args$d <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                opt <- try(uniroot(get_pwr, interval = if (is_cor) c(1e-4, 0.99) else c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) {
                    res <- opt
                } else {
                    if (is_cor) args_list$rho <- opt$root else args_list$d <- opt$root
                    args_list$power <- NULL
                    res <- try(do.call(func_to_call, args_list), silent = TRUE)
                }
            } else if (calc == "n") {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        tmp_args <- args_list
                        if (is_cor) {
                            eff_n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                            tmp_args$n <- eff_n
                        } else {
                            tmp_args$n2 <- max(2, round(n_val * n_mult))
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
                        if (is_cor) {
                            calc_n <- max(4, round(opt$root))
                            args_list$n <- if (is_spearman) max(4, round(calc_n * are_spearman)) else calc_n
                        } else {
                            args_list$n2 <- max(2, round(opt$root * n_mult))
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
                
                show_n2 <- !(design %in% c("one.sample", "one.sample_np", "onecor", "onecor_np"))
                
                table$getColumn("n1")$setVisible(calc == "n")
                table$getColumn("n2")$setVisible(calc == "n" && show_n2)
                table$getColumn("power")$setVisible(calc == "power")
                table$getColumn("es")$setVisible(calc == "es")
                
                table$getColumn("n1_user")$setVisible(calc != "n")
                table$getColumn("n2_user")$setVisible(calc != "n" && show_n2)
                table$getColumn("es_user")$setVisible(calc != "es")
                table$getColumn("power_user")$setVisible(calc != "power")
                
                res_n1 <- if (is_cor) res$n else res$n[1]
                if (is_spearman && calc != "n") res_n1 <- n_g1
                
                if (design %in% c("independent", "welch", "independent_np")) {
                    res_n2 <- if (length(res$n) > 1) res$n[2] else res_n1
                } else if (design %in% c("paired", "paired_np")) {
                    res_n2 <- if (length(res$n) > 1) res$n[2] else res_n1
                } else {
                    res_n2 <- NA 
                }
                
                res_es <- if (is_cor) (if (!is.null(res$parms$rho)) res$parms$rho else es) else (if (!is.null(res$parms$d)) res$parms$d else es)
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
                    onecor_np = .("One Sample Correlation (Spearman)")
                )
                
                table$setNote("calc_note", paste0(.("Calculated parameter: "), calc_map[[calc]]))
                table$setNote("design_note", paste0(.("Design: "), design_text))
                
                es_measure_text <- if (is_cor) {
                    .("Effect size measure: correlation coefficient (r)")
                } else {
                    .("Effect size measure: Cohen's d")
                }
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
            
            arg_d     <- if (calc == "es") NULL else es
            arg_power <- if (calc == "power") NULL else power
            
            if (is_cor) {
                n_mult <- 1
                args_list <- list(
                    rho         = arg_d,
                    null.rho    = 0,
                    power       = arg_power,
                    alpha       = alpha,
                    alternative = alt,
                    verbose     = FALSE
                )
                eff_n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, n_g1)
                args_list$n <- if (calc == "n") NULL else eff_n
                func_to_call <- pwrss::power.z.onecor
            } else {
                args_list <- list(
                    d           = arg_d,
                    power       = arg_power,
                    alpha       = alpha,
                    alternative = alt,
                    verbose     = FALSE
                )
                
                if (design == "independent") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$design <- "independent"
                    func_to_call <- pwrss::power.t.student
                } else if (design == "welch") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$var.ratio <- var_ratio
                    func_to_call <- pwrss::power.t.welch
                } else if (design == "independent_np") {
                    n_mult <- if (n_ratio > 0) n_ratio else 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                    args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                    args_list$design <- "independent"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else if (design == "paired_np") {
                    n_mult <- 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- "paired"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else if (design == "one.sample_np") {
                    n_mult <- 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- "one.sample"
                    func_to_call <- pwrss::power.np.wilcoxon
                } else {
                    n_mult <- 1
                    args_list$n2 <- if (calc == "n") NULL else max(2, n_g1)
                    args_list$n.ratio <- 1
                    args_list$design <- design
                    func_to_call <- pwrss::power.t.student
                }
            }

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    if (is_cor) tmp_args$rho <- d_val else tmp_args$d <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                opt <- try(uniroot(get_pwr, interval = if (is_cor) c(1e-4, 0.99) else c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                
                if (is_cor) args_list$rho <- opt$root else args_list$d <- opt$root
                args_list$power <- NULL
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                
            } else if (calc == "n") {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        tmp_args <- args_list
                        if (is_cor) {
                            eff_n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                            tmp_args$n <- eff_n
                        } else {
                            tmp_args$n2 <- max(2, round(n_val * n_mult))
                        }
                        tmp_args$power <- NULL
                        tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    
                    if (is_cor) {
                        calc_n <- max(4, round(opt$root))
                        args_list$n <- if (is_spearman) max(4, round(calc_n * are_spearman)) else calc_n
                    } else {
                        args_list$n2 <- max(2, round(opt$root * n_mult))
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
                if (!is.null(res$df)) {
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
            
            args_list <- list(alpha = alpha, alternative = alt, verbose = FALSE)

            if (is_cor) {
                n_mult <- 1
                args_list$null.rho <- 0
                func_to_call <- pwrss::power.z.onecor
            } else if (design == "independent") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$design <- "independent"
                func_to_call <- pwrss::power.t.student
            } else if (design == "welch") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$var.ratio <- var_ratio
                func_to_call <- pwrss::power.t.welch
            } else if (design == "independent_np") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$design <- "independent"
                func_to_call <- pwrss::power.np.wilcoxon
            } else if (design %in% c("paired_np", "one.sample_np")) {
                n_mult <- 1
                args_list$n.ratio <- 1
                args_list$design <- if(design == "paired_np") "paired" else "one.sample"
                func_to_call <- pwrss::power.np.wilcoxon
            } else {
                n_mult <- 1
                args_list$n.ratio <- 1
                args_list$design <- design
                func_to_call <- pwrss::power.t.student
            }

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    if (is_cor) {
                        tmp_args$rho <- d_val
                        tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                    } else {
                        tmp_args$d <- d_val
                        tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    }
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                opt <- try(uniroot(get_pwr, interval = if(is_cor) c(1e-4, 0.99) else c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            if (calc == "n") {
                tmp_args <- args_list
                if (is_cor) tmp_args$rho <- arg_d else tmp_args$d <- arg_d
                tmp_args$power <- power_val
                if (is_cor) tmp_args$n <- NULL else tmp_args$n2 <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        if (is_cor) {
                            t_args$rho <- arg_d
                            t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            t_args$d <- arg_d
                            t_args$n2 <- max(2, round(n_val * n_mult))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else { target_n1 <- if(is_cor) (if(is_spearman) ceiling(res$n / are_spearman) else res$n) else res$n[1] }
            } else { target_n1 <- n_g1 }
            
            if (calc == "power") {
                tmp_args <- args_list
                if (is_cor) {
                    tmp_args$rho <- arg_d
                    tmp_args$n <- if (is_spearman) max(4, round(target_n1 * are_spearman)) else max(4, round(target_n1))
                } else {
                    tmp_args$d <- arg_d
                    tmp_args$n2 <- max(2, round(target_n1 * n_mult))
                }
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            n_seq <- seq(if(is_cor) 4 else 2, max(50, ceiling(target_n1 * 2.5)), length.out = 50)
            pwr_seq <- numeric(length(n_seq))
            for (i in seq_along(n_seq)) {
                t_args <- args_list
                if (is_cor) {
                    t_args$rho <- arg_d
                    t_args$n <- if (is_spearman) max(4, round(n_seq[i] * are_spearman)) else max(4, round(n_seq[i]))
                } else {
                    t_args$d <- arg_d
                    t_args$n2 <- max(2, round(n_seq[i] * n_mult))
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
            
            args_list <- list(alpha = alpha, alternative = alt, verbose = FALSE)

            if (is_cor) {
                n_mult <- 1
                args_list$null.rho <- 0
                func_to_call <- pwrss::power.z.onecor
            } else if (design == "independent") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$design <- "independent"
                func_to_call <- pwrss::power.t.student
            } else if (design == "welch") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$var.ratio <- var_ratio
                func_to_call <- pwrss::power.t.welch
            } else if (design == "independent_np") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$design <- "independent"
                func_to_call <- pwrss::power.np.wilcoxon
            } else if (design %in% c("paired_np", "one.sample_np")) {
                n_mult <- 1
                args_list$n.ratio <- 1
                args_list$design <- if(design == "paired_np") "paired" else "one.sample"
                func_to_call <- pwrss::power.np.wilcoxon
            } else {
                n_mult <- 1
                args_list$n.ratio <- 1
                args_list$design <- design
                func_to_call <- pwrss::power.t.student
            }

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    if (is_cor) {
                        tmp_args$rho <- d_val
                        tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                    } else {
                        tmp_args$d <- d_val
                        tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    }
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                opt <- try(uniroot(get_pwr, interval = if (is_cor) c(1e-4, 0.99) else c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            if (calc == "n") {
                tmp_args <- args_list
                if (is_cor) tmp_args$rho <- arg_d else tmp_args$d <- arg_d
                tmp_args$power <- power_val
                if (is_cor) tmp_args$n <- NULL else tmp_args$n2 <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        if (is_cor) {
                            t_args$rho <- arg_d
                            t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            t_args$d <- arg_d
                            t_args$n2 <- max(2, round(n_val * n_mult))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else { target_n1 <- if(is_cor) (if(is_spearman) ceiling(res$n / are_spearman) else res$n) else res$n[1] }
            } else { target_n1 <- n_g1 }
            
            if (calc == "power") {
                tmp_args <- args_list
                if (is_cor) {
                    tmp_args$rho <- arg_d
                    tmp_args$n <- if (is_spearman) max(4, round(target_n1 * are_spearman)) else max(4, round(target_n1))
                } else {
                    tmp_args$d <- arg_d
                    tmp_args$n2 <- max(2, round(target_n1 * n_mult))
                }
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            d_seq <- if(is_cor) seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50) else seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            pwr_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                t_args <- args_list
                if (is_cor) {
                    t_args$rho <- d_seq[i]
                    t_args$n <- if (is_spearman) max(4, round(target_n1 * are_spearman)) else max(4, round(target_n1))
                } else {
                    t_args$d <- d_seq[i]
                    t_args$n2 <- max(2, round(target_n1 * n_mult))
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
            
            args_list <- list(alpha = alpha, alternative = alt, verbose = FALSE)

            if (is_cor) {
                n_mult <- 1
                args_list$null.rho <- 0
                func_to_call <- pwrss::power.z.onecor
            } else if (design == "independent") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$design <- "independent"
                func_to_call <- pwrss::power.t.student
            } else if (design == "welch") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$var.ratio <- var_ratio
                func_to_call <- pwrss::power.t.welch
            } else if (design == "independent_np") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                args_list$n.ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$design <- "independent"
                func_to_call <- pwrss::power.np.wilcoxon
            } else if (design %in% c("paired_np", "one.sample_np")) {
                n_mult <- 1
                args_list$n.ratio <- 1
                args_list$design <- if(design == "paired_np") "paired" else "one.sample"
                func_to_call <- pwrss::power.np.wilcoxon
            } else {
                n_mult <- 1
                args_list$n.ratio <- 1
                args_list$design <- design
                func_to_call <- pwrss::power.t.student
            }

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    if (is_cor) {
                        tmp_args$rho <- d_val
                        tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                    } else {
                        tmp_args$d <- d_val
                        tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    }
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                opt <- try(uniroot(get_pwr, interval = if(is_cor) c(1e-4, 0.99) else c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            if (calc == "power") {
                tmp_args <- args_list
                if (is_cor) {
                    tmp_args$rho <- arg_d
                    tmp_args$n <- if (is_spearman) max(4, round(n_g1 * are_spearman)) else max(4, round(n_g1))
                } else {
                    tmp_args$d <- arg_d
                    tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                }
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            if (calc == "n") {
                tmp_args <- args_list
                if (is_cor) tmp_args$rho <- arg_d else tmp_args$d <- arg_d
                tmp_args$power <- target_pwr
                if (is_cor) tmp_args$n <- NULL else tmp_args$n2 <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        if (is_cor) {
                            t_args$rho <- arg_d
                            t_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            t_args$d <- arg_d
                            t_args$n2 <- max(2, round(n_val * n_mult))
                        }
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - target_pwr)
                    }
                    opt <- try(uniroot(get_n, interval = c(4, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else { target_n1 <- if (is_cor) (if (is_spearman) ceiling(res$n / are_spearman) else res$n) else res$n[1] }
            } else { target_n1 <- n_g1 }

            d_seq <- if (is_cor) seq(max(0.01, arg_d * 0.2), min(0.99, arg_d * 2.5), length.out = 50) else seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            n_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                t_args <- args_list
                if (is_cor) {
                    t_args$rho <- d_seq[i]
                    t_args$power <- target_pwr
                    t_args$n <- NULL
                } else {
                    t_args$d <- d_seq[i]
                    t_args$power <- target_pwr
                    t_args$n2 <- NULL
                }
                tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                
                if (!inherits(tmp, "try-error")) {
                    n_seq[i] <- if (is_cor) (if (is_spearman) ceiling(tmp$n / are_spearman) else tmp$n) else tmp$n[1]
                } else {
                    get_n_loop <- function(n_val) {
                        tt_args <- args_list
                        if (is_cor) {
                            tt_args$rho <- d_seq[i]
                            tt_args$n <- if (is_spearman) max(4, round(n_val * are_spearman)) else max(4, round(n_val))
                        } else {
                            tt_args$d <- d_seq[i]
                            tt_args$n2 <- max(2, round(n_val * n_mult))
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
        }
    )
)
