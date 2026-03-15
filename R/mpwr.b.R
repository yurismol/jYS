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
            alpha     <- self$options$alpha
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            arg_d     <- if (calc == "es") NULL else es
            arg_power <- if (calc == "power") NULL else power
            
            args_list <- list(
                d           = arg_d,
                power       = arg_power,
                alpha       = alpha,
                alternative = alt,
                verbose     = FALSE
            )

            if (design == "independent") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                pwrss_n_ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                args_list$n.ratio <- pwrss_n_ratio
                args_list$design <- "independent"
                func_to_call <- pwrss::power.t.student
            } else if (design == "welch") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                pwrss_n_ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                args_list$n.ratio <- pwrss_n_ratio
                args_list$var.ratio <- var_ratio
                func_to_call <- pwrss::power.t.welch
            } else if (design == "independent_np") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                pwrss_n_ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                args_list$n.ratio <- pwrss_n_ratio
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

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args$d <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                opt <- try(uniroot(get_pwr, interval = c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) {
                    res <- opt
                } else {
                    args_list$d <- opt$root
                    args_list$power <- NULL
                    res <- try(do.call(func_to_call, args_list), silent = TRUE)
                }
            } else if (calc == "n") {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        tmp_args <- args_list
                        tmp_args$n2 <- max(2, round(n_val * n_mult))
                        tmp_args$power <- NULL
                        tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99) 
                        return(tmp$power - power)
                    }
                    opt <- try(uniroot(get_n, interval = c(2, 5000), extendInt = "yes"), silent = TRUE)
                    
                    if (inherits(opt, "try-error")) {
                        res <- opt
                    } else {
                        args_list$n2 <- max(2, round(opt$root * n_mult))
                        args_list$power <- NULL
                        res <- try(do.call(func_to_call, args_list), silent = TRUE)
                    }
                }
            } else {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
            }

            if (inherits(res, "try-error")) {
                self$results$text$setVisible(TRUE)
                self$results$text$setContent(paste0(.("Calculation error:\n"), res))
            } else {
                table <- self$results$powerTable
                
                res_n1 <- res$n[1] 
                
                if (design %in% c("independent", "welch", "independent_np")) {
                    res_n2 <- if (length(res$n) > 1) res$n[2] else res_n1
                } else if (design %in% c("paired", "paired_np")) {
                    res_n2 <- if (length(res$n) > 1) res$n[2] else res_n1
                } else {
                    res_n2 <- NA 
                }
                
                res_es <- if (!is.null(res$parms$d)) res$parms$d else es
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
                    n1    = res_n1,
                    n2    = res_n2,
                    es    = res_es,
                    power = res_pw,
                    alpha = res_a,
                    alt   = res_al
                ))
                
                calc_map <- list(
                    n = .("Sample Size (N)"), 
                    power = .("Power"), 
                    es = .("Effect Size (d)")
                )
                
                design_text <- switch(design,
                    independent = .("Independent Samples (Student)"),
                    welch = .("Independent Samples (Welch)"),
                    independent_np = .("Independent Samples (Wilcoxon)"),
                    paired = .("Paired Samples (Student)"),
                    paired_np = .("Paired Samples (Wilcoxon)"),
                    one.sample = .("One Sample (Student)"),
                    one.sample_np = .("One Sample (Wilcoxon)")
                )
                
                table$setNote("calc_note", paste0(.("Calculated parameter: "), calc_map[[calc]], " | ", .("Design: "), design_text))
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
            alpha     <- self$options$alpha
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            arg_d     <- if (calc == "es") NULL else es
            arg_power <- if (calc == "power") NULL else power
            
            args_list <- list(
                d           = arg_d,
                power       = arg_power,
                alpha       = alpha,
                alternative = alt,
                verbose     = FALSE
            )

            if (design == "independent") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                pwrss_n_ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                args_list$n.ratio <- pwrss_n_ratio
                args_list$design <- "independent"
                func_to_call <- pwrss::power.t.student
            } else if (design == "welch") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                pwrss_n_ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                args_list$n.ratio <- pwrss_n_ratio
                args_list$var.ratio <- var_ratio
                func_to_call <- pwrss::power.t.welch
            } else if (design == "independent_np") {
                n_mult <- if (n_ratio > 0) n_ratio else 1
                pwrss_n_ratio <- if (n_ratio > 0) 1 / n_ratio else 1
                args_list$n2 <- if (calc == "n") NULL else max(2, round(n_g1 * n_mult))
                args_list$n.ratio <- pwrss_n_ratio
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

            if (calc == "es") {
                get_pwr <- function(d_val) {
                    tmp_args <- args_list
                    tmp_args$d <- d_val
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power)
                }
                opt <- try(uniroot(get_pwr, interval = c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                
                args_list$d <- opt$root
                args_list$power <- NULL
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                
            } else if (calc == "n") {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        tmp_args <- args_list
                        tmp_args$n2 <- max(2, round(n_val * n_mult))
                        tmp_args$power <- NULL
                        tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power)
                    }
                    opt <- try(uniroot(get_n, interval = c(2, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    
                    args_list$n2 <- max(2, round(opt$root * n_mult))
                    args_list$power <- NULL
                    res <- try(do.call(func_to_call, args_list), silent = TRUE)
                }
            } else {
                res <- try(do.call(func_to_call, args_list), silent = TRUE)
            }

            if (!inherits(res, "try-error") && !is.null(res$df)) {
                pwrss::power.t.test(
                    ncp         = res$ncp,
                    null.ncp    = if (!is.null(res$null.ncp)) res$null.ncp else 0,
                    df          = res$df,
                    alpha       = res$parms$alpha,
                    alternative = res$parms$alternative,
                    plot        = TRUE,
                    verbose     = FALSE
                )
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
            alpha     <- self$options$alpha
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            args_list <- list(alpha = alpha, alternative = alt, verbose = FALSE)

            if (design == "independent") {
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
                    tmp_args$d <- d_val
                    tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                opt <- try(uniroot(get_pwr, interval = c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            if (calc == "n") {
                tmp_args <- args_list
                tmp_args$d <- arg_d
                tmp_args$power <- power_val
                tmp_args$n2 <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        t_args$d <- arg_d
                        t_args$n2 <- max(2, round(n_val * n_mult))
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    opt <- try(uniroot(get_n, interval = c(2, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else { target_n1 <- res$n[1] }
            } else { target_n1 <- n_g1 }
            
            if (calc == "power") {
                tmp_args <- args_list
                tmp_args$d <- arg_d
                tmp_args$n2 <- max(2, round(target_n1 * n_mult))
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            n_seq <- seq(2, max(50, ceiling(target_n1 * 2.5)), length.out = 50)
            pwr_seq <- numeric(length(n_seq))
            for (i in seq_along(n_seq)) {
                t_args <- args_list
                t_args$d <- arg_d
                t_args$power <- NULL
                t_args$n2 <- max(2, round(n_seq[i] * n_mult))
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
            alpha     <- self$options$alpha
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            args_list <- list(alpha = alpha, alternative = alt, verbose = FALSE)

            if (design == "independent") {
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
                    tmp_args$d <- d_val
                    tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                opt <- try(uniroot(get_pwr, interval = c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            if (calc == "n") {
                tmp_args <- args_list
                tmp_args$d <- arg_d
                tmp_args$power <- power_val
                tmp_args$n2 <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        t_args$d <- arg_d
                        t_args$n2 <- max(2, round(n_val * n_mult))
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - power_val)
                    }
                    opt <- try(uniroot(get_n, interval = c(2, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else { target_n1 <- res$n[1] }
            } else { target_n1 <- n_g1 }
            
            if (calc == "power") {
                tmp_args <- args_list
                tmp_args$d <- arg_d
                tmp_args$n2 <- max(2, round(target_n1 * n_mult))
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            pwr_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                t_args <- args_list
                t_args$d <- d_seq[i]
                t_args$power <- NULL
                t_args$n2 <- max(2, round(target_n1 * n_mult))
                tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                pwr_seq[i] <- if (!inherits(tmp, "try-error")) tmp$power else NA
            }
            
            df_plot <- na.omit(data.frame(ES = d_seq, Power = pwr_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = Power)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_pwr, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = arg_d, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("Power by Effect Size"), x = .("Effect Size (d)"), y = .("Power")) +
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
            alpha     <- self$options$alpha
            n_ratio   <- self$options$n_ratio
            var_ratio <- self$options$var_ratio

            args_list <- list(alpha = alpha, alternative = alt, verbose = FALSE)

            if (design == "independent") {
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
                    tmp_args$d <- d_val
                    tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                    tmp_args$power <- NULL
                    tmp <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                    if (inherits(tmp, "try-error")) return(-0.99)
                    return(tmp$power - power_val)
                }
                opt <- try(uniroot(get_pwr, interval = c(1e-4, 5), extendInt = "yes"), silent = TRUE)
                if (inherits(opt, "try-error")) return(FALSE)
                arg_d <- opt$root
            } else { arg_d <- es }
            
            if (calc == "power") {
                tmp_args <- args_list
                tmp_args$d <- arg_d
                tmp_args$n2 <- max(2, round(n_g1 * n_mult))
                tmp_args$power <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                target_pwr <- if (!inherits(res, "try-error")) res$power else NA
            } else { target_pwr <- power_val }
            
            if (calc == "n") {
                tmp_args <- args_list
                tmp_args$d <- arg_d
                tmp_args$power <- target_pwr
                tmp_args$n2 <- NULL
                res <- try(do.call(func_to_call, tmp_args), silent = TRUE)
                if (inherits(res, "try-error")) {
                    get_n <- function(n_val) {
                        t_args <- args_list
                        t_args$d <- arg_d
                        t_args$n2 <- max(2, round(n_val * n_mult))
                        t_args$power <- NULL
                        tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                        if (inherits(tmp, "try-error")) return(-0.99)
                        return(tmp$power - target_pwr)
                    }
                    opt <- try(uniroot(get_n, interval = c(2, 5000), extendInt = "yes"), silent = TRUE)
                    if (inherits(opt, "try-error")) return(FALSE)
                    target_n1 <- opt$root
                } else { target_n1 <- res$n[1] }
            } else { target_n1 <- n_g1 }

            d_seq <- seq(max(0.01, arg_d * 0.2), arg_d * 2.5, length.out = 50)
            n_seq <- numeric(length(d_seq))
            
            for (i in seq_along(d_seq)) {
                t_args <- args_list
                t_args$d <- d_seq[i]
                t_args$power <- target_pwr
                t_args$n2 <- NULL
                tmp <- try(do.call(func_to_call, t_args), silent = TRUE)
                
                if (!inherits(tmp, "try-error")) {
                    n_seq[i] <- tmp$n[1]
                } else {
                    get_n_loop <- function(n_val) {
                        tt_args <- args_list
                        tt_args$d <- d_seq[i]
                        tt_args$n2 <- max(2, round(n_val * n_mult))
                        tt_args$power <- NULL
                        tmp2 <- try(do.call(func_to_call, tt_args), silent = TRUE)
                        if (inherits(tmp2, "try-error")) return(-0.99)
                        return(tmp2$power - target_pwr)
                    }
                    opt <- try(uniroot(get_n_loop, interval = c(2, 5000), extendInt = "yes"), silent = TRUE)
                    n_seq[i] <- if (!inherits(opt, "try-error")) opt$root else NA
                }
            }
            
            df_plot <- na.omit(data.frame(ES = d_seq, N = n_seq))
            if (nrow(df_plot) == 0) return(FALSE)
            
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = ES, y = N)) +
                ggplot2::geom_line(color = "#3366B2", linewidth = 1.2) +
                ggplot2::geom_point(color = "#3366B2", size = 2) +
                ggplot2::geom_hline(yintercept = target_n1, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::geom_vline(xintercept = arg_d, linetype = "dashed", color = "#E54028", linewidth = 0.8) +
                ggplot2::labs(title = .("N by Effect Size"), x = .("Effect Size (d)"), y = .("Sample Size (N1)")) +
                ggtheme
            
            print(p)
            return(TRUE)
        }
    )
)
