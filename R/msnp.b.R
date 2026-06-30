
mSNPClass <- R6::R6Class(
    "mSNPClass",
    inherit = mSNPBase,
    private = list(
        # Internal state
        g_data = NULL,
        g_alleles = list(), # list of list(major, minor) per marker
        g_counts = list(),  # list of c(AA, AB, BB) per marker and population
        g_classified = list(), # list of classified genotypes per marker
        g_hwe_results = list(),
        g_assoc_results = list(),
        g_pop_results = list(),
        g_mdr_results = list(),
        g_ld_results = list(),
        
        .init = function() {
            # Footnotes and translation definitions
            freqTable <- self$results$hweGroup$freqTable
            hwTable <- self$results$hweGroup$hwTable
            assocTable <- self$results$assocGroup$assocTable
            popTable <- self$results$popGroup$popTable
            mdrBestTable <- self$results$mdrGroup$mdrBestTable
            mdrCellTable <- self$results$mdrGroup$mdrCellTable
            ldTable <- self$results$ldGroup$ldTable
            
            # Setup notes
            freqTable$setNote('n1', paste0("<b>", .("Allele frequencies (p, q)"), "</b> ", .("represent the proportions of the two alleles in the sample. For a biallelic marker with alleles A and B: p = freq(A) = (2*n_AA + n_AB) / (2*N), q = 1 - p.")))
            freqTable$setNote('n2', paste0("<b>", .("Observed genotype frequencies"), "</b> ", .("are the raw counts and proportions of each genotype (AA, AB, BB) directly from the data.")))
            freqTable$setNote('n3', paste0("<b>", .("Expected genotype frequencies"), "</b> ", .("are the counts predicted under Hardy–Weinberg equilibrium: E(AA) = p² × N, E(AB) = 2pq × N, E(BB) = q² × N.")))
            freqTable$setNote('n4', paste0("<b>", .("Observed heterozygosity (H_obs)"), "</b> ", .("is the proportion of heterozygous individuals (AB) in the sample: H_obs = n_AB / N.")))
            freqTable$setNote('n5', paste0("<b>", .("Expected heterozygosity (H_exp)"), "</b> ", .("is the heterozygosity expected under HWE: H_exp = 2pq. A significant difference between H_obs and H_exp may indicate inbreeding, population substructure, or selection.")))
            
            hwTable$setNote('n1', paste0("<b>", .("Chi-square test"), "</b> ", .("is the classical Pearson goodness-of-fit test comparing observed and expected genotype counts under HWE. Suitable for large sample sizes (N > 50). Asymptotic; may be unreliable for rare alleles.")))
            hwTable$setNote('n2', paste0("<b>", .("Exact test"), "</b> ", .("(Haldane, 1954) computes the exact probability of the observed heterozygote count, conditional on the allele counts. Recommended for small samples or rare alleles.")))
            hwTable$setNote('n3', paste0("<b>", .("Likelihood-ratio test"), "</b> ", .("compares the likelihood of the data under HWE versus the saturated model. The test statistic -2 × ln(L_HWE / L_sat) follows chi-square with 1 df.")))
            hwTable$setNote('n4', paste0("<b>", .("Permutation test"), "</b> ", .("estimates the p-value by randomly permuting alleles among genotypes. Distribution-free; valid for any sample size.")))
            hwTable$setNote('n5', paste0("<b>", .("Inbreeding coefficient (F_IS)"), "</b> ", .("measures deviation from HWE: F_IS = 1 - H_obs / H_exp. "), "<b>", .("F_IS > 0"), "</b> ", .("indicates heterozygote deficiency (inbreeding), "), "<b>", .("F_IS < 0"), "</b> ", .("indicates heterozygote excess (outbreeding), "), "<b>", .("F_IS = 0"), "</b> ", .("indicates Hardy–Weinberg equilibrium.")))
            
            assocTable$setNote('n1', paste0("<b>", .("Allelic test"), "</b> ", .("compares allele frequencies between case and control groups using a 2×2 contingency table. Tests whether the minor allele is significantly more (or less) frequent in cases than controls.")))
            assocTable$setNote('n2', paste0("<b>", .("Odds Ratio (OR)"), "</b> ", .("quantifies the strength of association between an allele/genotype and the outcome. "), "<b>", .("OR > 1"), "</b> ", .("indicates increased risk, "), "<b>", .("OR < 1"), "</b> ", .("indicates protective effect, "), "<b>", .("OR = 1"), "</b> ", .("indicates no association. The 95% CI that does not cross 1.0 indicates significance.")))
            assocTable$setNote('n3', paste0("<b>", .("Genetic models"), "</b> ", .("test the association under different inheritance patterns: "), "<b>", .("Codominant"), "</b>", " (AA vs AB vs BB), ", "<b>", .("Dominant"), "</b>", " (AA vs AB+BB), ", "<b>", .("Recessive"), "</b>", " (AA+AB vs BB), ", "<b>", .("Overdominant"), "</b>", " (AA+BB vs AB), ", "<b>", .("Log-additive"), "</b>", " (trend test for allele dose 0/1/2)."))
            assocTable$setNote('n4', paste0("<b>", .("p-adjusted"), "</b> ", .("is the p-value corrected for multiple testing across markers using the selected method (Bonferroni, Holm, or Benjamini–Hochberg FDR).")))
            assocTable$setNote('n5', paste0("<b>", .("Best model selection"), "</b>", .(": the genetic model with the lowest p-value (or AIC/BIC) is highlighted. This helps identify the most likely mode of inheritance for the observed association.")))
            
            popTable$setNote('n1', paste0("<b>", .("MAF (Minor Allele Frequency)"), "</b> ", .("is the frequency of the less common allele in the population. Markers with "), "<b>", .("MAF < 0.01"), "</b> ", .("are typically considered monomorphic and may be excluded from analysis. "), "<b>", .("MAF < 0.05"), "</b> ", .("indicates a rare variant.")))
            popTable$setNote('n2', paste0("<b>", .("PIC (Polymorphism Information Content)"), "</b> ", .("measures the informativeness of a genetic marker. "), "<b>", .("PIC > 0.5"), "</b> ", .("is highly informative, "), "<b>", .("0.25 < PIC < 0.5"), "</b> ", .("is moderately informative, "), "<b>", .("PIC < 0.25"), "</b> ", .("is slightly informative (Botstein et al., 1980).")))
            popTable$setNote('n3', paste0("<b>", .("Shannon Information Index (I)"), "</b> ", .("quantifies allelic diversity. Higher values indicate greater diversity. For a biallelic marker, maximum I = ln(2) ≈ 0.693 when allele frequencies are equal.")))
            popTable$setNote('n4', paste0("<b>", .("Effective number of alleles (Ne)"), "</b> ", .("is the reciprocal of homozygosity: Ne = 1 / Σ(pi²). Ne = 1 indicates fixation (monomorphic), Ne = 2 indicates equal allele frequencies for a biallelic marker.")))
            
            mdrBestTable$setNote('n1', paste0("<b>", .("MDR (Multifactor Dimensionality Reduction)"), "</b> ", .("(Ritchie et al., 2001) is a non-parametric method for detecting gene-gene interactions (epistasis). It reduces high-dimensional genotype combinations into a binary variable (high-risk vs. low-risk).")))
            mdrBestTable$setNote('n2', paste0("<b>", .("Balanced Accuracy (BA)"), "</b> ", .("= (Sensitivity + Specificity) / 2. Accounts for class imbalance. BA = 0.5 indicates random classification.")))
            mdrBestTable$setNote('n3', paste0("<b>", .("Cross-Validation Consistency (CVC)"), "</b> ", .("is the number of folds (out of K) in which the model was selected as best. Higher CVC indicates greater stability of the identified interaction.")))
            mdrBestTable$setNote('n4', paste0("<b>", .("Training BA"), "</b> ", .("may be optimistically biased. "), "<b>", .("Testing BA"), "</b> ", .("is computed on the held-out fold and provides an unbiased estimate of predictive performance.")))
            mdrBestTable$setNote('n5', paste0("<b>", .("Permutation p-value"), "</b> ", .("is obtained by shuffling outcome labels. It represents the proportion of permuted datasets that achieved a testing BA ≥ the observed value.")))
            
            mdrCellTable$setNote('n1', paste0("<b>", .("High-Risk (HR)"), "</b> ", .("cells have cases/controls ratio ≥ threshold T. "), "<b>", .("Low-Risk (LR)"), "</b> ", .("cells have ratio < T.")))
            mdrCellTable$setNote('n2', paste0("<b>", .("Ratio"), "</b> ", .("= Cases / Controls. Empty cells (no observations) are left unclassified.")))
            mdrCellTable$setNote('n3', paste0("<b>", .("Classification"), "</b> ", .("defines the epistatic model: subjects in HR cells are predicted as cases, LR cells as controls.")))
            
            ldTable$setNote('n1', paste0("<b>", .("D' (normalized LD)"), "</b> ", .("measures the strength of linkage disequilibrium between two loci, scaled to [-1, 1]. "), "<b>", .("|D'| = 1"), "</b> ", .("indicates complete LD (no recombination observed), "), "<b>", .("D' = 0"), "</b> ", .("indicates linkage equilibrium (independent assortment).")))
            ldTable$setNote('n2', paste0("<b>", .("r² (squared correlation)"), "</b> ", .("measures both LD strength and allele frequency similarity. "), "<b>", .("r² = 1"), "</b> ", .("indicates perfect LD (complete mutual predictability), "), "<b>", .("r² > 0.8"), "</b> ", .("is commonly used as the threshold for strong LD in GWAS tag-SNP selection. r² is preferred over D' for association study design because it directly relates to statistical power.")))
            ldTable$setNote('n3', paste0("<b>", .("Haplotype frequencies"), "</b> ", .("are estimated using the Expectation-Maximization (EM) algorithm from unphased genotype data. The p-value tests the null hypothesis D = 0 (linkage equilibrium).")))
        },
        
        .run = function() {
            if (length(self$options$vars) == 0) return()
            
            # Prepare data
            private$g_data <- self$data
            private$.parseGenotypes()
            
            # 1. HWE Analysis
            if (self$options$freqTable || self$options$hwTests) {
                private$.fillFreqAndHWETables()
            }
            
            # 2. Association Tests
            if (self$options$assocEnable && !is.null(self$options$outcome)) {
                private$.fillAssociationTable()
            }
            
            # 3. Population Genetics
            if (self$options$popMetrics) {
                private$.fillPopulationTable()
            }
            
            # 4. MDR Analysis
            if (self$options$mdrEnable && !is.null(self$options$outcome)) {
                private$.runMDRAnalysis()
            }
            
            # 5. LD Analysis
            if (self$options$ldEnable) {
                private$.fillLDTable()
            }
        },
        
        .parseGenotypes = function() {
            for (marker in self$options$vars) {
                x <- private$g_data[[marker]]
                non_na <- na.omit(x)
                non_na <- non_na[non_na != ""]
                
                if (length(non_na) == 0) {
                    private$g_alleles[[marker]] <- list(major="A", minor="B")
                    private$g_classified[[marker]] <- rep(NA, length(x))
                    next
                }
                
                # Check for numeric 0, 1, 2 coding
                is_numeric_coding <- all(as.character(non_na) %in% c("0", "1", "2"))
                if (is_numeric_coding) {
                    classified <- rep(NA, length(x))
                    classified[x == "0" | x == 0] <- "AA"
                    classified[x == "1" | x == 1] <- "AB"
                    classified[x == "2" | x == 2] <- "BB"
                    private$g_alleles[[marker]] <- list(major="A", minor="B")
                    private$g_classified[[marker]] <- classified
                    next
                }
                
                # Standard text character processing
                # Extract all characters (alleles)
                cleaned <- gsub("[^A-Za-z0-9]", "", as.character(non_na))
                all_chars <- unlist(strsplit(cleaned, ""))
                if (length(all_chars) == 0) {
                    private$g_alleles[[marker]] <- list(major="A", minor="B")
                    private$g_classified[[marker]] <- rep(NA, length(x))
                    next
                }
                
                # Sorted allele frequency to define major / minor
                allele_counts <- table(all_chars)
                alleles_sorted <- names(sort(allele_counts, decreasing = TRUE))
                major <- alleles_sorted[1]
                minor <- if (length(alleles_sorted) > 1) alleles_sorted[2] else major
                
                private$g_alleles[[marker]] <- list(major=major, minor=minor)
                
                classified <- rep(NA, length(x))
                for (i in seq_along(x)) {
                    val <- x[i]
                    if (is.na(val) || val == "") next
                    val_clean <- gsub("[^A-Za-z0-9]", "", as.character(val))
                    chars <- unlist(strsplit(val_clean, ""))
                    if (length(chars) == 0) next
                    
                    count_major <- sum(chars == major)
                    count_minor <- sum(chars == minor)
                    
                    if (count_major == 2) {
                        classified[i] <- "AA"
                    } else if (count_major == 1 && count_minor == 1) {
                        classified[i] <- "AB"
                    } else if (count_minor == 2) {
                        classified[i] <- "BB"
                    }
                }
                private$g_classified[[marker]] <- classified
            }
        },
        
        .fillFreqAndHWETables = function() {
            freqTable <- self$results$hweGroup$freqTable
            hwTable <- self$results$hweGroup$hwTable
            private$g_hwe_results <- list()
            
            has_group <- !is.null(self$options$group)
            groups <- if (has_group) levels(private$g_data[[self$options$group]]) else "Total"
            
            # Temporary storage to compute multiple correction
            raw_p_chi <- numeric()
            raw_p_exact <- numeric()
            raw_p_lr <- numeric()
            raw_p_perm <- numeric()
            keys <- list()
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                for (g in groups) {
                    subset_idx <- if (has_group) which(private$g_data[[self$options$group]] == g) else seq_along(classified)
                    
                    sub_class <- classified[subset_idx]
                    sub_class <- na.omit(sub_class)
                    N <- length(sub_class)
                    
                    obsAA <- sum(sub_class == "AA")
                    obsAB <- sum(sub_class == "AB")
                    obsBB <- sum(sub_class == "BB")
                    
                    # Allele counts
                    nA <- 2 * obsAA + obsAB
                    nB <- 2 * obsBB + obsAB
                    total_alleles <- 2 * N
                    
                    p_freq <- if (total_alleles > 0) nA / total_alleles else 0
                    q_freq <- 1 - p_freq
                    
                    # Expected counts under HWE
                    expAA <- (p_freq^2) * N
                    expAB <- 2 * p_freq * q_freq * N
                    expBB <- (q_freq^2) * N
                    
                    h_obs <- if (N > 0) obsAB / N else 0
                    h_exp <- 2 * p_freq * q_freq
                    
                    # Fill freqTable row
                    row_key <- paste(marker, g, sep="_")
                    row_val <- list(
                        marker = marker,
                        group = g,
                        p_freq = p_freq,
                        q_freq = q_freq,
                        obsAA = obsAA,
                        obsAB = obsAB,
                        obsBB = obsBB,
                        expAA = expAA,
                        expAB = expAB,
                        expBB = expBB,
                        h_obs = h_obs,
                        h_exp = h_exp
                    )
                    
                    # Write immediately
                    freqTable$addRow(rowKey = row_key, value = row_val)
                    private$g_hwe_results[[row_key]] <- row_val
                    
                    # Compute HWE Tests if selected
                    if (self$options$hwTests) {
                        counts_vec <- c(AA = obsAA, AB = obsAB, BB = obsBB)
                        
                        # Inbreeding coeff
                        fis <- if (h_exp > 0) 1 - (h_obs / h_exp) else 0
                        
                        # Chi-Square HWE Test
                        chi_stat <- NA
                        chi_p <- NA
                        if (N > 0) {
                            res <- tryCatch(HardyWeinberg::HWChisq(counts_vec, cc=0, verbose=FALSE), error = function(e) list(chisq=NA, pval=NA))
                            chi_stat <- res$chisq
                            chi_p <- res$pval
                        }
                        
                        # Exact HWE Test
                        exact_p <- NA
                        if (N > 0) {
                            exact_p <- tryCatch(HardyWeinberg::HWExact(counts_vec, verbose=FALSE)$pval, error = function(e) NA)
                        }
                        
                        # Likelihood-ratio HWE Test
                        lr_stat <- NA
                        lr_p <- NA
                        if (N > 0) {
                            res_lr <- tryCatch(HardyWeinberg::HWLratio(counts_vec, verbose=FALSE), error = function(e) list(G2=NA, pval=NA))
                            lr_stat <- res_lr$G2
                            lr_p <- res_lr$pval
                        }
                        
                        # Permutation HWE Test
                        perm_p <- NA
                        if (self$options$hwPerm && N > 0) {
                            perm_p <- tryCatch(HardyWeinberg::HWPerm(counts_vec, nperm=as.integer(self$options$nPerm), verbose=FALSE)$pval, error = function(e) NA)
                        }
                        
                        # Keep raw values for adjust
                        raw_p_chi <- c(raw_p_chi, chi_p)
                        raw_p_exact <- c(raw_p_exact, exact_p)
                        raw_p_lr <- c(raw_p_lr, lr_p)
                        if (self$options$hwPerm) raw_p_perm <- c(raw_p_perm, perm_p)
                        
                        keys[[length(keys) + 1]] <- list(row_key = row_key, marker = marker, group = g, chi_stat = chi_stat, lr_stat = lr_stat, fis = fis)
                    }
                }
            }
            
            # Apply adjustment
            if (self$options$hwTests && length(keys) > 0) {
                adj_method <- switch(self$options$adjust,
                                     "bonferroni" = "bonferroni",
                                     "holm" = "holm",
                                     "fdr" = "BH",
                                     "none" = "none")
                
                adj_p_chi <- if (adj_method != "none") stats::p.adjust(raw_p_chi, method = adj_method) else raw_p_chi
                adj_p_exact <- if (adj_method != "none") stats::p.adjust(raw_p_exact, method = adj_method) else raw_p_exact
                adj_p_lr <- if (adj_method != "none") stats::p.adjust(raw_p_lr, method = adj_method) else raw_p_lr
                adj_p_perm <- if (self$options$hwPerm && adj_method != "none") stats::p.adjust(raw_p_perm, method = adj_method) else raw_p_perm
                
                alpha_val <- as.numeric(self$options$alpha)
                
                for (i in seq_along(keys)) {
                    k <- keys[[i]]
                    
                    row_val <- list(
                        marker = k$marker,
                        group = k$group,
                        chi_stat = k$chi_stat,
                        chi_p = raw_p_chi[i],
                        chi_p_adj = adj_p_chi[i],
                        exact_p = raw_p_exact[i],
                        exact_p_adj = adj_p_exact[i],
                        lr_stat = k$lr_stat,
                        lr_p = raw_p_lr[i],
                        lr_p_adj = adj_p_lr[i],
                        fis = k$fis
                    )
                    
                    if (self$options$hwPerm) {
                        row_val$perm_p <- raw_p_perm[i]
                        row_val$perm_p_adj <- adj_p_perm[i]
                    }
                    
                    hwTable$addRow(rowKey = k$row_key, value = row_val)
                    
                    existing <- private$g_hwe_results[[k$row_key]]
                    if (!is.null(existing)) {
                        private$g_hwe_results[[k$row_key]] <- utils::modifyList(existing, row_val)
                    } else {
                        private$g_hwe_results[[k$row_key]] <- row_val
                    }
                    
                    # Highlight exact test p-value if significant
                    is_sig <- (!is.na(raw_p_exact[i]) && raw_p_exact[i] < alpha_val)
                    if (is_sig) {
                        hwTable$addFootnote(rowKey = k$row_key, col = "exact_p", .("Significant HWE deviation"))
                    }
                }
            }
        },
        
        .fillAssociationTable = function() {
            assocTable <- self$results$assocGroup$assocTable
            private$g_assoc_results <- list()
            
            outcome_col <- self$options$outcome
            outcome_var <- private$g_data[[outcome_col]]
            
            # Clean outcome to Case/Control binary
            levels_out <- levels(outcome_var)
            if (length(levels_out) < 2) {
                # We need case/control binary outcome
                return()
            }
            
            # Map levels to "Case" and "Control"
            # We assume first level is Control, second is Case, or try to identify
            # Let's map first level as Control and second level as Case
            case_level <- levels_out[2]
            control_level <- levels_out[1]
            
            # Recode outcome
            outcome_rec <- rep(NA, length(outcome_var))
            outcome_rec[outcome_var == case_level] <- "Case"
            outcome_rec[outcome_var == control_level] <- "Control"
            
            raw_p_vals <- numeric()
            rows_data <- list()
            
            # Select models to calculate
            model_list <- c("Allelic", "Genotypic", "Codominant (AB vs AA)", "Codominant (BB vs AA)", "Dominant", "Recessive", "Overdominant", "Log-additive")
            if (self$options$geneticModel != "all") {
                selected <- switch(self$options$geneticModel,
                                   "codominant" = c("Codominant (AB vs AA)", "Codominant (BB vs AA)"),
                                   "dominant" = "Dominant",
                                   "recessive" = "Recessive",
                                   "overdominant" = "Overdominant",
                                   "log-additive" = "Log-additive")
                model_list <- c("Allelic", "Genotypic", selected)
            }
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                
                # Get non-NA pairs
                complete_cases <- !is.na(classified) & !is.na(outcome_rec)
                sub_class <- classified[complete_cases]
                sub_out <- outcome_rec[complete_cases]
                
                if (length(sub_class) == 0) next
                
                # Prepare summary strings for counts
                cases_AA <- sum(sub_class == "AA" & sub_out == "Case")
                cases_AB <- sum(sub_class == "AB" & sub_out == "Case")
                cases_BB <- sum(sub_class == "BB" & sub_out == "Case")
                ctrls_AA <- sum(sub_class == "AA" & sub_out == "Control")
                ctrls_AB <- sum(sub_class == "AB" & sub_out == "Control")
                ctrls_BB <- sum(sub_class == "BB" & sub_out == "Control")
                
                counts_cases <- paste0("AA:", cases_AA, ", AB:", cases_AB, ", BB:", cases_BB)
                counts_ctrls <- paste0("AA:", ctrls_AA, ", AB:", ctrls_AB, ", BB:", ctrls_BB)
                
                for (model in model_list) {
                    or_val <- NA
                    or_lower <- NA
                    or_upper <- NA
                    p_val <- NA
                    
                    if (model == "Allelic") {
                        # Allelic test (2x2)
                        a <- 2 * cases_AA + cases_AB
                        b <- 2 * cases_BB + cases_AB
                        c <- 2 * ctrls_AA + ctrls_AB
                        d <- 2 * ctrls_BB + ctrls_AB
                        
                        tbl2x2 <- matrix(c(a, b, c, d), nrow=2, byrow=TRUE)
                        if (sum(tbl2x2) > 0) {
                            res_test <- tryCatch(stats::chisq.test(tbl2x2, correct=FALSE), error = function(e) list(p.value=NA))
                            p_val <- res_test$p.value
                            # Odds ratio for allele A vs B (Case vs Control)
                            # OR = (a * d) / (b * c)
                            if (b * c > 0) {
                                or_val <- (a * d) / (b * c)
                                log_or <- log(or_val)
                                se_log_or <- sqrt(1/max(1, a) + 1/max(1, b) + 1/max(1, c) + 1/max(1, d))
                                or_lower <- exp(log_or - 1.96 * se_log_or)
                                or_upper <- exp(log_or + 1.96 * se_log_or)
                            }
                        }
                    } else if (model == "Genotypic") {
                        # Genotypic test (3x2)
                        tbl3x2 <- matrix(c(cases_AA, ctrls_AA,
                                           cases_AB, ctrls_AB,
                                           cases_BB, ctrls_BB), nrow=3, byrow=TRUE)
                        if (sum(tbl3x2) > 0) {
                            res_test <- tryCatch(stats::chisq.test(tbl3x2, correct=FALSE), error = function(e) list(p.value=NA))
                            p_val <- res_test$p.value
                            # No single OR for 3x2
                        }
                    } else {
                        # Fit glm with specific coding
                        model_df <- data.frame(
                            y = as.factor(sub_out) # levels: Control, Case (Case is 1)
                        )
                        
                        if (model == "Codominant (AB vs AA)") {
                            # Codominant AB vs AA
                            model_df$x <- factor(sub_class, levels=c("AA", "AB", "BB"))
                            res_glm <- tryCatch(glm(y ~ x, data=model_df, family=binomial), error=function(e) NULL)
                            if (!is.null(res_glm)) {
                                sum_glm <- summary(res_glm)
                                if ("xAB" %in% rownames(sum_glm$coefficients)) {
                                    p_val <- sum_glm$coefficients["xAB", 4]
                                    or_val <- exp(sum_glm$coefficients["xAB", 1])
                                    ci <- tryCatch(suppressMessages(confint.default(res_glm, "xAB", level=0.95)), error=function(e) c(NA, NA))
                                    or_lower <- exp(ci[1])
                                    or_upper <- exp(ci[2])
                                }
                            }
                        } else if (model == "Codominant (BB vs AA)") {
                            # Codominant BB vs AA
                            model_df$x <- factor(sub_class, levels=c("AA", "AB", "BB"))
                            res_glm <- tryCatch(glm(y ~ x, data=model_df, family=binomial), error=function(e) NULL)
                            if (!is.null(res_glm)) {
                                sum_glm <- summary(res_glm)
                                if ("xBB" %in% rownames(sum_glm$coefficients)) {
                                    p_val <- sum_glm$coefficients["xBB", 4]
                                    or_val <- exp(sum_glm$coefficients["xBB", 1])
                                    ci <- tryCatch(suppressMessages(confint.default(res_glm, "xBB", level=0.95)), error=function(e) c(NA, NA))
                                    or_lower <- exp(ci[1])
                                    or_upper <- exp(ci[2])
                                }
                            }
                        } else if (model == "Dominant") {
                            # Dominant (AA vs AB+BB)
                            model_df$x <- ifelse(sub_class == "AA", 0, 1)
                            res_glm <- tryCatch(glm(y ~ x, data=model_df, family=binomial), error=function(e) NULL)
                            if (!is.null(res_glm)) {
                                sum_glm <- summary(res_glm)
                                if ("x" %in% rownames(sum_glm$coefficients)) {
                                    p_val <- sum_glm$coefficients["x", 4]
                                    or_val <- exp(sum_glm$coefficients["x", 1])
                                    ci <- tryCatch(suppressMessages(confint.default(res_glm, "x", level=0.95)), error=function(e) c(NA, NA))
                                    or_lower <- exp(ci[1])
                                    or_upper <- exp(ci[2])
                                }
                            }
                        } else if (model == "Recessive") {
                            # Recessive (AA+AB vs BB)
                            model_df$x <- ifelse(sub_class == "BB", 1, 0)
                            res_glm <- tryCatch(glm(y ~ x, data=model_df, family=binomial), error=function(e) NULL)
                            if (!is.null(res_glm)) {
                                sum_glm <- summary(res_glm)
                                if ("x" %in% rownames(sum_glm$coefficients)) {
                                    p_val <- sum_glm$coefficients["x", 4]
                                    or_val <- exp(sum_glm$coefficients["x", 1])
                                    ci <- tryCatch(suppressMessages(confint.default(res_glm, "x", level=0.95)), error=function(e) c(NA, NA))
                                    or_lower <- exp(ci[1])
                                    or_upper <- exp(ci[2])
                                }
                            }
                        } else if (model == "Overdominant") {
                            # Overdominant (AA+BB vs AB)
                            model_df$x <- ifelse(sub_class == "AB", 1, 0)
                            res_glm <- tryCatch(glm(y ~ x, data=model_df, family=binomial), error=function(e) NULL)
                            if (!is.null(res_glm)) {
                                sum_glm <- summary(res_glm)
                                if ("x" %in% rownames(sum_glm$coefficients)) {
                                    p_val <- sum_glm$coefficients["x", 4]
                                    or_val <- exp(sum_glm$coefficients["x", 1])
                                    ci <- tryCatch(suppressMessages(confint.default(res_glm, "x", level=0.95)), error=function(e) c(NA, NA))
                                    or_lower <- exp(ci[1])
                                    or_upper <- exp(ci[2])
                                }
                            }
                        } else if (model == "Log-additive") {
                            # Log-additive (0, 1, 2)
                            model_df$x <- ifelse(sub_class == "AA", 0, ifelse(sub_class == "AB", 1, 2))
                            res_glm <- tryCatch(glm(y ~ x, data=model_df, family=binomial), error=function(e) NULL)
                            if (!is.null(res_glm)) {
                                sum_glm <- summary(res_glm)
                                if ("x" %in% rownames(sum_glm$coefficients)) {
                                    p_val <- sum_glm$coefficients["x", 4]
                                    or_val <- exp(sum_glm$coefficients["x", 1])
                                    ci <- tryCatch(suppressMessages(confint.default(res_glm, "x", level=0.95)), error=function(e) c(NA, NA))
                                    or_lower <- exp(ci[1])
                                    or_upper <- exp(ci[2])
                                }
                            }
                        }
                    }
                    
                    raw_p_vals <- c(raw_p_vals, p_val)
                    rows_data[[length(rows_data) + 1]] <- list(
                        marker = marker,
                        model = model,
                        counts_cases = counts_cases,
                        counts_ctrls = counts_ctrls,
                        or_val = or_val,
                        or_lower = or_lower,
                        or_upper = or_upper,
                        p_val = p_val
                    )
                }
            }
            
            # Apply adjustment
            if (length(rows_data) > 0) {
                adj_method <- switch(self$options$adjust,
                                     "bonferroni" = "bonferroni",
                                     "holm" = "holm",
                                     "fdr" = "BH",
                                     "none" = "none")
                
                # Replace NA p-values temporarily to adjust, then put back
                p_adjust_vec <- raw_p_vals
                na_idx <- is.na(p_adjust_vec)
                p_adjust_vec[na_idx] <- 1
                
                adj_p_vals <- if (adj_method != "none") stats::p.adjust(p_adjust_vec, method = adj_method) else p_adjust_vec
                adj_p_vals[na_idx] <- NA
                
                for (i in seq_along(rows_data)) {
                    r <- rows_data[[i]]
                    r$p_adj <- adj_p_vals[i]
                    row_key <- paste(r$marker, r$model, sep="_")
                    assocTable$addRow(rowKey = row_key, value = r)
                    private$g_assoc_results[[row_key]] <- r
                }
            }
        },
        
        .fillPopulationTable = function() {
            popTable <- self$results$popGroup$popTable
            private$g_pop_results <- list()
            
            maf_thr <- switch(self$options$mafThreshold,
                              "0.01" = 0.01,
                              "0.05" = 0.05,
                              "0.10" = 0.10,
                              "none" = 0)
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                sub_class <- na.omit(classified)
                N <- length(sub_class)
                if (N == 0) next
                
                obsAA <- sum(sub_class == "AA")
                obsAB <- sum(sub_class == "AB")
                obsBB <- sum(sub_class == "BB")
                
                # Allele frequencies
                nA <- 2 * obsAA + obsAB
                nB <- 2 * obsBB + obsAB
                total_alleles <- 2 * N
                
                p <- nA / total_alleles
                q <- nB / total_alleles
                
                maf <- min(p, q)
                
                # PIC calculation for biallelic marker:
                # PIC = 1 - (p^2 + q^2) - 2 * p^2 * q^2
                pic <- 1 - (p^2 + q^2) - 2 * (p^2) * (q^2)
                
                # Shannon Information Index:
                # I = -p*ln(p) - q*ln(q)
                shannon <- 0
                if (p > 0) shannon <- shannon - p * log(p)
                if (q > 0) shannon <- shannon - q * log(q)
                
                # Effective number of alleles:
                # Ne = 1 / (p^2 + q^2)
                ne <- 1 / (p^2 + q^2)
                
                row_val <- list(
                    marker = marker,
                    maf = maf,
                    pic = pic,
                    shannon = shannon,
                    ne = ne
                )
                
                popTable$addRow(rowKey = marker, value = row_val)
                private$g_pop_results[[marker]] <- row_val
                
                # Warning for MAF below threshold
                if (maf_thr > 0 && maf < maf_thr) {
                    popTable$addFootnote(rowKey = marker, col = "maf", paste0(.("MAF is below threshold"), " (", maf_thr, ")"))
                }
            }
        },
        
        .runMDRAnalysis = function() {
            # Standard custom MDR
            outcome_col <- self$options$outcome
            outcome_var <- private$g_data[[outcome_col]]
            
            levels_out <- levels(outcome_var)
            if (length(levels_out) < 2) return()
            
            case_level <- levels_out[2]
            control_level <- levels_out[1]
            
            # Stratified CV setup
            df <- private$g_data
            df$outcome_mdr <- ifelse(df[[outcome_col]] == case_level, "Case", "Control")
            
            # Map SNP genotypes to 1, 2, 3
            # Ensure complete cases for MDR
            snps <- self$options$vars
            complete_cases <- complete.cases(df[, c(snps, "outcome_mdr")])
            df_comp <- df[complete_cases, ]
            
            if (nrow(df_comp) < 10) return()
            
            # Build integer matrix of genotypes: AA=1, AB=2, BB=3
            geno_mat <- matrix(0, nrow=nrow(df_comp), ncol=length(snps))
            colnames(geno_mat) <- snps
            for (snp in snps) {
                cl <- private$g_classified[[snp]][complete_cases]
                geno_mat[, snp] <- ifelse(cl == "AA", 1, ifelse(cl == "AB", 2, ifelse(cl == "BB", 3, NA)))
            }
            
            # Run MDR
            folds <- as.integer(self$options$mdrFolds)
            seed <- as.integer(self$options$mdrSeed)
            
            outcome_vec <- df_comp$outcome_mdr
            
            set.seed(seed)
            case_idx <- which(outcome_vec == "Case")
            ctrl_idx <- which(outcome_vec == "Control")
            
            case_folds <- sample(rep(1:folds, length.out = length(case_idx)))
            ctrl_folds <- sample(rep(1:folds, length.out = length(ctrl_idx)))
            
            fold_ids <- rep(0, nrow(df_comp))
            fold_ids[case_idx] <- case_folds
            fold_ids[ctrl_idx] <- ctrl_folds
            
            max_order <- as.integer(self$options$mdrOrder)
            
            best_models <- list()
            private$g_mdr_results <- list()
            
            for (ord in 1:max_order) {
                combs <- combn(snps, ord, simplify=FALSE)
                best_comb <- NULL
                best_train_ba <- -1
                best_test_ba <- -1
                best_cvc <- 0
                
                # Matrix to store train BA for each combination across all folds
                train_ba_mat <- matrix(0, nrow=length(combs), ncol=folds)
                test_ba_mat <- matrix(0, nrow=length(combs), ncol=folds)
                
                for (f in 1:folds) {
                    train_idx <- which(fold_ids != f)
                    test_idx <- which(fold_ids == f)
                    
                    train_y <- outcome_vec[train_idx]
                    test_y <- outcome_vec[test_idx]
                    
                    total_cases_train <- sum(train_y == "Case")
                    total_ctrls_train <- sum(train_y == "Control")
                    threshold <- total_cases_train / total_ctrls_train
                    
                    # Evaluate all combinations
                    for (c_idx in seq_along(combs)) {
                        comb <- combs[[c_idx]]
                        
                        # Generate cell index (1 to 3^ord)
                        cell_idx_train <- rep(1, length(train_idx))
                        cell_idx_test <- rep(1, length(test_idx))
                        
                        for (i in seq_along(comb)) {
                            cell_idx_train <- cell_idx_train + (geno_mat[train_idx, comb[i]] - 1) * (3^(i-1))
                            cell_idx_test <- cell_idx_test + (geno_mat[test_idx, comb[i]] - 1) * (3^(i-1))
                        }
                        
                        # Counts
                        cases_counts <- tabulate(cell_idx_train[train_y == "Case"], nbins = 3^ord)
                        ctrls_counts <- tabulate(cell_idx_train[train_y == "Control"], nbins = 3^ord)
                        
                        ratio <- cases_counts / ctrls_counts
                        is_high <- ratio >= threshold
                        is_high[is.nan(ratio)] <- FALSE
                        is_high[is.infinite(ratio)] <- TRUE
                        
                        # Predict train
                        pred_train <- ifelse(is_high[cell_idx_train], "Case", "Control")
                        pred_train[is.na(cell_idx_train)] <- "Control"
                        
                        se_tr <- sum(pred_train == "Case" & train_y == "Case") / max(1, total_cases_train)
                        sp_tr <- sum(pred_train == "Control" & train_y == "Control") / max(1, total_ctrls_train)
                        train_ba_mat[c_idx, f] <- (se_tr + sp_tr) / 2
                        
                        # Predict test
                        pred_test <- ifelse(is_high[cell_idx_test], "Case", "Control")
                        pred_test[is.na(cell_idx_test)] <- "Control"
                        
                        se_te <- sum(pred_test == "Case" & test_y == "Case") / max(1, sum(test_y == "Case"))
                        sp_te <- sum(pred_test == "Control" & test_y == "Control") / max(1, sum(test_y == "Control"))
                        test_ba_mat[c_idx, f] <- (se_te + sp_te) / 2
                    }
                }
                
                # Calculate metrics for each combination
                mean_train_ba <- rowMeans(train_ba_mat)
                mean_test_ba <- rowMeans(test_ba_mat)
                
                # Best model per fold based on max train BA
                best_fold_combs <- apply(train_ba_mat, 2, which.max)
                cvc_counts <- table(best_fold_combs)
                
                # Overall best combination is the one selected most often
                best_comb_idx <- as.integer(names(which.max(cvc_counts)))
                overall_best_comb <- combs[[best_comb_idx]]
                
                # CVC is the count of votes
                overall_cvc <- as.integer(cvc_counts[as.character(best_comb_idx)])
                
                best_models[[ord]] <- list(
                    order = ord,
                    combination = paste(overall_best_comb, collapse=" * "),
                    snps = overall_best_comb,
                    train_ba = mean_train_ba[best_comb_idx],
                    test_ba = mean_test_ba[best_comb_idx],
                    cvc = overall_cvc
                )
            }
            
            # 4.1 Run Permutation test if selected
            if (self$options$mdrPermTest) {
                n_perm <- as.integer(self$options$nPermMDR)
                perm_test_bas <- matrix(0, nrow=max_order, ncol=n_perm)
                
                # Perform permutations
                for (p in 1:n_perm) {
                    perm_y <- sample(outcome_vec)
                    
                    # We repeat the MDR CV for each order
                    for (ord in 1:max_order) {
                        combs <- combn(snps, ord, simplify=FALSE)
                        train_ba_mat_perm <- matrix(0, nrow=length(combs), ncol=folds)
                        test_ba_mat_perm <- matrix(0, nrow=length(combs), ncol=folds)
                        
                        for (f in 1:folds) {
                            train_idx <- which(fold_ids != f)
                            test_idx <- which(fold_ids == f)
                            
                            train_y_p <- perm_y[train_idx]
                            test_y_p <- perm_y[test_idx]
                            
                            total_cases_train <- sum(train_y_p == "Case")
                            total_ctrls_train <- sum(train_y_p == "Control")
                            threshold <- total_cases_train / total_ctrls_train
                            
                            for (c_idx in seq_along(combs)) {
                                comb <- combs[[c_idx]]
                                cell_idx_train <- rep(1, length(train_idx))
                                cell_idx_test <- rep(1, length(test_idx))
                                
                                for (i in seq_along(comb)) {
                                    cell_idx_train <- cell_idx_train + (geno_mat[train_idx, comb[i]] - 1) * (3^(i-1))
                                    cell_idx_test <- cell_idx_test + (geno_mat[test_idx, comb[i]] - 1) * (3^(i-1))
                                }
                                
                                cases_counts <- tabulate(cell_idx_train[train_y_p == "Case"], nbins = 3^ord)
                                ctrls_counts <- tabulate(cell_idx_train[train_y_p == "Control"], nbins = 3^ord)
                                
                                ratio <- cases_counts / ctrls_counts
                                is_high <- ratio >= threshold
                                is_high[is.nan(ratio)] <- FALSE
                                is_high[is.infinite(ratio)] <- TRUE
                                
                                pred_train <- ifelse(is_high[cell_idx_train], "Case", "Control")
                                pred_train[is.na(cell_idx_train)] <- "Control"
                                
                                se_tr <- sum(pred_train == "Case" & train_y_p == "Case") / max(1, total_cases_train)
                                sp_tr <- sum(pred_train == "Control" & train_y_p == "Control") / max(1, total_ctrls_train)
                                train_ba_mat_perm[c_idx, f] <- (se_tr + sp_tr) / 2
                                
                                pred_test <- ifelse(is_high[cell_idx_test], "Case", "Control")
                                pred_test[is.na(cell_idx_test)] <- "Control"
                                
                                se_te <- sum(pred_test == "Case" & test_y_p == "Case") / max(1, sum(test_y_p == "Case"))
                                sp_te <- sum(pred_test == "Control" & test_y_p == "Control") / max(1, sum(test_y_p == "Control"))
                                test_ba_mat_perm[c_idx, f] <- (se_te + sp_te) / 2
                            }
                        }
                        
                        # Mean test BA of best permuted model (based on train BA max)
                        best_fold_combs_perm <- apply(train_ba_mat_perm, 2, which.max)
                        cvc_counts_perm <- table(best_fold_combs_perm)
                        best_comb_idx_perm <- as.integer(names(which.max(cvc_counts_perm)))
                        
                        perm_test_bas[ord, p] <- mean(test_ba_mat_perm[best_comb_idx_perm, ])
                    }
                }
                
                # Assign p-values
                for (ord in 1:max_order) {
                    obs_test_ba <- best_models[[ord]]$test_ba
                    p_val <- sum(perm_test_bas[ord, ] >= obs_test_ba) / n_perm
                    best_models[[ord]]$p_val <- p_val
                }
            }
            
            # Fill tables
            mdrBestTable <- self$results$mdrGroup$mdrBestTable
            for (ord in 1:max_order) {
                m <- best_models[[ord]]
                row_val <- list(
                    order = m$order,
                    combination = m$combination,
                    train_ba = m$train_ba,
                    test_ba = m$test_ba,
                    cvc = m$cvc
                )
                if (self$options$mdrPermTest) row_val$p_val <- m$p_val
                
                mdrBestTable$addRow(rowKey = as.character(m$order), value = row_val)
                private$g_mdr_results[[as.character(m$order)]] <- row_val
            }
            
            # Save best models state for cells table and plot
            image_best_models <- best_models
            
            # Fill cells table if checked
            if (self$options$mdrCellTable) {
                mdrCellTable <- self$results$mdrGroup$mdrCellTable
                
                # Fill for the overall best model (highest testing BA among all orders)
                test_bas <- sapply(best_models, function(x) x$test_ba)
                best_ord <- which.max(test_bas)
                m_best <- best_models[[best_ord]]
                
                comb_snps <- m_best$snps
                
                # Counts in whole dataset
                cell_idx_all <- rep(1, nrow(df_comp))
                for (i in seq_along(comb_snps)) {
                    cell_idx_all <- cell_idx_all + (geno_mat[, comb_snps[i]] - 1) * (3^(i-1))
                }
                
                # Get unique genotype cells present in data
                comb_genotypes <- list()
                for (snp in comb_snps) {
                    cl <- private$g_classified[[snp]][complete_cases]
                    comb_genotypes[[snp]] <- cl
                }
                
                # Unique cells observed
                cells_df <- unique(df_comp[, comb_snps, drop=FALSE])
                
                total_cases <- sum(outcome_vec == "Case")
                total_ctrls <- sum(outcome_vec == "Control")
                threshold <- total_cases / total_ctrls
                
                for (r in 1:nrow(cells_df)) {
                    row_data <- cells_df[r, , drop=FALSE]
                    cell_label <- paste(sapply(comb_snps, function(s) paste0(s, ":", row_data[1, s])), collapse=", ")
                    
                    # Match indexes
                    match_idx <- rep(TRUE, nrow(df_comp))
                    for (snp in comb_snps) {
                        match_idx <- match_idx & (df_comp[[snp]] == row_data[1, snp])
                    }
                    
                    cases <- sum(match_idx & outcome_vec == "Case")
                    controls <- sum(match_idx & outcome_vec == "Control")
                    
                    ratio <- if (controls > 0) cases / controls else Inf
                    risk <- if (ratio >= threshold) .("High Risk") else .("Low Risk")
                    
                    row_val <- list(
                        combination = m_best$combination,
                        cell = cell_label,
                        cases = cases,
                        controls = controls,
                        ratio = if (is.infinite(ratio)) NA else ratio,
                        risk = risk
                    )
                    
                    mdrCellTable$addRow(rowKey = as.character(r), value = row_val)
                }
            }
        },
        
        .fillLDTable = function() {
            ldTable <- self$results$ldGroup$ldTable
            private$g_ld_results <- list()
            
            snps <- self$options$vars
            if (length(snps) < 2) return()
            
            pairs <- combn(snps, 2, simplify=FALSE)
            
            for (pair in pairs) {
                s1 <- pair[1]
                s2 <- pair[2]
                
                cl1 <- private$g_classified[[s1]]
                cl2 <- private$g_classified[[s2]]
                
                complete_cases <- !is.na(cl1) & !is.na(cl2)
                c1 <- cl1[complete_cases]
                c2 <- cl2[complete_cases]
                N <- length(c1)
                
                if (N == 0) next
                
                # Contingency table (3x3)
                tbl3x3 <- table(factor(c1, levels=c("AA", "AB", "BB")),
                                factor(c2, levels=c("AA", "AB", "BB")))
                
                # Estimate haplotype frequencies
                hap_freqs <- private$estimateHaplotypes(tbl3x3)
                p11 <- hap_freqs["p11"]
                p12 <- hap_freqs["p12"]
                p21 <- hap_freqs["p21"]
                p22 <- hap_freqs["p22"]
                
                # Marginal allele frequencies
                pA <- p11 + p12
                pa <- 1 - pA
                pB <- p11 + p21
                pb <- 1 - pB
                
                # D coefficient
                d <- p11 - (pA * pB)
                
                # D' (normalized D)
                d_prime <- 0
                if (d >= 0) {
                    d_max <- min(pA * pb, pa * pB)
                    if (d_max > 0) d_prime <- d / d_max
                } else {
                    d_max <- min(pA * pB, pa * pb)
                    if (d_max > 0) d_prime <- d / d_max
                }
                
                # r²
                denom <- pA * pa * pB * pb
                r2 <- 0
                if (denom > 0) {
                    r2 <- (d^2) / denom
                }
                
                # Significance test (2 N r2 follows chi-square with 1 df)
                chi_stat <- 2 * N * r2
                p_val <- stats::pchisq(chi_stat, df = 1, lower.tail = FALSE)
                
                row_val <- list(
                    marker1 = s1,
                    marker2 = s2,
                    d = d,
                    d_prime = d_prime,
                    r2 = r2,
                    p_val = p_val
                )
                
                ldTable$addRow(rowKey = paste(s1, s2, sep="_"), value = row_val)
                private$g_ld_results[[paste(s1, s2, sep="_")]] <- row_val
            }
        },
        
        estimateHaplotypes = function(n) {
            N <- sum(n)
            if (N == 0) return(c(p11=0.25, p12=0.25, p21=0.25, p22=0.25))
            
            pA <- (2 * sum(n[1,]) + sum(n[2,])) / (2 * N)
            pB <- (2 * sum(n[,1]) + sum(n[,2])) / (2 * N)
            
            p11 <- pA * pB
            p12 <- pA * (1 - pB)
            p21 <- (1 - pA) * pB
            p22 <- (1 - pA) * (1 - pB)
            
            for (iter in 1:100) {
                p11_old <- p11
                
                denom <- (p11 * p22) + (p12 * p21)
                theta <- if (denom == 0) 0.5 else (p11 * p22) / denom
                
                nAB <- 2 * n[1,1] + n[1,2] + n[2,1] + 2 * theta * n[2,2]
                nAb <- 2 * n[1,3] + n[1,2] + n[2,3] + 2 * (1 - theta) * n[2,2]
                naB <- 2 * n[3,1] + n[2,1] + n[3,2] + 2 * (1 - theta) * n[2,2]
                nab <- 2 * n[3,3] + n[2,3] + n[3,2] + 2 * theta * n[2,2]
                
                p11 <- nAB / (2 * N)
                p12 <- nAb / (2 * N)
                p21 <- naB / (2 * N)
                p22 <- nab / (2 * N)
                
                if (abs(p11 - p11_old) < 1e-7) break
            }
            
            return(c(p11=p11, p12=p12, p21=p21, p22=p22))
        },
        
        # ------------------ Ploting functions ------------------
        
        .plotTernary = function(image, ...) {
            if (length(self$options$vars) == 0) return(FALSE)
            
            # Draw ternary diagram using HardyWeinberg package
            has_group <- !is.null(self$options$group)
            groups <- if (has_group) levels(private$g_data[[self$options$group]]) else "Total"
            
            # Collect counts matrix
            counts_list <- list()
            marker_names <- character()
            group_names <- character()
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                for (g in groups) {
                    subset_idx <- if (has_group) which(private$g_data[[self$options$group]] == g) else seq_along(classified)
                    sub_class <- na.omit(classified[subset_idx])
                    
                    obsAA <- sum(sub_class == "AA")
                    obsAB <- sum(sub_class == "AB")
                    obsBB <- sum(sub_class == "BB")
                    
                    counts_list[[length(counts_list) + 1]] <- c(AA=obsAA, AB=obsAB, BB=obsBB)
                    marker_names <- c(marker_names, marker)
                    group_names <- c(group_names, g)
                }
            }
            
            counts_mat <- do.call(rbind, counts_list)
            rownames(counts_mat) <- if (has_group) {
                outer(self$options$vars, groups, paste, sep="_")
            } else {
                self$options$vars
            }
            
            region_type <- as.integer(self$options$ternaryRegion)
            
            # Define premium colors
            palette <- c("#2B5C8F", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
            col_green <- "#2E7D32"
            col_red <- "#C62828"
            
            # Map colors and labels based on groups/HWE significance
            col_vec <- character()
            labels_vec <- character()
            
            for (i in seq_len(nrow(counts_mat))) {
                if (has_group) {
                    # Color by population group
                    col_idx <- which(groups == group_names[i])
                    col_idx <- ((col_idx - 1) %% length(palette)) + 1
                    col_vec <- c(col_vec, palette[col_idx])
                    
                    labels_vec <- c(labels_vec, paste0(marker_names[i], " (", group_names[i], ")"))
                } else {
                    # Color by HWE significance (green if p >= 0.05, red if p < 0.05)
                    row_key <- paste(marker_names[i], group_names[i], sep="_")
                    row <- private$g_hwe_results[[row_key]]
                    is_sig <- FALSE
                    if (!is.null(row) && !is.na(row$exact_p)) {
                        is_sig <- (row$exact_p < 0.05)
                    }
                    col_vec <- c(col_vec, if (is_sig) col_red else col_green)
                    
                    labels_vec <- c(labels_vec, marker_names[i])
                }
            }
            
            # Set margins to leave space for legend at the bottom
            old_par <- par(mar = c(5.5, 4, 4, 2) + 0.1)
            on.exit(par(old_par))
            
            # Alternating positions: 3 (above), 4 (right), 2 (left), 1 (below) to avoid label overlaps
            pos_choices <- c(3, 4, 2, 1)
            pos_vec <- rep(pos_choices, length.out = nrow(counts_mat))
            
            # Plot ternary plot with custom colors and labels next to points
            HardyWeinberg::HWTernaryPlot(
                counts_mat, 
                region = region_type, 
                signifcolour = FALSE, 
                markercol = col_vec, 
                markerbgcol = col_vec, 
                markerlab = labels_vec,
                markerpos = pos_vec,
                mcex = 0.8,
                cex = 1.5,
                vertex.cex = 1.2
            )
            
            # Allow plotting outside the plot region to draw legend below the triangle
            par(xpd = TRUE)
            
            # Add a legend to explain the colors at the bottom (y = -0.08 is close to base)
            if (has_group) {
                legend(0.5, -0.08, legend = groups, col = palette[1:length(groups)], pch = 19, 
                       cex = 1.1, horiz = TRUE, xjust = 0.5, bty = "n")
            } else {
                legend(0.5, -0.08, legend = c(.("HWE compliant"), .("HWE deviation")), 
                       col = c(col_green, col_red), pch = 19, 
                       cex = 1.1, horiz = TRUE, xjust = 0.5, bty = "n")
            }
            
            TRUE
        },
        
        .plotQQ = function(image, ...) {
            if (length(self$options$vars) == 0) return(FALSE)
            
            has_group <- !is.null(self$options$group)
            groups <- if (has_group) levels(private$g_data[[self$options$group]]) else "Total"
            
            # Collect counts matrix (HWQqplot expects genotype counts columns: AA, AB, BB)
            counts_list <- list()
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                for (g in groups) {
                    subset_idx <- if (has_group) which(private$g_data[[self$options$group]] == g) else seq_along(classified)
                    sub_class <- na.omit(classified[subset_idx])
                    
                    obsAA <- sum(sub_class == "AA")
                    obsAB <- sum(sub_class == "AB")
                    obsBB <- sum(sub_class == "BB")
                    
                    if (obsAA + obsAB + obsBB > 0) {
                        counts_list[[length(counts_list) + 1]] <- c(AA=obsAA, AB=obsAB, BB=obsBB)
                    }
                }
            }
            
            if (length(counts_list) == 0) return(FALSE)
            
            counts_mat <- do.call(rbind, counts_list)
            
            # Compute exact p-values
            pvals <- HardyWeinberg::HWExactStats(counts_mat, plinkcode = FALSE)
            out <- sort(pvals, index.return = TRUE)
            pvals <- out$x
            counts_mat <- counts_mat[out$ix, ]
            n <- length(pvals)
            
            # Simulate expected p-values under the null hypothesis (nsim = 100)
            epvals <- HardyWeinberg:::getpvals(counts_mat, 100)
            
            lpvals <- -log10(pvals)
            lepvals <- -log10(epvals)
            
            mm <- max(c(lpvals, lepvals))
            if (is.na(mm) || is.infinite(mm)) mm <- 5
            
            # Render Q-Q plot without forwarding graphical ... arguments to statistical functions
            plot(
                lpvals, lpvals, 
                xlab = .("Expected p-value (-log10)"), 
                ylab = .("Observed p-value (-log10)"), 
                xlim = c(0, mm), 
                ylim = c(0, mm), 
                main = .("Q-Q plot for HWE"), 
                type = "n"
            )
            
            for (i in 1:100) {
                points(sort(lepvals[, i]), sort(lpvals), type = "l", col = "grey")
            }
            abline(0, 1, col = "green", lwd = 2)
            
            TRUE
        },
        
        .plotBar = function(image, ...) {
            if (length(self$options$vars) == 0) return(FALSE)
            
            has_group <- !is.null(self$options$group)
            groups <- if (has_group) levels(private$g_data[[self$options$group]]) else "Total"
            
            plot_df <- data.frame()
            obs_label <- .("Observed")
            exp_label <- .("Expected")
            
            for (marker in self$options$vars) {
                for (g in groups) {
                    row_key <- paste(marker, g, sep="_")
                    row <- private$g_hwe_results[[row_key]]
                    if (!is.null(row)) {
                        plot_df <- rbind(plot_df, data.frame(
                            Marker = marker,
                            Group = g,
                            Genotype = c("AA", "AB", "BB", "AA", "AB", "BB"),
                            Type = c(obs_label, obs_label, obs_label, exp_label, exp_label, exp_label),
                            Count = c(row$obsAA, row$obsAB, row$obsBB, row$expAA, row$expAB, row$expBB)
                        ))
                    }
                }
            }
            
            if (nrow(plot_df) == 0) return(FALSE)
            
            fill_colors <- setNames(c("#2B5C8F", "#D95F02"), c(obs_label, exp_label))
            
            p <- ggplot(plot_df, aes(x = Genotype, y = Count, fill = Type)) +
                geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
                scale_fill_manual(values = fill_colors) +
                theme_minimal(base_size = 12) +
                theme(panel.grid.major.x = element_blank(),
                      legend.position = "bottom",
                      legend.text = element_text(size = 12),
                      legend.title = element_text(size = 12)) +
                labs(x = .("Genotype"), y = .("Count"), fill = .("Type"))
                
            if (has_group) {
                p <- p + facet_grid(Group ~ Marker)
            } else {
                p <- p + facet_wrap(~ Marker)
            }
            
            print(p)
            TRUE
        },
        
        .plotForest = function(image, ...) {
            if (length(self$options$vars) == 0 || is.null(self$options$outcome)) return(FALSE)
            
            plot_df <- data.frame()
            
            # Select models
            model_list <- c("Codominant (AB vs AA)", "Codominant (BB vs AA)", "Dominant", "Recessive", "Overdominant", "Log-additive")
            if (self$options$geneticModel != "all") {
                model_list <- switch(self$options$geneticModel,
                                     "codominant" = c("Codominant (AB vs AA)", "Codominant (BB vs AA)"),
                                     "dominant" = "Dominant",
                                     "recessive" = "Recessive",
                                     "overdominant" = "Overdominant",
                                     "log-additive" = "Log-additive")
            }
            
            translate_model <- function(m) {
                switch(m,
                       "Codominant (AB vs AA)" = .("Codominant (AB vs AA)"),
                       "Codominant (BB vs AA)" = .("Codominant (BB vs AA)"),
                       "Dominant" = .("Dominant"),
                       "Recessive" = .("Recessive"),
                       "Overdominant" = .("Overdominant"),
                       "Log-additive" = .("Log-additive"),
                       m)
            }
            
            for (marker in self$options$vars) {
                for (model in model_list) {
                    row_key <- paste(marker, model, sep="_")
                    row <- private$g_assoc_results[[row_key]]
                    if (!is.null(row) && !is.na(row$or_val)) {
                        plot_df <- rbind(plot_df, data.frame(
                            Marker = marker,
                            Model = translate_model(model),
                            OR = row$or_val,
                            Lower = row$or_lower,
                            Upper = row$or_upper
                        ))
                    }
                }
            }
            
            if (nrow(plot_df) == 0) return(FALSE)
            
            p <- ggplot(plot_df, aes(x = OR, y = Marker)) +
                geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray50") +
                geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, size = 0.8, color = "#2B5C8F") +
                geom_point(size = 3.5, color = "#D95F02") +
                scale_x_log10() +
                theme_minimal(base_size = 12) +
                labs(x = .("Odds Ratio (log scale)"), y = .("Marker"))
                
            if (self$options$geneticModel == "all") {
                p <- p + facet_wrap(~ Model, ncol = 2)
            } else {
                model_title <- switch(self$options$geneticModel,
                                      "codominant" = .("Codominant"),
                                      "dominant" = .("Dominant"),
                                      "recessive" = .("Recessive"),
                                      "overdominant" = .("Overdominant"),
                                      "log-additive" = .("Log-additive"))
                p <- p + labs(title = paste(.("Genetic model:"), model_title))
            }
            
            print(p)
            TRUE
        },
        
        .plotMDRHeatmap = function(image, ...) {
            if (length(self$options$vars) == 0 || is.null(self$options$outcome)) return(FALSE)
            
            # Find the best 2-way model
            best_2way_row <- private$g_mdr_results[["2"]]
            if (is.null(best_2way_row) || is.na(best_2way_row$combination)) return(FALSE)
            
            # Parse SNP names
            combination <- best_2way_row$combination
            comb_snps <- unlist(strsplit(combination, " * ", fixed=TRUE))
            if (length(comb_snps) != 2) return(FALSE)
            
            # Reconstruct counts in dataset
            outcome_col <- self$options$outcome
            outcome_var <- private$g_data[[outcome_col]]
            levels_out <- levels(outcome_var)
            if (length(levels_out) < 2) return(FALSE)
            
            case_level <- levels_out[2]
            control_level <- levels_out[1]
            
            df <- private$g_data
            df$outcome_mdr <- ifelse(df[[outcome_col]] == case_level, "Case", "Control")
            complete_cases <- complete.cases(df[, c(comb_snps, "outcome_mdr")])
            df_comp <- df[complete_cases, ]
            
            total_cases <- sum(df_comp$outcome_mdr == "Case")
            total_ctrls <- sum(df_comp$outcome_mdr == "Control")
            threshold <- total_cases / total_ctrls
            
            # Genotypes grids
            g1 <- private$g_classified[[comb_snps[1]]][complete_cases]
            g2 <- private$g_classified[[comb_snps[2]]][complete_cases]
            
            grid_df <- expand.grid(
                G1 = c("AA", "AB", "BB"),
                G2 = c("AA", "AB", "BB")
            )
            
            plot_list <- list()
            for (r in 1:nrow(grid_df)) {
                val1 <- grid_df$G1[r]
                val2 <- grid_df$G2[r]
                
                match_idx <- (g1 == val1) & (g2 == val2)
                cases <- sum(match_idx & df_comp$outcome_mdr == "Case")
                controls <- sum(match_idx & df_comp$outcome_mdr == "Control")
                
                ratio <- if (controls > 0) cases / controls else if (cases > 0) Inf else 0
                risk <- if (ratio >= threshold) "High" else "Low"
                
                cases_lbl <- .("Ca")
                ctrls_lbl <- .("Co")
                plot_list[[r]] <- data.frame(
                    SNP1 = val1,
                    SNP2 = val2,
                    Cases = cases,
                    Controls = controls,
                    Ratio = ratio,
                    Risk = risk,
                    Label = paste0(cases_lbl, ":", cases, "\n", ctrls_lbl, ":", controls)
                )
            }
            plot_df <- do.call(rbind, plot_list)
            
            p <- ggplot(plot_df, aes(x = SNP1, y = SNP2, fill = Risk)) +
                geom_tile(color = "black", size = 0.5) +
                geom_text(aes(label = Label), size = 5.5, color = "black", fontface = "bold") +
                scale_fill_manual(values = c("High" = "#FF8C8C", "Low" = "#8CCEFF"),
                                  labels = c("High" = .("High Risk"), "Low" = .("Low Risk"))) +
                theme_minimal(base_size = 12) +
                labs(x = comb_snps[1], y = comb_snps[2], fill = .("Risk Class"))
                
            print(p)
            TRUE
        },
        
        .plotMDRBar = function(image, ...) {
            plot_df <- data.frame()
            train_lbl <- .("Training")
            test_lbl <- .("Testing")
            
            max_order <- as.integer(self$options$mdrOrder)
            for (ord in 1:max_order) {
                row <- private$g_mdr_results[[as.character(ord)]]
                if (!is.null(row) && !is.na(row$train_ba)) {
                    plot_df <- rbind(plot_df, data.frame(
                        Order = factor(ord),
                        Type = train_lbl,
                        BA = row$train_ba
                    ))
                    plot_df <- rbind(plot_df, data.frame(
                        Order = factor(ord),
                        Type = test_lbl,
                        BA = row$test_ba
                    ))
                }
            }
            
            if (nrow(plot_df) == 0) return(FALSE)
            
            fill_colors <- setNames(c("#2B5C8F", "#D95F02"), c(train_lbl, test_lbl))
            
            p <- ggplot(plot_df, aes(x = Order, y = BA, fill = Type)) +
                geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.2) +
                scale_fill_manual(values = fill_colors) +
                coord_cartesian(ylim = c(0.4, 1.0)) +
                theme_minimal(base_size = 12) +
                theme(legend.position = "bottom",
                      legend.text = element_text(size = 12),
                      legend.title = element_text(size = 12)) +
                labs(x = .("Interaction Order"), y = .("Balanced Accuracy (BA)"), fill = .("Type"))
                
            print(p)
            TRUE
        },
        
        .plotLDHeatmap = function(image, ...) {
            snps <- self$options$vars
            if (length(snps) < 2) return(FALSE)
            
            # Parse data
            ld_df <- data.frame()
            for (row_key in names(private$g_ld_results)) {
                row <- private$g_ld_results[[row_key]]
                if (!is.null(row) && !is.na(row$r2)) {
                    r2_val <- as.numeric(row$r2)
                    dp_val <- as.numeric(row$d_prime)
                    metric_val <- if (self$options$ldMetric == "r2") r2_val else dp_val
                    ld_df <- rbind(ld_df, data.frame(
                        Marker1 = factor(row$marker1, levels=snps),
                        Marker2 = factor(row$marker2, levels=snps),
                        Value = metric_val
                    ))
                }
            }
            
            if (nrow(ld_df) == 0) return(FALSE)
            
            metric_title <- if (self$options$ldMetric == "r2") "r²" else "D'"
            
            p <- ggplot(ld_df, aes(x = Marker1, y = Marker2, fill = Value)) +
                geom_tile(color = "gray80", size = 0.5) +
                geom_text(aes(label = sprintf("%.2f", Value)), color = "black", size = 5, fontface = "bold") +
                scale_fill_gradient(low = "#FFFFFF", high = "#E41A1C", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
                theme_minimal(base_size = 12) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                      axis.text.y = element_text(size = 11),
                      panel.grid = element_blank(),
                      legend.text = element_text(size = 11),
                      legend.title = element_text(size = 12)) +
                labs(x = "", y = "", fill = metric_title)
                
            print(p)
            TRUE
        }
    )
)
