
mSNPClass <- R6::R6Class(
    "mSNPClass",
    inherit = mSNPBase,
    private = list(
        # Internal state
        g_data = NULL,
        g_alleles = list(), # list of list(major, minor) per marker
        g_counts = list(),  # list of c(AA, AB, BB) per marker and population
        g_classified = list(), # list of classified genotypes per marker
        g_qc_status = list(),  # list of QC statuses per marker
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
            
            if (isTRUE(self$options$freqPct)) {
                freqTable$getColumn("p_freq")$setTitle(paste0(.("p (A)"), " (%)"))
                freqTable$getColumn("q_freq")$setTitle(paste0(.("q (B)"), " (%)"))
                freqTable$getColumn("obsAA")$setTitle(paste0(.("Obs AA"), " (n, %)"))
                freqTable$getColumn("obsAB")$setTitle(paste0(.("Obs AB"), " (n, %)"))
                freqTable$getColumn("obsBB")$setTitle(paste0(.("Obs BB"), " (n, %)"))
                freqTable$getColumn("expAA")$setTitle(paste0(.("Exp AA"), " (n, %)"))
                freqTable$getColumn("expAB")$setTitle(paste0(.("Exp AB"), " (n, %)"))
                freqTable$getColumn("expBB")$setTitle(paste0(.("Exp BB"), " (n, %)"))
                freqTable$getColumn("h_obs")$setTitle(paste0(.("H(obs)"), " (%)"))
                freqTable$getColumn("h_exp")$setTitle(paste0(.("H(exp)"), " (%)"))
            } else {
                freqTable$getColumn("p_freq")$setTitle(.("p (A)"))
                freqTable$getColumn("q_freq")$setTitle(.("q (B)"))
                freqTable$getColumn("obsAA")$setTitle(.("Obs AA"))
                freqTable$getColumn("obsAB")$setTitle(.("Obs AB"))
                freqTable$getColumn("obsBB")$setTitle(.("Obs BB"))
                freqTable$getColumn("expAA")$setTitle(.("Exp AA"))
                freqTable$getColumn("expAB")$setTitle(.("Exp AB"))
                freqTable$getColumn("expBB")$setTitle(.("Exp BB"))
                freqTable$getColumn("h_obs")$setTitle(.("H(obs)"))
                freqTable$getColumn("h_exp")$setTitle(.("H(exp)"))
            }
            
            # Setup groupTable columns dynamically
            groupTable <- self$results$hweGroup$groupAnalysisTable
            has_group <- !is.null(self$options$group)
            show_group_table <- has_group && isTRUE(self$options$groupTable)
            groupTable$setVisible(show_group_table)
            if (isTRUE(self$options$groupAlleles)) {
                groupTable$setTitle(.("Analysis of Alleles and Genotypes by Groups"))
            } else {
                groupTable$setTitle(.("Analysis of Genotypes by Groups"))
            }
            
            if (show_group_table) {
                group_var_name <- self$options$group
                if (group_var_name %in% names(self$data)) {
                    groups <- levels(self$data[[group_var_name]])
                    if (is.null(groups) || length(groups) == 0) {
                        groups <- levels(factor(self$data[[group_var_name]]))
                    }
                    for (g in groups) {
                        groupTable$addColumn(name = paste0("grp_", g), title = g, type = "text")
                    }
                    # Calculate degrees of freedom
                    # df for a contingency table is (r - 1) * (c - 1)
                    # c = G (number of groups), r = 2 for Genotype test, r = 3 for Polymorphism test
                    G <- length(groups)
                    if (G < 2) G <- 2 # fallback
                    df_gt <- G - 1
                    df_poly <- if (isTRUE(self$options$polyDf1)) G - 1 else 2 * (G - 1)

                    groupTable$addColumn(
                        name = "genotype_chi",
                        title = paste0("χ² (df=", df_gt, ")"),
                        type = "number",
                        combineBelow = TRUE,
                        superTitle = .("Genotype test")
                    )
                    groupTable$addColumn(
                        name = "genotype_p",
                        title = .("p"),
                        type = "number",
                        format = "zto,pvalue",
                        combineBelow = TRUE,
                        superTitle = .("Genotype test")
                    )
                    groupTable$addColumn(
                        name = "poly_chi",
                        title = paste0("χ² (df=", df_poly, ")"),
                        type = "number",
                        combineBelow = TRUE,
                        superTitle = .("Polymorphism test")
                    )
                    groupTable$addColumn(
                        name = "poly_p",
                        title = .("p"),
                        type = "number",
                        format = "zto,pvalue",
                        combineBelow = TRUE,
                        superTitle = .("Polymorphism test")
                    )
                }
            }
            
            # Setup notes
            freqTable$setNote('n1', paste0("<b>", .("Allele frequencies (p, q)"), "</b> ", .("represent the proportions of the two alleles in the sample. For a biallelic marker with alleles A and B: p = freq(A) = (2*n_AA + n_AB) / (2*N), q = 1 - p.")))
            freqTable$setNote('n2', paste0("<b>", .("Observed genotype frequencies"), "</b> ", .("are the raw counts and proportions of each genotype (AA, AB, BB) directly from the data.")))
            freqTable$setNote('n3', paste0("<b>", .("Expected genotype frequencies"), "</b> ", .("are the counts predicted under Hardy-Weinberg equilibrium: E(AA) = p² × N, E(AB) = 2pq × N, E(BB) = q² × N.")))
            freqTable$setNote('n4', paste0("<b>", .("Observed heterozygosity (H_obs)"), "</b> ", .("is the proportion of heterozygous individuals (AB) in the sample: H_obs = n_AB / N.")))
            freqTable$setNote('n5', paste0("<b>", .("Expected heterozygosity (H_exp)"), "</b> ", .("is the heterozygosity expected under HWE: H_exp = 2pq. A significant difference between H_obs and H_exp may indicate inbreeding, population substructure, or selection.")))
            
            hwTable$setNote('n1', paste0("<b>", .("Chi-square test"), "</b> ", .("is the classical Pearson goodness-of-fit test comparing observed and expected genotype counts under HWE. Suitable for large sample sizes (N > 50). Asymptotic; may be unreliable for rare alleles.")))
            hwTable$setNote('n2', paste0("<b>", .("Exact test"), "</b> ", .("(Haldane, 1954) computes the exact probability of the observed heterozygote count, conditional on the allele counts. Recommended for small samples or rare alleles.")))
            hwTable$setNote('n3', paste0("<b>", .("Likelihood-ratio test"), "</b> ", .("compares the likelihood of the data under HWE versus the saturated model. The test statistic -2 × ln(L_HWE / L_sat) follows chi-square with 1 df.")))
            hwTable$setNote('n4', paste0("<b>", .("Permutation test"), "</b> ", .("estimates the p-value by randomly permuting alleles among genotypes. Distribution-free; valid for any sample size.")))
            hwTable$setNote('n5', paste0("<b>", .("Inbreeding coefficient (F_IS)"), "</b> ", .("measures deviation from HWE: F_IS = 1 - H_obs / H_exp. "), "<b>", .("F_IS > 0"), "</b> ", .("indicates heterozygote deficiency (inbreeding), "), "<b>", .("F_IS < 0"), "</b> ", .("indicates heterozygote excess (outbreeding), "), "<b>", .("F_IS = 0"), "</b> ", .("indicates Hardy-Weinberg equilibrium.")))
            
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
            
            # 0. Quality Control Analysis
            private$.performQC()
            
            # 1. HWE Analysis
            if (self$options$freqTable || self$options$hwTests || self$options$groupTable || self$options$ternaryPlot || self$options$qqPlot || self$options$barPlot || self$options$hweSplitOutcome) {
                private$.fillFreqAndHWETables()
            }
            
            # 1b. Group Table Analysis
            has_group <- !is.null(self$options$group)
            show_group_table <- has_group && isTRUE(self$options$groupTable)
            self$results$hweGroup$groupAnalysisTable$setVisible(show_group_table)
            if (show_group_table) {
                private$.fillGroupTable()
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
            
            # 6. Polygenic Risk Score
            if (self$options$prsOutput) {
                private$.calculatePRS()
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
                
                # Find the heterozygote genotype and define major (wild-type) allele as its first character
                user_wt <- NULL
                unique_genotypes <- unique(as.character(non_na))
                for (geno in unique_genotypes) {
                    cleaned_geno <- gsub("[^A-Za-z0-9]", "", geno)
                    geno_chars <- unlist(strsplit(cleaned_geno, ""))
                    if (length(unique(geno_chars)) == 2) {
                        if (nchar(cleaned_geno) >= 2) {
                            user_wt <- substr(cleaned_geno, 1, 1)
                            break
                        }
                    }
                }
                
                if (!is.null(user_wt) && user_wt %in% names(allele_counts)) {
                    major <- user_wt
                    # Minor is the other allele (not the major one)
                    other_alleles <- setdiff(names(allele_counts), major)
                    minor <- if (length(other_alleles) > 0) other_alleles[1] else major
                } else {
                    major <- alleles_sorted[1]
                    minor <- if (length(alleles_sorted) > 1) alleles_sorted[2] else major
                }
                
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
        
        .performQC = function() {
            private$g_qc_status <- list()
            
            call_rate_thr <- switch(self$options$qcCallRate,
                                    "0.90" = 0.90,
                                    "0.95" = 0.95,
                                    "0.98" = 0.98,
                                    "none" = 0)
            
            maf_thr <- switch(self$options$qcMaf,
                              "0.01" = 0.01,
                              "0.05" = 0.05,
                              "0.10" = 0.10,
                              "none" = 0)
            
            hwe_thr <- switch(self$options$qcHwe,
                              "0.05" = 0.05,
                              "0.01" = 0.01,
                              "0.001" = 0.001,
                              "0.0001" = 0.0001,
                              "none" = 0)
            
            # Find controls if binary outcome
            has_outcome <- !is.null(self$options$outcome)
            outcome_var <- if (has_outcome) private$g_data[[self$options$outcome]] else NULL
            is_binary <- FALSE
            controls_idx <- seq_along(private$g_classified[[self$options$vars[1]]])
            
            if (has_outcome) {
                is_binary <- is.factor(outcome_var) || (is.numeric(outcome_var) && length(unique(na.omit(outcome_var))) == 2)
                if (is_binary) {
                    levels_out <- levels(factor(outcome_var))
                    control_level <- levels_out[1]
                    controls_idx <- which(outcome_var == control_level)
                }
            }
            
            qcTable <- self$results$qcGroup$qcTable
            show_qc <- (call_rate_thr > 0 || maf_thr > 0 || hwe_thr > 0)
            qcTable$setVisible(show_qc)
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                total_n <- length(classified)
                non_na <- na.omit(classified)
                non_na <- non_na[non_na != ""]
                
                # Call rate
                call_rate <- if (total_n > 0) length(non_na) / total_n else 0
                
                # MAF
                obsAA <- sum(non_na == "AA")
                obsAB <- sum(non_na == "AB")
                obsBB <- sum(non_na == "BB")
                
                is_x_linked <- isTRUE(self$options$xLinked) && !is.null(self$options$gender)
                if (is_x_linked) {
                    gender_var <- private$g_data[[self$options$gender]]
                    gender_levels <- levels(gender_var)
                    female_idx_g <- grep("f|w|жен|2|girl|woman", gender_levels, ignore.case=TRUE)
                    if (length(female_idx_g) > 0) {
                        female_level <- gender_levels[female_idx_g[1]]
                        male_level <- gender_levels[-female_idx_g[1]][1]
                    } else {
                        female_level <- gender_levels[1]
                        male_level <- if (length(gender_levels) > 1) gender_levels[2] else NULL
                    }
                    male_idx <- which(gender_var == male_level)
                    female_idx <- which(gender_var == female_level)
                    
                    obsA <- sum(classified[male_idx] == "AA" | classified[male_idx] == "A", na.rm=TRUE)
                    obsB <- sum(classified[male_idx] == "BB" | classified[male_idx] == "B", na.rm=TRUE)
                    obsAA <- sum(classified[female_idx] == "AA", na.rm=TRUE)
                    obsAB <- sum(classified[female_idx] == "AB", na.rm=TRUE)
                    obsBB <- sum(classified[female_idx] == "BB", na.rm=TRUE)
                    
                    nA <- 2 * obsAA + obsAB + obsA
                    total_alleles <- 2 * length(na.omit(classified[female_idx])) + length(na.omit(classified[male_idx]))
                } else {
                    nA <- 2 * obsAA + obsAB
                    total_alleles <- 2 * length(non_na)
                }
                
                p <- if (total_alleles > 0) nA / total_alleles else 0
                q <- 1 - p
                maf <- min(p, q)
                
                # HWE p-value in controls or overall
                hwe_p <- private$.getHWEpValForQC(marker, controls_idx)
                
                # Exclusions
                status <- "Passed"
                if (call_rate < call_rate_thr) {
                    status <- paste0("Excluded: Call Rate < ", call_rate_thr)
                } else if (maf < maf_thr) {
                    status <- paste0("Excluded: MAF < ", maf_thr)
                } else if (hwe_p < hwe_thr) {
                    status <- paste0("Excluded: HWE p < ", hwe_thr)
                }
                
                private$g_qc_status[[marker]] <- status
                
                if (show_qc) {
                    row_val <- list(
                        marker = marker,
                        call_rate = call_rate,
                        maf = maf,
                        hwe_p = hwe_p,
                        status = status
                    )
                    qcTable$addRow(rowKey = marker, value = row_val)
                }
            }
        },
        
        .getHWEpValForQC = function(marker, indices) {
            classified <- private$g_classified[[marker]]
            sub_class <- classified[indices]
            sub_class <- na.omit(sub_class)
            sub_class <- sub_class[sub_class != ""]
            N <- length(sub_class)
            if (N == 0) return(1.0)
            
            is_x_linked <- isTRUE(self$options$xLinked) && !is.null(self$options$gender)
            if (is_x_linked) {
                gender_var <- private$g_data[[self$options$gender]][indices]
                gender_levels <- levels(gender_var)
                female_idx_g <- grep("f|w|жен|2|girl|woman", gender_levels, ignore.case=TRUE)
                if (length(female_idx_g) > 0) {
                    female_level <- gender_levels[female_idx_g[1]]
                    male_level <- gender_levels[-female_idx_g[1]][1]
                } else {
                    female_level <- gender_levels[1]
                    male_level <- if (length(gender_levels) > 1) gender_levels[2] else NULL
                }
                
                male_idx <- which(gender_var == male_level)
                female_idx <- which(gender_var == female_level)
                
                obsA <- sum(sub_class[male_idx] == "AA" | sub_class[male_idx] == "A", na.rm=TRUE)
                obsB <- sum(sub_class[male_idx] == "BB" | sub_class[male_idx] == "B", na.rm=TRUE)
                obsAA <- sum(sub_class[female_idx] == "AA", na.rm=TRUE)
                obsAB <- sum(sub_class[female_idx] == "AB", na.rm=TRUE)
                obsBB <- sum(sub_class[female_idx] == "BB", na.rm=TRUE)
                
                counts_vec <- c(A = obsA, B = obsB, AA = obsAA, AB = obsAB, BB = obsBB)
                res <- tryCatch(HardyWeinberg::HWExactSex(counts_vec, verbose=FALSE), error = function(e) list(pval=1.0))
                return(if (is.list(res)) res$pval else res)
            } else {
                obsAA <- sum(sub_class == "AA")
                obsAB <- sum(sub_class == "AB")
                obsBB <- sum(sub_class == "BB")
                counts_vec <- c(AA = obsAA, AB = obsAB, BB = obsBB)
                res <- tryCatch(HardyWeinberg::HWExact(counts_vec, verbose=FALSE), error = function(e) list(pval=1.0))
                return(if (is.list(res)) res$pval else res)
            }
        },
        
        .fillFreqAndHWETables = function() {
            freqTable <- self$results$hweGroup$freqTable
            hwTable <- self$results$hweGroup$hwTable
            private$g_hwe_results <- list()
            
            # Dynamic group subsets (supports splitting by outcome)
            has_group <- !is.null(self$options$group)
            has_outcome <- !is.null(self$options$outcome) && !is.numeric(private$g_data[[self$options$outcome]]) && length(levels(factor(private$g_data[[self$options$outcome]]))) >= 2
            split_hwe <- isTRUE(self$options$hweSplitOutcome) && has_outcome
            
            subsets <- list()
            if (has_group && split_hwe) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (gl in grp_levels) {
                    for (ol in out_levels) {
                        subsets[[paste0(gl, " - ", ol)]] <- which(private$g_data[[self$options$group]] == gl & private$g_data[[self$options$outcome]] == ol)
                    }
                }
            } else if (has_group) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                for (gl in grp_levels) {
                    subsets[[gl]] <- which(private$g_data[[self$options$group]] == gl)
                }
            } else if (split_hwe) {
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (ol in out_levels) {
                    subsets[[ol]] <- which(private$g_data[[self$options$outcome]] == ol)
                }
            } else {
                subsets[["Total"]] <- seq_along(private$g_classified[[self$options$vars[1]]])
            }
            
            # Temporary storage to compute multiple correction
            raw_p_chi <- numeric()
            raw_p_exact <- numeric()
            raw_p_lr <- numeric()
            raw_p_perm <- numeric()
            keys <- list()
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                for (g in names(subsets)) {
                    subset_idx <- subsets[[g]]
                    sub_class <- classified[subset_idx]
                    sub_class <- na.omit(sub_class)
                    sub_class <- sub_class[sub_class != ""]
                    
                    is_x_linked <- isTRUE(self$options$xLinked) && !is.null(self$options$gender)
                    
                    if (is_x_linked) {
                        gender_var <- private$g_data[[self$options$gender]][subset_idx]
                        gender_levels <- levels(gender_var)
                        female_idx_g <- grep("f|w|жен|2|girl|woman", gender_levels, ignore.case=TRUE)
                        if (length(female_idx_g) > 0) {
                            female_level <- gender_levels[female_idx_g[1]]
                            male_level <- gender_levels[-female_idx_g[1]][1]
                        } else {
                            female_level <- gender_levels[1]
                            male_level <- if (length(gender_levels) > 1) gender_levels[2] else NULL
                        }
                        
                        male_idx <- which(gender_var == male_level)
                        female_idx <- which(gender_var == female_level)
                        
                        male_geno <- sub_class[male_idx]
                        female_geno <- sub_class[female_idx]
                        
                        obsA <- sum(male_geno == "AA" | male_geno == "A", na.rm=TRUE)
                        obsB <- sum(male_geno == "BB" | male_geno == "B", na.rm=TRUE)
                        obsAA <- sum(female_geno == "AA", na.rm=TRUE)
                        obsAB <- sum(female_geno == "AB", na.rm=TRUE)
                        obsBB <- sum(female_geno == "BB", na.rm=TRUE)
                        
                        N_females <- length(na.omit(female_geno))
                        N_males <- length(na.omit(male_geno))
                        N <- N_females + N_males
                        total_alleles <- 2 * N_females + N_males
                        
                        nA <- 2 * obsAA + obsAB + obsA
                        nB <- 2 * obsBB + obsAB + obsB
                        
                        p_freq <- if (total_alleles > 0) nA / total_alleles else 0
                        q_freq <- 1 - p_freq
                        
                        expAA <- (p_freq^2) * N_females
                        expAB <- 2 * p_freq * q_freq * N_females
                        expBB <- (q_freq^2) * N_females
                        
                        h_obs <- if (N_females > 0) obsAB / N_females else 0
                        h_exp <- 2 * p_freq * q_freq
                    } else {
                        obsAA <- sum(sub_class == "AA")
                        obsAB <- sum(sub_class == "AB")
                        obsBB <- sum(sub_class == "BB")
                        N <- length(sub_class)
                        nA <- 2 * obsAA + obsAB
                        nB <- 2 * obsBB + obsAB
                        total_alleles <- 2 * N
                        
                        p_freq <- if (total_alleles > 0) nA / total_alleles else 0
                        q_freq <- 1 - p_freq
                        
                        expAA <- (p_freq^2) * N
                        expAB <- 2 * p_freq * q_freq * N
                        expBB <- (q_freq^2) * N
                        
                        h_obs <- if (N > 0) obsAB / N else 0
                        h_exp <- 2 * p_freq * q_freq
                    }
                    
                    row_key <- paste(marker, g, sep="_")
                    row_val <- list(
                        marker = marker,
                        group = g,
                        p_freq = p_freq,
                        q_freq = q_freq,
                        obsAA = if (is_x_linked) paste0(obsAA, " F / ", obsA, " M") else as.character(obsAA),
                        obsAB = if (is_x_linked) paste0(obsAB, " F") else as.character(obsAB),
                        obsBB = if (is_x_linked) paste0(obsBB, " F / ", obsB, " M") else as.character(obsBB),
                        expAA = if (is_x_linked) paste0(format(round(expAA, 1), nsmall=1), " F") else format(round(expAA, 1), nsmall=1),
                        expAB = if (is_x_linked) paste0(format(round(expAB, 1), nsmall=1), " F") else format(round(expAB, 1), nsmall=1),
                        expBB = if (is_x_linked) paste0(format(round(expBB, 1), nsmall=1), " F") else format(round(expBB, 1), nsmall=1),
                        raw_obsAA = obsAA,
                        raw_obsAB = obsAB,
                        raw_obsBB = obsBB,
                        raw_expAA = expAA,
                        raw_expAB = expAB,
                        raw_expBB = expBB,
                        h_obs = h_obs,
                        h_exp = h_exp
                    )
                    
                    if (isTRUE(self$options$freqPct)) {
                        if (is_x_linked) {
                            obsAA_pct <- if (N_females > 0) (obsAA / N_females * 100) else 0
                            obsAB_pct <- if (N_females > 0) (obsAB / N_females * 100) else 0
                            obsBB_pct <- if (N_females > 0) (obsBB / N_females * 100) else 0
                            
                            expAA_pct <- if (N_females > 0) (expAA / N_females * 100) else 0
                            expAB_pct <- if (N_females > 0) (expAB / N_females * 100) else 0
                            expBB_pct <- if (N_females > 0) (expBB / N_females * 100) else 0
                            
                            table_val <- list(
                                marker = marker,
                                group = g,
                                p_freq = p_freq * 100,
                                q_freq = q_freq * 100,
                                obsAA = paste0(obsAA, " F (", format(round(obsAA_pct, 1), nsmall = 1), "%), ", obsA, " M (A)"),
                                obsAB = paste0(obsAB, " F (", format(round(obsAB_pct, 1), nsmall = 1), "%)"),
                                obsBB = paste0(obsBB, " F (", format(round(obsBB_pct, 1), nsmall = 1), "%), ", obsB, " M (B)"),
                                expAA = paste0(format(round(expAA, 1), nsmall = 1), " F (", format(round(expAA_pct, 1), nsmall = 1), "%)"),
                                expAB = paste0(format(round(expAB, 1), nsmall = 1), " F (", format(round(expAB_pct, 1), nsmall = 1), "%)"),
                                expBB = paste0(format(round(expBB, 1), nsmall = 1), " F (", format(round(expBB_pct, 1), nsmall = 1), "%)"),
                                h_obs = h_obs * 100,
                                h_exp = h_exp * 100
                            )
                        } else {
                            obsAA_pct <- if (N > 0) (obsAA / N * 100) else 0
                            obsAB_pct <- if (N > 0) (obsAB / N * 100) else 0
                            obsBB_pct <- if (N > 0) (obsBB / N * 100) else 0
                            
                            expAA_pct <- if (N > 0) (expAA / N * 100) else 0
                            expAB_pct <- if (N > 0) (expAB / N * 100) else 0
                            expBB_pct <- if (N > 0) (expBB / N * 100) else 0
                            
                            table_val <- list(
                                marker = marker,
                                group = g,
                                p_freq = p_freq * 100,
                                q_freq = q_freq * 100,
                                obsAA = paste0(obsAA, " (", format(round(obsAA_pct, 1), nsmall = 1), "%)"),
                                obsAB = paste0(obsAB, " (", format(round(obsAB_pct, 1), nsmall = 1), "%)"),
                                obsBB = paste0(obsBB, " (", format(round(obsBB_pct, 1), nsmall = 1), "%)"),
                                expAA = paste0(format(round(expAA, 1), nsmall = 1), " (", format(round(expAA_pct, 1), nsmall = 1), "%)"),
                                expAB = paste0(format(round(expAB, 1), nsmall = 1), " (", format(round(expAB_pct, 1), nsmall = 1), "%)"),
                                expBB = paste0(format(round(expBB, 1), nsmall = 1), " (", format(round(expBB_pct, 1), nsmall = 1), "%)"),
                                h_obs = h_obs * 100,
                                h_exp = h_exp * 100
                            )
                        }
                    } else {
                        table_val <- list(
                            marker = marker,
                            group = g,
                            p_freq = p_freq,
                            q_freq = q_freq,
                            obsAA = row_val$obsAA,
                            obsAB = row_val$obsAB,
                            obsBB = row_val$obsBB,
                            expAA = row_val$expAA,
                            expAB = row_val$expAB,
                            expBB = row_val$expBB,
                            h_obs = h_obs,
                            h_exp = h_exp
                        )
                    }
                    
                    freqTable$addRow(rowKey = row_key, value = table_val)
                    private$g_hwe_results[[row_key]] <- row_val
                    
                    if (self$options$hwTests) {
                        counts_vec <- if (is_x_linked) c(A = obsA, B = obsB, AA = obsAA, AB = obsAB, BB = obsBB) else c(AA = obsAA, AB = obsAB, BB = obsBB)
                        
                        fis <- if (h_exp > 0) 1 - (h_obs / h_exp) else 0
                        
                        chi_stat <- NA
                        chi_p <- NA
                        if (N > 0) {
                            res <- tryCatch(HardyWeinberg::HWChisq(counts_vec, x.linked = is_x_linked, cc=0, verbose=FALSE), error = function(e) list(chisq=NA, pval=NA))
                            chi_stat <- res$chisq
                            chi_p <- res$pval
                        }
                        
                        exact_p <- NA
                        if (N > 0) {
                            exact_p <- if (is_x_linked) {
                                tryCatch(HardyWeinberg::HWExactSex(counts_vec, verbose=FALSE)$pval, error = function(e) NA)
                            } else {
                                tryCatch(HardyWeinberg::HWExact(counts_vec, verbose=FALSE)$pval, error = function(e) NA)
                            }
                        }
                        
                        lr_stat <- NA
                        lr_p <- NA
                        if (N > 0) {
                            res_lr <- tryCatch(HardyWeinberg::HWLratio(counts_vec, x.linked = is_x_linked, verbose=FALSE), error = function(e) list(G2=NA, pval=NA))
                            lr_stat <- res_lr$G2
                            lr_p <- res_lr$pval
                        }
                        
                        perm_p <- NA
                        if (self$options$hwPerm && N > 0) {
                            perm_p <- tryCatch(HardyWeinberg::HWPerm(counts_vec, x.linked = is_x_linked, nperm=as.integer(self$options$nPerm), verbose=FALSE)$pval, error = function(e) NA)
                        }
                        
                        raw_p_chi <- c(raw_p_chi, chi_p)
                        raw_p_exact <- c(raw_p_exact, exact_p)
                        raw_p_lr <- c(raw_p_lr, lr_p)
                        if (self$options$hwPerm) raw_p_perm <- c(raw_p_perm, perm_p)
                        
                        keys[[length(keys) + 1]] <- list(row_key = row_key, marker = marker, group = g, chi_stat = chi_stat, lr_stat = lr_stat, fis = fis)
                    }
                }
            }
            
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
                    
                    # Highlight if marker failed QC
                    marker_status <- private$g_qc_status[[k$marker]]
                    if (!is.null(marker_status) && marker_status != "Passed") {
                        hwTable$addFootnote(rowKey = k$row_key, col = "marker", paste0(.("Failed QC"), " (", marker_status, ")"))
                    }
                }
            }
        },
        
        .fillAssociationTable = function() {
            assocTable <- self$results$assocGroup$assocTable
            private$g_assoc_results <- list()
            
            outcome_col <- self$options$outcome
            outcome_var <- private$g_data[[outcome_col]]
            
            # Detect outcome type
            is_continuous <- FALSE
            if (is.numeric(outcome_var)) {
                if (length(unique(na.omit(outcome_var))) > 2) {
                    is_continuous <- TRUE
                }
            }
            
            # Setup columns dynamically
            if (is_continuous) {
                assocTable$getColumn("or_val")$setTitle(.("Beta"))
                assocTable$getColumn("or_lower")$setTitle(.("Beta Lower 95% CI"))
                assocTable$getColumn("or_upper")$setTitle(.("Beta Upper 95% CI"))
                assocTable$getColumn("counts_cases")$setTitle(.("Genotype Means (Mean ± SD)"))
                assocTable$getColumn("counts_ctrls")$setTitle(.("Genotype N"))
            } else {
                assocTable$getColumn("or_val")$setTitle(.("Odds Ratio"))
                assocTable$getColumn("or_lower")$setTitle(.("OR Lower 95% CI"))
                assocTable$getColumn("or_upper")$setTitle(.("OR Upper 95% CI"))
                assocTable$getColumn("counts_cases")$setTitle(.("Genotypes Cases"))
                assocTable$getColumn("counts_ctrls")$setTitle(.("Genotypes Controls"))
            }
            
            if (is_continuous) {
                # Continuous outcome does not use Case/Control mapping
                outcome_rec <- outcome_var
            } else {
                # Clean outcome to Case/Control binary
                levels_out <- levels(factor(outcome_var))
                if (length(levels_out) < 2) {
                    # We need case/control binary outcome
                    return()
                }
                case_level <- levels_out[2]
                control_level <- levels_out[1]
                
                # Recode outcome
                outcome_rec <- rep(NA, length(outcome_var))
                outcome_rec[outcome_var == case_level] <- "Case"
                outcome_rec[outcome_var == control_level] <- "Control"
            }
            
            # QC check to exclude variables
            analyzed_vars <- self$options$vars
            call_rate_thr <- switch(self$options$qcCallRate, "0.90" = 0.90, "0.95" = 0.95, "0.98" = 0.98, "none" = 0)
            maf_thr <- switch(self$options$qcMaf, "0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10, "none" = 0)
            hwe_thr <- switch(self$options$qcHwe, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001, "0.0001" = 0.0001, "none" = 0)
            if (call_rate_thr > 0 || maf_thr > 0 || hwe_thr > 0) {
                analyzed_vars <- Filter(function(m) private$g_qc_status[[m]] == "Passed", self$options$vars)
            }
            
            raw_p_vals <- numeric()
            rows_data <- list()
            
            # Select models to calculate
            if (is_continuous) {
                model_list <- c("Codominant (AB vs AA)", "Codominant (BB vs AA)", "Dominant", "Recessive", "Overdominant", "Log-additive")
                if (self$options$geneticModel != "all") {
                    model_list <- switch(self$options$geneticModel,
                                       "codominant" = c("Codominant (AB vs AA)", "Codominant (BB vs AA)"),
                                       "dominant" = "Dominant",
                                       "recessive" = "Recessive",
                                       "overdominant" = "Overdominant",
                                       "log-additive" = "Log-additive")
                }
            } else {
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
            }
            
            has_covs <- !is.null(self$options$covs) && length(self$options$covs) > 0
            cov_names <- self$options$covs
            
            for (marker in analyzed_vars) {
                classified <- private$g_classified[[marker]]
                
                # Get non-NA pairs
                complete_cases <- !is.na(classified) & !is.na(outcome_rec)
                if (has_covs) {
                    for (cv in cov_names) {
                        complete_cases <- complete_cases & !is.na(private$g_data[[cv]])
                    }
                }
                
                sub_class <- classified[complete_cases]
                sub_out <- outcome_rec[complete_cases]
                
                if (length(sub_class) == 0) next
                
                major <- private$g_alleles[[marker]]$major
                minor <- private$g_alleles[[marker]]$minor
                
                if (is_continuous) {
                    # Genotype means and counts
                    vals_AA <- sub_out[sub_class == "AA"]
                    vals_AB <- sub_out[sub_class == "AB"]
                    vals_BB <- sub_out[sub_class == "BB"]
                    
                    fmt_val <- function(m, s) {
                        if (is.nan(m) || is.na(m)) "NA"
                        else paste0(format(round(m, 2), nsmall = 2), " ± ", format(round(s, 2), nsmall = 2))
                    }
                    
                    counts_cases <- paste0(major, major, ": ", fmt_val(mean(vals_AA, na.rm=TRUE), sd(vals_AA, na.rm=TRUE)), ", ", major, minor, ": ", fmt_val(mean(vals_AB, na.rm=TRUE), sd(vals_AB, na.rm=TRUE)), ", ", minor, minor, ": ", fmt_val(mean(vals_BB, na.rm=TRUE), sd(vals_BB, na.rm=TRUE)))
                    counts_ctrls <- paste0(major, major, ": ", length(vals_AA), ", ", major, minor, ": ", length(vals_AB), ", ", minor, minor, ": ", length(vals_BB))
                } else {
                    # Binary outcome counts
                    cases_AA <- sum(sub_class == "AA" & sub_out == "Case")
                    cases_AB <- sum(sub_class == "AB" & sub_out == "Case")
                    cases_BB <- sum(sub_class == "BB" & sub_out == "Case")
                    ctrls_AA <- sum(sub_class == "AA" & sub_out == "Control")
                    ctrls_AB <- sum(sub_class == "AB" & sub_out == "Control")
                    ctrls_BB <- sum(sub_class == "BB" & sub_out == "Control")
                    
                    counts_cases <- paste0(major, major, ":", cases_AA, ", ", major, minor, ":", cases_AB, ", ", minor, minor, ":", cases_BB)
                    counts_ctrls <- paste0(major, major, ":", ctrls_AA, ", ", major, minor, ":", ctrls_AB, ", ", minor, minor, ":", ctrls_BB)
                }
                
                # Build modeling data frame
                model_df <- data.frame(y = sub_out)
                if (has_covs) {
                    for (cv in cov_names) {
                        model_df[[cv]] <- private$g_data[[cv]][complete_cases]
                    }
                }
                
                for (model in model_list) {
                    or_val <- NA
                    or_lower <- NA
                    or_upper <- NA
                    p_val <- NA
                    
                    if (model == "Allelic" && !is_continuous) {
                        # Allelic test (2x2) - unadjusted
                        a <- 2 * cases_AA + cases_AB
                        b <- 2 * cases_BB + cases_AB
                        c <- 2 * ctrls_AA + ctrls_AB
                        d <- 2 * ctrls_BB + ctrls_AB
                        
                        tbl2x2 <- matrix(c(a, b, c, d), nrow=2, byrow=TRUE)
                        if (sum(tbl2x2) > 0) {
                            res_test <- tryCatch(stats::chisq.test(tbl2x2, correct=FALSE), error = function(e) list(p.value=NA))
                            p_val <- res_test$p.value
                            if (b * c > 0) {
                                or_val <- (a * d) / (b * c)
                                log_or <- log(or_val)
                                se_log_or <- sqrt(1/max(1, a) + 1/max(1, b) + 1/max(1, c) + 1/max(1, d))
                                or_lower <- exp(log_or - 1.96 * se_log_or)
                                or_upper <- exp(log_or + 1.96 * se_log_or)
                            }
                        }
                    } else if (model == "Genotypic" && !is_continuous) {
                        # Genotypic test (3x2) - unadjusted
                        tbl3x2 <- matrix(c(cases_AA, ctrls_AA,
                                           cases_AB, ctrls_AB,
                                           cases_BB, ctrls_BB), nrow=3, byrow=TRUE)
                        if (sum(tbl3x2) > 0) {
                            res_test <- tryCatch(stats::chisq.test(tbl3x2, correct=FALSE), error = function(e) list(p.value=NA))
                            p_val <- res_test$p.value
                        }
                    } else {
                        # Fit model (logistic or linear)
                        if (model == "Codominant (AB vs AA)") {
                            model_df$x <- factor(sub_class, levels=c("AA", "AB", "BB"))
                        } else if (model == "Codominant (BB vs AA)") {
                            model_df$x <- factor(sub_class, levels=c("AA", "AB", "BB"))
                        } else if (model == "Dominant") {
                            model_df$x <- ifelse(sub_class == "AA", 0, 1)
                        } else if (model == "Recessive") {
                            model_df$x <- ifelse(sub_class == "BB", 1, 0)
                        } else if (model == "Overdominant") {
                            model_df$x <- ifelse(sub_class == "AB", 1, 0)
                        } else if (model == "Log-additive") {
                            model_df$x <- ifelse(sub_class == "AA", 0, ifelse(sub_class == "AB", 1, 2))
                        }
                        
                        formula_str <- "y ~ x"
                        if (has_covs) {
                            formula_str <- paste(formula_str, paste(cov_names, collapse=" + "), sep=" + ")
                        }
                        formula_obj <- as.formula(formula_str)
                        
                        if (is_continuous) {
                            res_fit <- tryCatch(lm(formula_obj, data=model_df), error=function(e) NULL)
                        } else {
                            model_df$y <- as.factor(model_df$y)
                            res_fit <- tryCatch(glm(formula_obj, data=model_df, family=binomial), error=function(e) NULL)
                        }
                        
                        if (!is.null(res_fit)) {
                            sum_fit <- summary(res_fit)
                            coefs <- sum_fit$coefficients
                            
                            target_coef <- "x"
                            if (model == "Codominant (AB vs AA)") {
                                target_coef <- "xAB"
                            } else if (model == "Codominant (BB vs AA)") {
                                target_coef <- "xBB"
                            }
                            
                            if (target_coef %in% rownames(coefs)) {
                                p_val <- coefs[target_coef, 4]
                                beta_est <- coefs[target_coef, 1]
                                ci <- tryCatch(suppressMessages(confint.default(res_fit, target_coef, level=0.95)), error=function(e) c(NA, NA))
                                
                                if (is_continuous) {
                                    or_val <- beta_est
                                    or_lower <- ci[1]
                                    or_upper <- ci[2]
                                } else {
                                    or_val <- exp(beta_est)
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
            
            if (has_covs && !is_continuous) {
                assocTable$setNote('cov_note', .("Note: Allelic and Genotypic models are unadjusted. Logistic regression is fitted for other models."))
            } else if (has_covs && is_continuous) {
                assocTable$setNote('cov_note', .("Note: Linear regression is adjusted for selected covariates."))
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
            
            # QC check to exclude variables
            analyzed_vars <- self$options$vars
            call_rate_thr <- switch(self$options$qcCallRate, "0.90" = 0.90, "0.95" = 0.95, "0.98" = 0.98, "none" = 0)
            maf_qc_thr <- switch(self$options$qcMaf, "0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10, "none" = 0)
            hwe_thr <- switch(self$options$qcHwe, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001, "0.0001" = 0.0001, "none" = 0)
            if (call_rate_thr > 0 || maf_qc_thr > 0 || hwe_thr > 0) {
                analyzed_vars <- Filter(function(m) private$g_qc_status[[m]] == "Passed", self$options$vars)
            }
            
            for (marker in analyzed_vars) {
                classified <- private$g_classified[[marker]]
                non_na <- na.omit(classified)
                N <- length(non_na)
                if (N == 0) next
                
                obsAA <- sum(non_na == "AA")
                obsAB <- sum(non_na == "AB")
                obsBB <- sum(non_na == "BB")
                
                is_x_linked <- isTRUE(self$options$xLinked) && !is.null(self$options$gender)
                if (is_x_linked) {
                    gender_var <- private$g_data[[self$options$gender]]
                    gender_levels <- levels(gender_var)
                    female_idx_g <- grep("f|w|жен|2|girl|woman", gender_levels, ignore.case=TRUE)
                    if (length(female_idx_g) > 0) {
                        female_level <- gender_levels[female_idx_g[1]]
                        male_level <- gender_levels[-female_idx_g[1]][1]
                    } else {
                        female_level <- gender_levels[1]
                        male_level <- if (length(gender_levels) > 1) gender_levels[2] else NULL
                    }
                    
                    male_idx <- which(gender_var == male_level)
                    female_idx <- which(gender_var == female_level)
                    
                    obsA <- sum(classified[male_idx] == "AA" | classified[male_idx] == "A", na.rm=TRUE)
                    obsB <- sum(classified[male_idx] == "BB" | classified[male_idx] == "B", na.rm=TRUE)
                    obsAA <- sum(classified[female_idx] == "AA", na.rm=TRUE)
                    obsAB <- sum(classified[female_idx] == "AB", na.rm=TRUE)
                    obsBB <- sum(classified[female_idx] == "BB", na.rm=TRUE)
                    
                    nA <- 2 * obsAA + obsAB + obsA
                    total_alleles <- 2 * length(na.omit(classified[female_idx])) + length(na.omit(classified[male_idx]))
                } else {
                    nA <- 2 * obsAA + obsAB
                    total_alleles <- 2 * N
                }
                
                p <- if (total_alleles > 0) nA / total_alleles else 0
                q <- 1 - p
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
            call_rate_thr <- switch(self$options$qcCallRate, "0.90" = 0.90, "0.95" = 0.95, "0.98" = 0.98, "none" = 0)
            maf_thr <- switch(self$options$qcMaf, "0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10, "none" = 0)
            hwe_thr <- switch(self$options$qcHwe, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001, "0.0001" = 0.0001, "none" = 0)
            if (call_rate_thr > 0 || maf_thr > 0 || hwe_thr > 0) {
                snps <- Filter(function(m) private$g_qc_status[[m]] == "Passed", snps)
            }
            if (length(snps) == 0) return()
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
            call_rate_thr <- switch(self$options$qcCallRate, "0.90" = 0.90, "0.95" = 0.95, "0.98" = 0.98, "none" = 0)
            maf_thr <- switch(self$options$qcMaf, "0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10, "none" = 0)
            hwe_thr <- switch(self$options$qcHwe, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001, "0.0001" = 0.0001, "none" = 0)
            if (call_rate_thr > 0 || maf_thr > 0 || hwe_thr > 0) {
                snps <- Filter(function(m) private$g_qc_status[[m]] == "Passed", snps)
            }
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
        
        .fillGroupTable = function() {
            groupTable <- self$results$hweGroup$groupAnalysisTable
            group_var_name <- self$options$group
            groups <- levels(private$g_data[[group_var_name]])
            if (is.null(groups) || length(groups) == 0) {
                groups <- levels(factor(private$g_data[[group_var_name]]))
            }
            
            run_chisq_test <- function(tab) {
                if (ncol(tab) < 2 || nrow(tab) < 2) return(list(chisq=NA, df=NA, p=NA))
                if (sum(tab) == 0) return(list(chisq=NA, df=NA, p=NA))
                col_sums <- colSums(tab)
                tab <- tab[, col_sums > 0, drop = FALSE]
                if (ncol(tab) < 2) return(list(chisq=NA, df=NA, p=NA))
                
                res <- tryCatch({
                    test_res <- stats::chisq.test(tab, correct = FALSE)
                    list(chisq = test_res$statistic, df = test_res$parameter, p = test_res$p.value)
                }, error = function(e) {
                    list(chisq = NA, df = NA, p = NA)
                })
                return(res)
            }
            
            formatChi2 <- function(chi_sq, df, p) {
                if (is.na(chi_sq) || is.na(df) || is.na(p)) {
                    return("–")
                }
                p_str <- if (p < 0.001) "p < 0.001" else paste0("p = ", format(round(p, 3), nsmall = 3))
                paste0("df = ", df, ", χ² = ", format(round(chi_sq, 2), nsmall = 2), ", ", p_str)
            }
            
            for (marker in self$options$vars) {
                major <- private$g_alleles[[marker]]$major
                minor <- private$g_alleles[[marker]]$minor
                
                g_labels <- list(
                    AA = paste0(major, major),
                    AB = paste0(major, minor),
                    BB = paste0(minor, minor)
                )
                
                sub_class <- private$g_classified[[marker]]
                grp_var <- private$g_data[[group_var_name]]
                
                # Filter NA
                valid_idx <- !is.na(sub_class) & !is.na(grp_var) & grp_var != ""
                sub_class_val <- sub_class[valid_idx]
                grp_var_val <- grp_var[valid_idx]
                
                # Count table
                genotypes <- c("AA", "AB", "BB")
                counts <- matrix(0, nrow = 3, ncol = length(groups), dimnames = list(genotypes, groups))
                for (g in groups) {
                    g_idx <- which(grp_var_val == g)
                    for (gt in genotypes) {
                        counts[gt, g] <- sum(sub_class_val[g_idx] == gt)
                    }
                }
                
                # Column totals for percentages
                col_totals <- colSums(counts)
                
                # Overall 3xG test for polymorphism
                test_poly <- run_chisq_test(counts)
                if (isTRUE(self$options$polyDf1)) {
                    if (!is.na(test_poly$df)) {
                        test_poly$df <- test_poly$df / 2
                    }
                    if (!is.na(test_poly$chisq) && !is.na(test_poly$df)) {
                        test_poly$p <- stats::pchisq(test_poly$chisq, df = test_poly$df, lower.tail = FALSE)
                    }
                }
                
                # Allele counts matrix (2 x G)
                # Row 1: major allele, Row 2: minor allele
                allele_counts <- rbind(
                    major = counts["AA", ] * 2 + counts["AB", ],
                    minor = counts["BB", ] * 2 + counts["AB", ]
                )
                
                # Overall 2xG test for alleles
                test_allele <- run_chisq_test(allele_counts)
                
                for (gt in genotypes) {
                    row_key <- paste(marker, gt, sep="_")
                    row_val <- list(
                        marker = marker,
                        genotype = g_labels[[gt]]
                    )
                    
                    # Group columns
                    for (g in groups) {
                        cnt <- counts[gt, g]
                        tot <- col_totals[g]
                        
                        if (isTRUE(self$options$freqPct)) {
                            pct_val <- if (tot > 0) (cnt / tot * 100) else 0
                            pct_str <- format(round(pct_val, 1), nsmall = 1)
                            row_val[[paste0("grp_", g)]] <- paste0(cnt, " (", pct_str, "%)")
                        } else {
                            row_val[[paste0("grp_", g)]] <- as.character(cnt)
                        }
                    }
                    
                    # 2xG test for genotype gt vs others
                    row1 <- counts[gt, ]
                    row2 <- colSums(counts[genotypes != gt, , drop = FALSE])
                    table_2xG <- rbind(row1, row2)
                    
                    test_gt <- run_chisq_test(table_2xG)
                    row_val$genotype_chi <- test_gt$chisq
                    row_val$genotype_p <- test_gt$p
                    
                    row_val$poly_chi <- test_poly$chisq
                    row_val$poly_p <- test_poly$p
                    
                    groupTable$addRow(rowKey = row_key, value = row_val)
                }
                
                # Add allele rows: major (A) and minor (B)
                if (isTRUE(self$options$groupAlleles)) {
                    alleles_list <- c("major", "minor")
                    allele_labels <- list(major = major, minor = minor)
                    
                    for (al in alleles_list) {
                        row_key <- paste(marker, "allele", al, sep="_")
                        row_val <- list(
                            marker = marker,
                            genotype = allele_labels[[al]]
                        )
                        
                        # Group columns for alleles
                        for (g in groups) {
                            cnt <- allele_counts[al, g]
                            # Total alleles in the group is 2 * total individuals (col_totals[g])
                            tot_al <- 2 * col_totals[g]
                            
                            if (isTRUE(self$options$freqPct)) {
                                pct_val <- if (tot_al > 0) (cnt / tot_al * 100) else 0
                                pct_str <- format(round(pct_val, 1), nsmall = 1)
                                row_val[[paste0("grp_", g)]] <- paste0(cnt, " (", pct_str, "%)")
                            } else {
                                row_val[[paste0("grp_", g)]] <- as.character(cnt)
                            }
                        }
                        
                        # Write allelic test results into genotype_chi and genotype_p columns
                        row_val$genotype_chi <- test_allele$chisq
                        row_val$genotype_p <- test_allele$p
                        
                        # Polymorphism test columns are empty (NA) for allele rows
                        row_val$poly_chi <- NA
                        row_val$poly_p <- NA
                        
                        groupTable$addRow(rowKey = row_key, value = row_val)
                    }
                }
                
                # Add total row: Сумма/Всего
                if (isTRUE(self$options$groupTotal)) {
                    row_key <- paste(marker, "total", sep="_")
                    row_val <- list(
                        marker = marker,
                        genotype = .("Total")
                    )
                    
                    # Group columns for total
                    for (g in groups) {
                        cnt <- col_totals[g]
                        if (isTRUE(self$options$freqPct)) {
                            row_val[[paste0("grp_", g)]] <- paste0(cnt, " (100.0%)")
                        } else {
                            row_val[[paste0("grp_", g)]] <- as.character(cnt)
                        }
                    }
                    
                    row_val$genotype_chi <- NA
                    row_val$genotype_p <- NA
                    row_val$poly_chi <- NA
                    row_val$poly_p <- NA
                    
                    groupTable$addRow(rowKey = row_key, value = row_val)
                }
            }
        },
        
        # ------------------ Ploting functions ------------------
        
        .plotTernary = function(image, ...) {
            if (length(self$options$vars) == 0) return(FALSE)
            
            has_group <- !is.null(self$options$group)
            has_outcome <- !is.null(self$options$outcome) && !is.numeric(private$g_data[[self$options$outcome]]) && length(levels(factor(private$g_data[[self$options$outcome]]))) >= 2
            split_hwe <- isTRUE(self$options$hweSplitOutcome) && has_outcome
            
            subsets <- list()
            if (has_group && split_hwe) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (gl in grp_levels) {
                    for (ol in out_levels) {
                        subsets[[paste0(gl, " - ", ol)]] <- which(private$g_data[[self$options$group]] == gl & private$g_data[[self$options$outcome]] == ol)
                    }
                }
            } else if (has_group) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                for (gl in grp_levels) {
                    subsets[[gl]] <- which(private$g_data[[self$options$group]] == gl)
                }
            } else if (split_hwe) {
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (ol in out_levels) {
                    subsets[[ol]] <- which(private$g_data[[self$options$outcome]] == ol)
                }
            } else {
                subsets[["Total"]] <- seq_along(private$g_classified[[self$options$vars[1]]])
            }
            
            groups <- names(subsets)
            has_multi_groups <- (length(groups) > 1)
            
            # Collect counts matrix
            counts_list <- list()
            marker_names <- character()
            group_names <- character()
            
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                for (g in groups) {
                    subset_idx <- subsets[[g]]
                    sub_class <- na.omit(classified[subset_idx])
                    sub_class <- sub_class[sub_class != ""]
                    
                    is_x_linked <- isTRUE(self$options$xLinked) && !is.null(self$options$gender)
                    if (is_x_linked) {
                        gender_var <- private$g_data[[self$options$gender]][subset_idx]
                        gender_levels <- levels(gender_var)
                        female_idx_g <- grep("f|w|жен|2|girl|woman", gender_levels, ignore.case=TRUE)
                        if (length(female_idx_g) > 0) {
                            female_level <- gender_levels[female_idx_g[1]]
                        } else {
                            female_level <- gender_levels[1]
                        }
                        female_idx <- which(gender_var == female_level)
                        female_geno <- sub_class[female_idx]
                        
                        obsAA <- sum(female_geno == "AA", na.rm=TRUE)
                        obsAB <- sum(female_geno == "AB", na.rm=TRUE)
                        obsBB <- sum(female_geno == "BB", na.rm=TRUE)
                    } else {
                        obsAA <- sum(sub_class == "AA")
                        obsAB <- sum(sub_class == "AB")
                        obsBB <- sum(sub_class == "BB")
                    }
                    
                    counts_list[[length(counts_list) + 1]] <- c(AA=obsAA, AB=obsAB, BB=obsBB)
                    marker_names <- c(marker_names, marker)
                    group_names <- c(group_names, g)
                }
            }
            
            counts_mat <- do.call(rbind, counts_list)
            rownames(counts_mat) <- if (has_multi_groups) {
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
                if (has_multi_groups) {
                    col_idx <- which(groups == group_names[i])
                    col_idx <- ((col_idx - 1) %% length(palette)) + 1
                    col_vec <- c(col_vec, palette[col_idx])
                    labels_vec <- c(labels_vec, paste0(marker_names[i], " (", group_names[i], ")"))
                } else {
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
            
            old_par <- par(mar = c(5.5, 4, 4, 2) + 0.1)
            on.exit(par(old_par))
            
            pos_choices <- c(3, 4, 2, 1)
            pos_vec <- rep(pos_choices, length.out = nrow(counts_mat))
            
            HardyWeinberg::HWTernaryPlot(
                counts_mat, 
                region = region_type, 
                signifcolour = FALSE, 
                markercol = col_vec, 
                markerbgcol = col_vec, 
                markerlab = NULL,
                markerpos = pos_vec,
                mcex = 0.8,
                cex = 1.5,
                vertex.cex = 1.2
            )
            
            Xcom <- HardyWeinberg::HWClo(counts_mat)
            M <- matrix(c(-1/sqrt(3), 0, 0, 1, 1/sqrt(3), 0), ncol = 2, byrow = TRUE)
            Xc <- Xcom %*% M
            text(Xc[,1], Xc[,2], labels_vec, col = col_vec, cex = 0.8, pos = pos_vec)
            
            par(xpd = TRUE)
            
            if (has_multi_groups) {
                legend(0, -0.08, legend = groups, col = palette[1:length(groups)], pch = 19, 
                       cex = 1.1, horiz = TRUE, xjust = 0.5, bty = "n")
            } else {
                legend(0, -0.08, legend = c(.("HWE compliant"), .("HWE deviation")), 
                       col = c(col_green, col_red), pch = 19, 
                       cex = 1.1, horiz = TRUE, xjust = 0.5, bty = "n")
            }
            
            if (isTRUE(self$options$ternaryShowMAF)) {
                # Draw MAF threshold boundary lines if applicable
                maf_val <- 0.05
                maf_opt <- self$options$mafThreshold
                if (!is.null(maf_opt) && maf_opt != "none") {
                    maf_val <- as.numeric(maf_opt)
                }
                
                x_left  <- 2 * (maf_val - 0.5) / sqrt(3)
                x_right <- 2 * (0.5 - maf_val) / sqrt(3)
                y_height <- 2 * maf_val
                
                segments(x0 = x_left, y0 = 0, x1 = x_left, y1 = y_height, 
                         col = "#D32F2F", lty = "longdash", lwd = 1.5)
                segments(x0 = x_right, y0 = 0, x1 = x_right, y1 = y_height, 
                         col = "#D32F2F", lty = "longdash", lwd = 1.5)
                
                text(x_left + 0.01, y_height + 0.02, paste0("MAF = ", maf_val), 
                     adj = c(0, 0.5), col = "#D32F2F", cex = 0.8)
                text(x_right - 0.01, y_height + 0.02, paste0("MAF = ", maf_val), 
                     adj = c(1, 0.5), col = "#D32F2F", cex = 0.8)
            }
            
            TRUE
        },
        
        .plotQQ = function(image, ...) {
            if (length(self$options$vars) == 0) return(FALSE)
            
            has_group <- !is.null(self$options$group)
            has_outcome <- !is.null(self$options$outcome) && !is.numeric(private$g_data[[self$options$outcome]]) && length(levels(factor(private$g_data[[self$options$outcome]]))) >= 2
            split_hwe <- isTRUE(self$options$hweSplitOutcome) && has_outcome
            
            subsets <- list()
            if (has_group && split_hwe) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (gl in grp_levels) {
                    for (ol in out_levels) {
                        subsets[[paste0(gl, " - ", ol)]] <- which(private$g_data[[self$options$group]] == gl & private$g_data[[self$options$outcome]] == ol)
                    }
                }
            } else if (has_group) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                for (gl in grp_levels) {
                    subsets[[gl]] <- which(private$g_data[[self$options$group]] == gl)
                }
            } else if (split_hwe) {
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (ol in out_levels) {
                    subsets[[ol]] <- which(private$g_data[[self$options$outcome]] == ol)
                }
            } else {
                subsets[["Total"]] <- seq_along(private$g_classified[[self$options$vars[1]]])
            }
            
            groups <- names(subsets)
            
            # Collect counts matrix (HWQqplot expects genotype counts columns: AA, AB, BB)
            counts_list <- list()
            for (marker in self$options$vars) {
                classified <- private$g_classified[[marker]]
                for (g in groups) {
                    subset_idx <- subsets[[g]]
                    sub_class <- na.omit(classified[subset_idx])
                    sub_class <- sub_class[sub_class != ""]
                    
                    is_x_linked <- isTRUE(self$options$xLinked) && !is.null(self$options$gender)
                    if (is_x_linked) {
                        gender_var <- private$g_data[[self$options$gender]][subset_idx]
                        gender_levels <- levels(gender_var)
                        female_idx_g <- grep("f|w|жен|2|girl|woman", gender_levels, ignore.case=TRUE)
                        if (length(female_idx_g) > 0) {
                            female_level <- gender_levels[female_idx_g[1]]
                        } else {
                            female_level <- gender_levels[1]
                        }
                        female_idx <- which(gender_var == female_level)
                        female_geno <- sub_class[female_idx]
                        
                        obsAA <- sum(female_geno == "AA", na.rm=TRUE)
                        obsAB <- sum(female_geno == "AB", na.rm=TRUE)
                        obsBB <- sum(female_geno == "BB", na.rm=TRUE)
                    } else {
                        obsAA <- sum(sub_class == "AA")
                        obsAB <- sum(sub_class == "AB")
                        obsBB <- sum(sub_class == "BB")
                    }
                    
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
            has_outcome <- !is.null(self$options$outcome) && !is.numeric(private$g_data[[self$options$outcome]]) && length(levels(factor(private$g_data[[self$options$outcome]]))) >= 2
            split_hwe <- isTRUE(self$options$hweSplitOutcome) && has_outcome
            
            subsets <- list()
            if (has_group && split_hwe) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (gl in grp_levels) {
                    for (ol in out_levels) {
                        subsets[[paste0(gl, " - ", ol)]] <- which(private$g_data[[self$options$group]] == gl & private$g_data[[self$options$outcome]] == ol)
                    }
                }
            } else if (has_group) {
                grp_levels <- levels(private$g_data[[self$options$group]])
                for (gl in grp_levels) {
                    subsets[[gl]] <- which(private$g_data[[self$options$group]] == gl)
                }
            } else if (split_hwe) {
                out_levels <- levels(factor(private$g_data[[self$options$outcome]]))
                for (ol in out_levels) {
                    subsets[[ol]] <- which(private$g_data[[self$options$outcome]] == ol)
                }
            } else {
                subsets[["Total"]] <- seq_along(private$g_classified[[self$options$vars[1]]])
            }
            
            groups <- names(subsets)
            
            plot_df <- data.frame()
            obs_label <- .("Observed")
            exp_label <- .("Expected")
            
            all_levels <- c()
            marker_idx <- 0
            for (marker in self$options$vars) {
                marker_idx <- marker_idx + 1
                major <- private$g_alleles[[marker]]$major
                minor <- private$g_alleles[[marker]]$minor
                
                # Append spaces to make genotype strings unique across markers for correct ordering
                spaces <- paste(rep(" ", marker_idx), collapse = "")
                geno_labels <- c(paste0(major, major, spaces), paste0(major, minor, spaces), paste0(minor, minor, spaces))
                all_levels <- c(all_levels, geno_labels)
                
                for (g in groups) {
                    row_key <- paste(marker, g, sep="_")
                    row <- private$g_hwe_results[[row_key]]
                    if (!is.null(row)) {
                        # Extract numeric values safely (supporting X-linked and fallback)
                        raw_vals <- c(
                            if (!is.null(row$raw_obsAA)) row$raw_obsAA else suppressWarnings(as.numeric(row$obsAA)),
                            if (!is.null(row$raw_obsAB)) row$raw_obsAB else suppressWarnings(as.numeric(row$obsAB)),
                            if (!is.null(row$raw_obsBB)) row$raw_obsBB else suppressWarnings(as.numeric(row$obsBB)),
                            if (!is.null(row$raw_expAA)) row$raw_expAA else suppressWarnings(as.numeric(row$expAA)),
                            if (!is.null(row$raw_expAB)) row$raw_expAB else suppressWarnings(as.numeric(row$expAB)),
                            if (!is.null(row$raw_expBB)) row$raw_expBB else suppressWarnings(as.numeric(row$expBB))
                        )
                        plot_df <- rbind(plot_df, data.frame(
                            Marker = marker,
                            Group = g,
                            Genotype = geno_labels,
                            Type = c(obs_label, obs_label, obs_label, exp_label, exp_label, exp_label),
                            Count = raw_vals
                        ))
                    }
                }
            }
            
            if (nrow(plot_df) > 0) {
                plot_df$Genotype <- factor(plot_df$Genotype, levels = all_levels)
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
                
            if (length(groups) > 1) {
                p <- p + facet_grid(Group ~ Marker, scales = "free_x")
            } else {
                p <- p + facet_wrap(~ Marker, scales = "free_x")
            }
            
            print(p)
            TRUE
        },
        
        .plotForest = function(image, ...) {
            if (length(self$options$vars) == 0 || is.null(self$options$outcome)) return(FALSE)
            
            plot_df <- data.frame()
            
            outcome_col <- self$options$outcome
            outcome_var <- private$g_data[[outcome_col]]
            is_continuous <- FALSE
            if (is.numeric(outcome_var)) {
                if (length(unique(na.omit(outcome_var))) > 2) {
                    is_continuous <- TRUE
                }
            }
            
            # Select models
            if (is_continuous) {
                model_list <- c("Codominant (AB vs AA)", "Codominant (BB vs AA)", "Dominant", "Recessive", "Overdominant", "Log-additive")
                if (self$options$geneticModel != "all") {
                    model_list <- switch(self$options$geneticModel,
                                         "codominant" = c("Codominant (AB vs AA)", "Codominant (BB vs AA)"),
                                         "dominant" = "Dominant",
                                         "recessive" = "Recessive",
                                         "overdominant" = "Overdominant",
                                         "log-additive" = "Log-additive")
                }
            } else {
                model_list <- c("Codominant (AB vs AA)", "Codominant (BB vs AA)", "Dominant", "Recessive", "Overdominant", "Log-additive")
                if (self$options$geneticModel != "all") {
                    model_list <- switch(self$options$geneticModel,
                                         "codominant" = c("Codominant (AB vs AA)", "Codominant (BB vs AA)"),
                                         "dominant" = "Dominant",
                                         "recessive" = "Recessive",
                                         "overdominant" = "Overdominant",
                                         "log-additive" = "Log-additive")
                }
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
            
            if (is_continuous) {
                p <- ggplot(plot_df, aes(x = OR, y = Marker)) +
                    geom_vline(xintercept = 0.0, linetype = "dashed", color = "gray50") +
                    geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, size = 0.8, color = "#2B5C8F") +
                    geom_point(size = 3.5, color = "#D95F02") +
                    theme_minimal(base_size = 12) +
                    labs(x = .("Effect size (Beta)"), y = .("Marker"))
            } else {
                if (isTRUE(self$options$assocForestTrunc)) {
                    min_or <- min(plot_df$OR, na.rm = TRUE)
                    max_or <- max(plot_df$OR, na.rm = TRUE)
                    
                    lower_limit <- 10^(floor(log10(min(0.2, min_or))))
                    upper_limit <- 10^(ceiling(log10(max(5, max_or))))
                    
                    df_normal <- plot_df[!is.na(plot_df$Lower) & !is.na(plot_df$Upper) & plot_df$Lower >= lower_limit & plot_df$Upper <= upper_limit, ]
                    df_left_trunc <- plot_df[!is.na(plot_df$Lower) & !is.na(plot_df$Upper) & plot_df$Lower < lower_limit & plot_df$Upper <= upper_limit, ]
                    df_right_trunc <- plot_df[!is.na(plot_df$Lower) & !is.na(plot_df$Upper) & plot_df$Lower >= lower_limit & plot_df$Upper > upper_limit, ]
                    df_both_trunc <- plot_df[!is.na(plot_df$Lower) & !is.na(plot_df$Upper) & plot_df$Lower < lower_limit & plot_df$Upper > upper_limit, ]
                    
                    p <- ggplot(plot_df, aes(x = OR, y = Marker)) +
                        geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray50")
                    
                    if (nrow(df_normal) > 0) {
                        p <- p + geom_errorbarh(data = df_normal, aes(xmin = Lower, xmax = Upper), height = 0.2, size = 0.8, color = "#2B5C8F")
                    }
                    
                    if (nrow(df_left_trunc) > 0) {
                        p <- p + geom_segment(data = df_left_trunc, aes(x = Upper, xend = lower_limit, y = Marker, yend = Marker),
                                              arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last"), size = 0.8, color = "#2B5C8F") +
                                 geom_errorbarh(data = df_left_trunc, aes(xmin = Upper, xmax = Upper), height = 0.2, size = 0.8, color = "#2B5C8F")
                    }
                    
                    if (nrow(df_right_trunc) > 0) {
                        p <- p + geom_segment(data = df_right_trunc, aes(x = Lower, xend = upper_limit, y = Marker, yend = Marker),
                                              arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "last"), size = 0.8, color = "#2B5C8F") +
                                 geom_errorbarh(data = df_right_trunc, aes(xmin = Lower, xmax = Lower), height = 0.2, size = 0.8, color = "#2B5C8F")
                    }
                    
                    if (nrow(df_both_trunc) > 0) {
                        p <- p + geom_segment(data = df_both_trunc, aes(x = lower_limit, xend = upper_limit, y = Marker, yend = Marker),
                                              arrow = arrow(length = unit(0.1, "inches"), type = "closed", ends = "both"), size = 0.8, color = "#2B5C8F")
                    }
                    
                    p <- p + geom_point(size = 3.5, color = "#D95F02") +
                        scale_x_log10(limits = c(lower_limit, upper_limit), labels = function(x) sapply(x, function(val) if (is.na(val)) "" else format(val, scientific = FALSE, trim = TRUE))) +
                        theme_minimal(base_size = 12) +
                        labs(x = .("Odds Ratio (log scale)"), y = .("Marker"))
                } else {
                    p <- ggplot(plot_df, aes(x = OR, y = Marker)) +
                        geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray50") +
                        geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, size = 0.8, color = "#2B5C8F") +
                        geom_point(size = 3.5, color = "#D95F02") +
                        scale_x_log10(labels = function(x) sapply(x, function(val) if (is.na(val)) "" else format(val, scientific = FALSE, trim = TRUE))) +
                        theme_minimal(base_size = 12) +
                        labs(x = .("Odds Ratio (log scale)"), y = .("Marker"))
                }
            }
                
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
            
            major1 <- private$g_alleles[[comb_snps[1]]]$major
            minor1 <- private$g_alleles[[comb_snps[1]]]$minor
            geno1 <- c(paste0(major1, major1), paste0(major1, minor1), paste0(minor1, minor1))
            
            major2 <- private$g_alleles[[comb_snps[2]]]$major
            minor2 <- private$g_alleles[[comb_snps[2]]]$minor
            geno2 <- c(paste0(major2, major2), paste0(major2, minor2), paste0(minor2, minor2))
            
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
                geom_text(aes(label = Label), size = 4.5, color = "black", fontface = "bold") +
                scale_x_discrete(labels = c("AA" = geno1[1], "AB" = geno1[2], "BB" = geno1[3])) +
                scale_y_discrete(labels = c("AA" = geno2[1], "AB" = geno2[2], "BB" = geno2[3])) +
                scale_fill_manual(values = c("High" = "#FF8C8C", "Low" = "#8CCEFF"),
                                  labels = c("High" = .("High Risk"), "Low" = .("Low Risk"))) +
                theme_minimal(base_size = 12) +
                theme(
                    legend.position = "bottom",
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 12),
                    axis.title.x = element_text(size = 14, face = "bold"),
                    axis.title.y = element_text(size = 14, face = "bold"),
                    axis.text.x = element_text(size = 12, face = "bold"),
                    axis.text.y = element_text(size = 12, face = "bold")
                ) +
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
                      legend.title = element_text(size = 12),
                      axis.title.x = element_text(size = 14, face = "bold"),
                      axis.title.y = element_text(size = 14, face = "bold"),
                      axis.text.x = element_text(size = 12, face = "bold"),
                      axis.text.y = element_text(size = 12, face = "bold")) +
                labs(x = .("Interaction Order"), y = .("Balanced Accuracy (BA)"), fill = .("Type"))
                
            print(p)
            TRUE
        },
        
        .plotLDHeatmap = function(image, ...) {
            snps <- self$options$vars
            call_rate_thr <- switch(self$options$qcCallRate, "0.90" = 0.90, "0.95" = 0.95, "0.98" = 0.98, "none" = 0)
            maf_thr <- switch(self$options$qcMaf, "0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10, "none" = 0)
            hwe_thr <- switch(self$options$qcHwe, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001, "0.0001" = 0.0001, "none" = 0)
            if (call_rate_thr > 0 || maf_thr > 0 || hwe_thr > 0) {
                snps <- Filter(function(m) private$g_qc_status[[m]] == "Passed", snps)
            }
            if (length(snps) < 2) return(FALSE)
            
            # Parse data
            ld_df <- data.frame()
            for (row_key in names(private$g_ld_results)) {
                row <- private$g_ld_results[[row_key]]
                if (!is.null(row) && !is.na(row$r2)) {
                    r2_val <- as.numeric(row$r2)
                    dp_val <- as.numeric(row$d_prime)
                    metric_val <- if (self$options$ldMetric == "r2") r2_val else dp_val
                    if (row$marker1 %in% snps && row$marker2 %in% snps) {
                        ld_df <- rbind(ld_df, data.frame(
                            Marker1 = factor(row$marker1, levels=snps),
                            Marker2 = factor(row$marker2, levels=snps),
                            Value = metric_val
                        ))
                    }
                }
            }
            
            if (nrow(ld_df) == 0) return(FALSE)
            
            metric_title <- if (self$options$ldMetric == "r2") "r²" else "D'"
            
            n_markers <- length(snps)
            font_size <- if (n_markers <= 10) 5 else if (n_markers <= 20) 4 else 3
            
            p <- ggplot(ld_df, aes(x = Marker1, y = Marker2, fill = Value)) +
                geom_tile(color = "gray80", size = 0.5)
                
            if (n_markers <= 25) {
                p <- p + geom_text(aes(label = round(Value * 100)), color = "black", size = font_size, fontface = "bold")
            }
            
            p <- p + scale_fill_gradient(low = "#FFFFFF", high = "#E41A1C", limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
                theme_minimal(base_size = 12) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                      axis.text.y = element_text(size = 11),
                      panel.grid = element_blank(),
                      legend.text = element_text(size = 11),
                      legend.title = element_text(size = 12)) +
                labs(x = "", y = "", fill = metric_title)
                
            print(p)
            TRUE
        },
        
        .plotManhattan = function(image, ...) {
            if (length(self$options$vars) == 0 || is.null(self$options$outcome)) return(FALSE)
            
            plot_df <- data.frame()
            for (marker in self$options$vars) {
                # Find association results for this marker
                marker_pvals <- numeric()
                for (row_key in names(private$g_assoc_results)) {
                    row <- private$g_assoc_results[[row_key]]
                    if (!is.null(row) && row$marker == marker && !is.na(row$p_val)) {
                        marker_pvals <- c(marker_pvals, row$p_val)
                    }
                }
                if (length(marker_pvals) > 0) {
                    min_p <- min(marker_pvals)
                    plot_df <- rbind(plot_df, data.frame(
                        Marker = marker,
                        PValue = min_p,
                        LogP = -log10(min_p)
                    ))
                }
            }
            
            if (nrow(plot_df) == 0) return(FALSE)
            
            plot_df$Marker <- factor(plot_df$Marker, levels = self$options$vars)
            
            n_tests <- length(self$options$vars)
            bonf_p <- 0.05 / max(1, n_tests)
            
            p <- ggplot(plot_df, aes(x = Marker, y = LogP)) +
                geom_bar(stat = "identity", fill = "#2B5C8F", width = 0.5, color = "black", size = 0.2) +
                geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue", size = 0.8) +
                geom_hline(yintercept = -log10(bonf_p), linetype = "dashed", color = "red", size = 0.8) +
                theme_minimal(base_size = 12) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11, face = "bold"),
                      axis.text.y = element_text(size = 11),
                      axis.title.x = element_text(size = 12, face = "bold"),
                      axis.title.y = element_text(size = 12, face = "bold")) +
                labs(x = .("Marker"), y = .("-log10(p-value)"))
            
            print(p)
            TRUE
        },
        
        .calculatePRS = function() {
            markers <- self$options$vars
            N_ind <- nrow(private$g_data)
            scores <- rep(0, N_ind)
            
            weights <- numeric(length(markers))
            names(weights) <- markers
            
            is_continuous <- FALSE
            outcome_col <- self$options$outcome
            if (!is.null(outcome_col)) {
                outcome_var <- private$g_data[[outcome_col]]
                if (is.numeric(outcome_var) && length(unique(na.omit(outcome_var))) > 2) {
                    is_continuous <- TRUE
                }
            }
            
            for (m in markers) {
                row_key <- paste(m, "Log-additive", sep="_")
                row <- private$g_assoc_results[[row_key]]
                w <- 0.0
                if (!is.null(row) && !is.na(row$or_val)) {
                    if (is_continuous) {
                        w <- row$or_val
                    } else {
                        w <- log(row$or_val)
                    }
                }
                weights[m] <- w
            }
            
            for (i in 1:N_ind) {
                ind_score <- 0
                valid_snps <- 0
                for (m in markers) {
                    val <- private$g_classified[[m]][i]
                    if (!is.na(val)) {
                        dosage <- switch(val, "AA" = 0, "AB" = 1, "BB" = 2, 0)
                        ind_score <- ind_score + weights[m] * dosage
                        valid_snps <- valid_snps + 1
                    }
                }
                if (valid_snps > 0 && valid_snps < length(markers)) {
                    ind_score <- ind_score * (length(markers) / valid_snps)
                }
                scores[i] <- if (valid_snps > 0) ind_score else NA
            }
            
            self$results$prsOutput$setValues(scores)
        }
    )
)
