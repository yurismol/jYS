# This file is a generated template, your changes will not be overwritten

mOUTClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mOUTClass",
    inherit = mOUTBase,
    public = list(
        .savePart = function(path, part, ...) {
            smart_lookup <- function(results, p_str, options = NULL) {
                if (is.null(results) || is.null(p_str)) return(NULL)
                if (length(p_str) > 1) p_str <- paste(p_str, collapse = "/")
                if (!nzchar(p_str)) return(NULL)

                covs <- tryCatch(options$covariates, error = function(e) NULL)
                group_var <- tryCatch(options$group, error = function(e) NULL)
                vars <- tryCatch(options$vars, error = function(e) NULL)

                find_child <- function(curr, seg) {
                    if (is.null(curr) || is.null(seg) || !nzchar(seg)) return(NULL)
                    clean <- gsub('^["\']|["\']$', '', seg)

                    if (inherits(curr, 'Array')) {
                        nxt <- tryCatch(curr$get(key = clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)

                        nxt <- tryCatch(curr$get(name = paste0('"', clean, '"')), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                        nxt <- tryCatch(curr$get(name = seg), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                        nxt <- tryCatch(curr$get(name = clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)

                        items <- tryCatch(curr$items, error = function(e) NULL)
                        if (!is.null(items) && length(items) > 0) {
                            # 1. Exact match by key, name, or title
                            for (it in items) {
                                it_key <- tryCatch(it$key, error = function(e) NULL)
                                it_name <- tryCatch(it$name, error = function(e) NULL)
                                it_title <- tryCatch(it$title, error = function(e) NULL)
                                if (identical(it_key, clean) || identical(it_key, seg) ||
                                    identical(it_name, clean) || identical(it_name, seg) ||
                                    identical(it_title, clean) || identical(it_title, seg)) {
                                    return(it)
                                }
                            }

                            # 2. Match covariate by index if clean is in covs or group_var
                            if (!is.null(covs) && clean %in% covs) {
                                c_idx <- which(covs == clean)
                                target_key <- paste0("..cov_", c_idx, "_")
                                for (it in items) {
                                    if (identical(it$key, target_key) || grepl(paste0("^\\.\\.cov_", c_idx), as.character(it$key))) {
                                        return(it)
                                    }
                                }
                            }
                            if (!is.null(group_var) && (clean == group_var || grepl(paste0("^", clean), group_var))) {
                                for (it in items) {
                                    if (identical(it$key, "..group") || grepl("^\\.\\.group", as.character(it$key))) {
                                        return(it)
                                    }
                                }
                            }

                            # 3. Match suffix of title after dash
                            for (it in items) {
                                it_title <- tryCatch(as.character(it$title), error = function(e) "")
                                title_var <- trimws(sub(".*[\u2013\u2014-]\\s*", "", it_title))
                                title_var_clean <- gsub("[_()]", "", title_var)
                                clean_nopunct <- gsub("[_()]", "", clean)
                                if (nzchar(title_var) && (identical(title_var, clean) || identical(title_var_clean, clean_nopunct))) {
                                    return(it)
                                }
                            }

                            # 4. Word boundary match in title
                            for (it in items) {
                                it_title <- tryCatch(as.character(it$title), error = function(e) "")
                                pattern <- paste0("(^|[^a-zA-Z0-9_])", clean, "([^a-zA-Z0-9_]|$)")
                                if (grepl(pattern, it_title)) {
                                    return(it)
                                }
                            }

                            # 5. Check integer index
                            idx <- suppressWarnings(as.integer(clean))
                            if (!is.na(idx)) {
                                items_len <- length(items)
                                if (idx == 0 && items_len >= 1) return(items[[1]])
                                if (idx >= 1 && idx <= items_len) return(items[[idx]])
                            }
                        }
                    }

                    nxt <- tryCatch(curr$.lookup(seg), error = function(e) NULL)
                    if (!is.null(nxt)) return(nxt)
                    if (seg != clean) {
                        nxt <- tryCatch(curr$.lookup(clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                    }

                    if (inherits(curr, 'Group')) {
                        nxt <- tryCatch(curr$get(clean), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                        nxt <- tryCatch(curr$get(seg), error = function(e) NULL)
                        if (!is.null(nxt)) return(nxt)
                    }

                    items <- tryCatch(curr$items, error = function(e) NULL)
                    if (!is.null(items) && length(items) > 0) {
                        if (clean %in% names(items)) return(items[[clean]])
                        if (seg %in% names(items)) return(items[[seg]])
                        for (it in items) {
                            it_name <- tryCatch(it$name, error = function(e) NULL)
                            it_key <- tryCatch(it$key, error = function(e) NULL)
                            it_title <- tryCatch(it$title, error = function(e) NULL)
                            if (identical(it_name, clean) || identical(it_name, seg) ||
                                identical(it_key, clean) || identical(it_key, seg)) {
                                return(it)
                            }
                        }
                    }
                    NULL
                }

                traverse <- function(node, segs) {
                    if (is.null(node) || length(segs) == 0) return(node)
                    child <- find_child(node, segs[1])
                    if (!is.null(child)) {
                        return(traverse(child, segs[-1]))
                    }
                    NULL
                }

                search_recursive <- function(node, segs) {
                    if (is.null(node)) return(NULL)
                    res <- traverse(node, segs)
                    if (!is.null(res)) return(res)

                    items <- tryCatch(node$items, error = function(e) NULL)
                    if (!is.null(items) && length(items) > 0) {
                        for (child in items) {
                            if (inherits(child, 'Group') || inherits(child, 'Array')) {
                                res <- search_recursive(child, segs)
                                if (!is.null(res)) return(res)
                            }
                        }
                    }
                    NULL
                }

                norm_p <- gsub('\\[[\'"]?([^\'"]+?)[\'"]?\\]', '/\\1', p_str)
                parts <- strsplit(norm_p, '/+', perl = TRUE)[[1]]
                parts <- parts[parts != ""]
                if (length(parts) == 0) return(NULL)

                # Strip analysisId or 'results' prefix if present
                if (length(parts) > 1 && (parts[1] == "results" || !is.na(suppressWarnings(as.integer(parts[1]))))) {
                    if (is.null(find_child(results, parts[1]))) {
                        parts <- parts[-1]
                    }
                }

                target <- search_recursive(results, parts)
                if (inherits(target, 'Array') && length(target$items) > 0) {
                    target <- target$items[[1]]
                }
                target
            }

            element <- smart_lookup(self$results, part, self$options)
            if (is.null(element)) {
                return(FALSE)
            }

            if (inherits(element, 'Array')) {
                if (length(element$items) > 0) {
                    element <- element$items[[1]]
                } else {
                    return(FALSE)
                }
            }

            if (inherits(element, 'Image')) {
                if (is.null(element$state) && !is.null(element$parent$state)) {
                    parent_state <- element$parent$state
                    if (!is.null(parent_state$zph)) {
                        var_name <- element$key
                        var_idx <- if (!is.null(var_name) && var_name %in% rownames(parent_state$zph$table)) {
                            which(rownames(parent_state$zph$table) == var_name)
                        } else 1
                        element$setState(list(zph = parent_state$zph, var_idx = var_idx, var_name = var_name))
                    } else if (!is.null(element$key) && !is.null(parent_state[[element$key]])) {
                        element$setState(parent_state[[element$key]])
                    } else if (is.list(parent_state) && length(parent_state) > 0) {
                        element$setState(parent_state[[1]])
                    }
                }
            }

            if (element$requiresData && is.function(private$.ensureData)) {
                private$.ensureData()
            }
            save_ok <- tryCatch({
                element$saveAs(path, ...)
                file.exists(path) && file.info(path)$size > 0
            }, error = function(e) {
                tryCatch({
                    element$saveAs(path)
                    file.exists(path) && file.info(path)$size > 0
                }, error = function(e2) {
                    FALSE
                })
            })
            return(save_ok)
        }
    ),
    private = list(
        .ensureData = function() {
            if (is.null(private$.data) || nrow(private$.data) == 0) {
                d <- tryCatch(self$readDataset(headerOnly = FALSE), error = function(e) NULL)
                if (!is.null(d) && nrow(d) > 0)
                    private$.data <- d
            }
            return(!is.null(private$.data) && nrow(private$.data) > 0)
        },

        .init = function() {
          private$.initOutputs()

          # Initialize columns for outstat
          outable <- self$results$stat$outstat
          
          grp <- self$options$group
          if (!is.null(grp)) outable$addColumn(name="grp", title=.("Group"), type='text')
          
          outable$addColumn(name="noutl", title=.("Outliers found"), type='integer')
          
          if (self$options$norm) outable$addColumn(name="psh", title=.("Normality p-value"), type='number')
          
          outlcheck <- self$options$outlcheck
          if (outlcheck %in% c("ZS", "MAH")) {
            outable$addColumn(name="m", title=.("Mean"), type='number')
          } else {
            outable$addColumn(name="m", title=.("Median"), type='number')
          }
          
          outable$addColumn(name="lf",  title=.("Lower fence"), type='number')
          outable$addColumn(name="uf",  title=.("Upper fence"), type='number')
          
          if (self$options$norm) outable$setNote('flag', .('<b>*p<0.05</b> - The hypothesis of normal distribution was rejected by the Shapiro-Wilk test'))
          
          # Initialize columns for oind tables
          oind <- self$results$oind
          keys <- self$options$vars
          for (key in keys) {
            tab <- oind$get(key=key)
            if (!is.null(grp)) tab$addColumn(name="grp",  title=.("Group"), type='text')
            tab$addColumn(name="onum", title=.("Outliers found"), type='integer')
            tab$addColumn(name="indx", title=.("Indices (Rows)"), type='text')
          }
        },

        .run = function() {
          #self$results$text$setContent(self$options$vars)
          private$.populateOutputs()
          private$.filltable()
        },

        .initOutputs=function() {
            description = function(part1) {
                return(
                    jmvcore::format(.("{varType} without outliers"),
                        varType=part1)
                )
            }

            title = function(part1=NULL, part2=NULL) {
                return(jmvcore::format("{} ({})", part2, part1))
            }

            if (self$options$remOut) {
              keys <- self$options$vars
              measureTypes <- sapply(keys, function(x) private$.columnType(self$data[[x]]))

              titles <- vapply(keys, function(key) title(.("outl"), key), '')
              descriptions <- vapply(keys, function(key) description(key), '')
              self$results$remOut$set(keys, titles, descriptions, measureTypes)
            }
        },

        .columnType = function(column) {
            if (inherits(column, "ordered")) {
                return("ordinal")
            } else if (inherits(column, "factor")) {
                return("nominal")
            } else {
                return("continuous")
            }
        },

        .calcIQR=function(x, grp="", fence=1.5, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data  <- split(df, df$grp)
           find_ranges <- function(df) {
             median <- median(df$x, na.rm=na.rm)
             Q1   <- quantile(df$x, 0.25, na.rm=na.rm)
             Q3   <- quantile(df$x, 0.75, na.rm=na.rm)
             IQR  <- Q3 - Q1
             lr   <- Q1 - fence*IQR
             ur   <- Q3 + fence*IQR
             outlier_condition <- (df$x < lr | df$x > ur) & !is.na(df$x)
             ind  <- as.integer(rownames(df)[outlier_condition])
             no   <- length(ind)
             test <- try(shapiro.test(df$x))
             if (jmvcore::isError(test)) {
               norm <- ""
             } else {
               norm <- test$p.value
             }
             return(list(lr, ur, no, median, norm, ind))
           }
           return(lapply(split_data, find_ranges))
        },

        .outliers=function(x, grp="", fence=1.5, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data <- split(df, df$grp)
           find_outliers <- function(df) {
             Q1  <- quantile(df$x, 0.25, na.rm=na.rm)
             Q3  <- quantile(df$x, 0.75, na.rm=na.rm)
             IQR <- Q3 - Q1
             outlier_condition <- df$x < (Q1 - fence*IQR) | df$x > (Q3 + fence*IQR)
             as.integer(rownames(df)[outlier_condition])
           }
           indices <- unlist(lapply(split_data, find_outliers), use.names=FALSE)
           return(indices)
        },

        .calcZ=function(x, grp="", thr=3, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data <- split(df, df$grp)
           find_ranges <- function(df) {
             mean <- mean(df$x, na.rm=na.rm)
             sd   <- sd(df$x, na.rm=na.rm)
             z_scores <- (df$x-mean)/sd		# Compute z-scores
             outlier_condition <- (abs(z_scores) > thr) & !is.na(df$x)
             ind <- as.integer(rownames(df)[outlier_condition])
             no  <- length(ind)
             ur <- mean + thr * sd
             lr <- mean - thr * sd
             #return(unlist(list(lr, ur, no, mean), use.names=FALSE))
             test <- try(shapiro.test(df$x))
             if (jmvcore::isError(test)) {
               norm <- ""
             } else {
               norm <- test$p.value
             }
             return(list(lr, ur, no, mean, norm, ind))
           }
           return(lapply(split_data, find_ranges))
        },

        .z_score=function(x, grp="", fence=3.0, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data <- split(df, df$grp)
           find_outliers <- function(df) {
             z_scores <- (df$x-mean(df$x))/sd(df$x)	# Compute z-scores
             outlier_condition <- abs(z_scores) > fence
             as.integer(rownames(df)[outlier_condition])
           }
           indices <- unlist(lapply(split_data, find_outliers), use.names=FALSE)
           return(indices)
        },

        .calcMAH=function(x, grp="", na.rm=TRUE, ...) {

        },

        .calcMZ=function(x, grp="", thr=3.5, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data <- split(df, df$grp)
           find_ranges <- function(df) {
             median <- median(df$x, na.rm=na.rm)
             mad    <- mad(df$x, constant=1, na.rm=na.rm)
             if (mad==0) {
               mad   <- mean(abs(df$x-mean(df$x, na.rm=na.rm)), na.rm=na.rm)
               denom <- 0.797885	# 1 / 1.253314
             } else {
               denom <- 0.6745		# 1 / 1.48258
             }
             lr <- median - thr*mad/denom
             ur <- median + thr*mad/denom
             z_scores <- denom * (df$x-median) / mad
             outlier_condition <- (abs(z_scores) > thr) & !is.na(df$x)
             ind <- as.integer(rownames(df)[outlier_condition])
             no  <- length(ind)
             test <- try(shapiro.test(df$x))
             if (jmvcore::isError(test)) {
               norm <- ""
             } else {
               norm <- test$p.value
             }
             return(list(lr, ur, no, median, norm, ind))
           }
           return(lapply(split_data, find_ranges))
        },

        .mz_score=function(x, grp="", thr=3.5, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data <- split(df, df$grp)
           find_outliers <- function(df) {
             median <- median(df$x, na.rm=na.rm)
             mad <- mad(df$x, constant=1, na.rm=na.rm)	# constant=1 to get raw MAD without scaling for normality
             if (mad==0) {
               mad   <- mean(abs(df$x-mean(df$x, na.rm=na.rm)), na.rm=na.rm)
               denom <- 0.797885	# 1 / 1.253314
             } else {
               denom <- 0.6745		# 1 / 1.48258
             }
             z_scores <- denom * (df$x-median) / mad
             outlier_condition <- abs(z_scores) > thr
             as.integer(rownames(df)[outlier_condition])
           }
           indices <- unlist(lapply(split_data, find_outliers), use.names=FALSE)
           return(indices)
        },

        .mz_var=function(x, grp="", fence=3.0, na.rm=TRUE, ...) {
           df <- data.frame(x, grp)
           split_data <- split(df, df$grp)
           z_calc <- function(df) {
             z_scores <- 0.6745 * (df$x-median(df$x))/mad(df$x, constant=1)	# constant=1 to get raw MAD without scaling for normality
	     z_scores
           }
           zvar <- unsplit(lapply(split_data, z_calc), grp)
           return(zvar)
        },

        .plot=function(image, ggtheme, theme, ...) {
	    range <- as.double(self$options$fence)
            if (range==0) return(FALSE)
            private$.ensureData()
	    #https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/boxplot
	    vars  <- self$options$vars
	    nVars <- length(vars)
            key   <- image$key
	    #dat   <- image$state$data
            dat   <- data.frame(self$data, check.names=FALSE)
            if (nrow(dat) == 0) return(FALSE)
	    grp   <- self$options$group
	    if (is.null(grp)) {
	      frm <- stats::as.formula(jmvcore::composeFormula(NULL, key))
	    } else {
	      frm <- stats::as.formula(jmvcore::composeFormula(key, grp))
	    }
            car::Boxplot(frm, data=dat, range=range)
	    return(TRUE)
        },

        .decimalplaces=function(col) {
          maxd = 0
          for (x in col) {
            if (!is.na(x) && (x%%1)!= 0) {
              maxd = max(maxd, nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][2]))
            }
          }
          return(maxd)
        },

        .splitstr=function(str="") {
          # Split into elements
          elements <- unlist(strsplit(str, ", "))
	  ng <- 10
          # Group into chunks of 10, then paste with commas and newlines
          grouped <- split(elements, ceiling(seq_along(elements)/ng))
          return(paste(sapply(grouped, function(x) paste(x, collapse=", ")), collapse=",<br>"))
        },

        .filltable=function() {
          outable <- self$results$stat$outstat
          oind    <- self$results$oind
          grp   <- self$options$group
          outlcheck <- self$options$outlcheck
          fence <- as.double(self$options$fence)

          keys  <- self$options$vars
          dat   <- data.frame(self$data, check.names=FALSE)
          dat   <- jmvcore::select(dat, c(self$options$vars, self$options$group))
          g  <- ""
          if (!is.null(self$options$group)) g <- dat[[self$options$group]]
          for (i in seq_along(keys)) {
            key <- keys[[i]]
            d   <- dat[[key]]
            dec <- private$.decimalplaces(d)
            if (outlcheck=="IQR") {
              outl  <- private$.calcIQR(d, g, fence)
            } else if (outlcheck=="ZS") {
	      thold <- as.double(self$options$tholdZS)
              outl  <- private$.calcZ(d, g, thold)
            } else if (outlcheck=="mZS") {
	      thold <- as.double(self$options$tholdmZS)
              outl  <- private$.calcMZ(d, g, thold)
            } else if (outlcheck=="MAH") {
            } else {
              outl  <- ""
            }
            nm <- names(outl)
            tab  <- oind$get(key=key)
            for (j in seq_along(outl)) {
              lst  <- unlist(outl[[j]])
              gr   <- nm[[j]]
              norm <- ifelse(lst[5]<0.001,"<0.001",lst[5])
              outable$addRow(rowKey=paste(key, gr),
		list(var=key, grp=gr, m=lst[4], lf=lst[1], uf=lst[2], noutl=lst[3],
		psh=norm)
                )
              if (self$options$norm && lst[5]<0.05) outable$addSymbol(rowKey=paste(key, gr), "psh", '*')
              indx <- toString(unlist(outl[[j]][6]))
              indx <- private$.splitstr(indx)
              tab$addRow(rowKey=paste(key, gr), list(grp=gr, onum=lst[3], indx=indx))
            }
          }
        },

        .populateOutputs=function() {
            dat   <- data.frame(self$data, check.names=FALSE)
            dat   <- jmvcore::select(dat, c(self$options$vars, self$options$group))

            if (self$options$remOut && self$results$remOut$isNotFilled()) {
                self$results$remOut$setRowNums(rownames(self$data))
                keys <- self$options$vars
		g  <- ""
		if (!is.null(self$options$group)) g <- dat[[self$options$group]]
		fence <- as.double(self$options$fence)
                outlcheck <- self$options$outlcheck
                for (i in seq_along(keys)) {
                  key <- keys[[i]]
                  d   <- dat[[key]]
                  dec <- private$.decimalplaces(d)
                  if (outlcheck=="IQR") {
                    outl <- private$.outliers(d, g, fence)
                  } else if (outlcheck=="ZS") {
		    thold <- as.double(self$options$tholdZS)
                    outl  <- private$.z_score(d, g, thold)
                  } else if (outlcheck=="mZS") {
		    thold <- as.double(self$options$tholdmZS)
                    outl  <- private$.mz_score(d, g, thold)
                  }
                  d[outl] <- NA
                  self$results$remOut$setValues(index=i, round(d, dec))
                }
            }
        }
  )
)
