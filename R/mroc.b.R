
# This file is a generated template, your changes will not be overwritten

mROCClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mROCClass",
    inherit = mROCBase,
    private = list(
      .init=function() {
          private$.errorCheck()
	  #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
      },

      .run = function() {
	  vars  <- self$options$vars
	  nVars <- length(vars)
          dat   <- data.frame(self$data, check.names=FALSE)
          private$.errorCheck()
          for (var in vars) {
            image <- self$results$plots$get(key=var)
            image$setState(list(data=dat))
            image <- self$results$splots$get(key=var)
            image$setState(list(data=dat))
          }
          image <- self$results$plot
          image$setState(list(data=dat))
	  #if (self$options$sepROC) image$visible=FALSE

          # Populate the ROC results table
          if (self$options$showTable)
            private$.populateTable(dat)
      },

      .populateTable = function(dat) {
          rocTable <- self$results$rocTable
          rocTable$deleteRows()

          vars  <- self$options$vars
          nVars <- length(vars)
          perc  <- self$options$perc
          dL    <- self$options$cmpDeLong
          mult  <- ifelse(perc, 1, 1)
          fmt   <- ifelse(perc, '%#.1f', '%#.3f')

          # data subset for non-sep mode
          if (!is.null(self$options$groups) && !self$options$sepROC) {
            dat <- dat[dat[[self$options$groups]] == self$options$selgroup, , drop=FALSE]
          }

          cls  <- jmvcore::select(dat, self$options$class)
          lev  <- base::levels(cls[!is.na(cls[[self$options$class]]), self$options$class])

          # Determine iteration mode
          if (self$options$sepROC && !is.null(self$options$groups)) {
            # sepROC: for each var, iterate over group levels
            for (var in vars) {
              grp   <- self$options$groups
              grps  <- jmvcore::select(dat, grp)
              glevs <- base::levels(grps[!is.na(grps[[grp]]), grp])

              oROC <- NULL
              for (gl in glevs) {
                dd  <- dat[dat[[grp]] == gl, , drop=FALSE]
                frm <- stats::as.formula(jmvcore::composeFormula(self$options$class, var))
                roc <- tryCatch(pROC::roc(frm, data=dd, percent=perc, quiet=TRUE),
                                error=function(e) NULL)
                if (is.null(roc)) next

                row_name <- paste0(var, " : ", gl)
                private$.addTableRow(rocTable, roc, row_name, lev, dL, oROC, paired=FALSE, fmt=fmt)
                if (dL != "wcontr" || is.null(oROC)) oROC <- roc
              }
            }
          } else {
            # Main mode: iterate over vars
            oROC <- NULL
            for (var in vars) {
              frm <- stats::as.formula(jmvcore::composeFormula(self$options$class, var))
              roc <- tryCatch(pROC::roc(frm, data=dat, percent=perc, quiet=TRUE),
                              error=function(e) NULL)
              if (is.null(roc)) next

              private$.addTableRow(rocTable, roc, var, lev, dL, oROC, paired=TRUE, fmt=fmt)
              if (dL != "wcontr" || is.null(oROC)) oROC <- roc
            }
          }
      },

      .addTableRow = function(rocTable, roc, label, lev, dL, oROC, paired, fmt) {
          perc <- self$options$perc

          # AUC + CI
          rci     <- pROC::ci(roc, conf.level=self$options$ciWidth/100)
          auc_val <- as.numeric(rci[2])

          # CI string
          ci_fmt  <- gsub("%%", "", fmt)
          auc_ci  <- paste0(sprintf(ci_fmt, rci[1]), " - ", sprintf(ci_fmt, rci[3]))
          if (perc) auc_ci <- paste0(auc_ci, "%")

          # Best point (cutoff, Se, Sp)
          bss    <- ifelse(self$options$theBest %in% c("bestY","bssY"), "youden", "closest.topleft")
          coords <- tryCatch(
            pROC::coords(roc, "best", best.method=bss,
                         ret=c("threshold","sensitivity","specificity")),
            error=function(e) NULL)

          cutoff <- NA; se <- NA; sp <- NA
          if (!is.null(coords)) {
            if (is.data.frame(coords)) {
              cutoff <- as.numeric(coords[1, "threshold"])
              se     <- as.numeric(coords[1, "sensitivity"])
              sp     <- as.numeric(coords[1, "specificity"])
            } else if (is.matrix(coords)) {
              cutoff <- as.numeric(coords[1, "threshold"])
              se     <- as.numeric(coords[1, "sensitivity"])
              sp     <- as.numeric(coords[1, "specificity"])
            } else {
              cutoff <- as.numeric(coords["threshold"])
              se     <- as.numeric(coords["sensitivity"])
              sp     <- as.numeric(coords["specificity"])
            }
          }

          # Direction
          l1 <- substr(lev[1],1,1); l2 <- substr(lev[2],1,1)
          if (l1 == l2) { l1 <- substr(lev[1],1,2); l2 <- substr(lev[2],1,2) }
          dir_str <- paste0(l1, roc$direction, l2)

          # DeLong p-value
          pval_val <- ''
          if (dL != "none") {
            if (dL == "w05") {
              null_auc <- ifelse(perc, 50, 0.5)
              variance <- pROC::var(roc, method="delong")
              if (variance <= 0) {
                pval_val <- ifelse(roc$auc == null_auc, 1.0, 0.0001)
              } else {
                z    <- (roc$auc - null_auc) / sqrt(variance)
                pval_val <- 2 * stats::pnorm(-abs(z))
              }
            } else if (!is.null(oROC)) {
              rt <- tryCatch(
                pROC::roc.test(roc, oROC, paired=paired,
                               parallel=TRUE, method="d", alternative="two.sided"),
                error=function(e) NULL)
              if (!is.null(rt)) {
                pval_val <- rt$p.value
              }
            }
          }

          rocTable$addRow(rowKey=label, values=list(
            var       = label,
            auc       = auc_val,
            auc_ci    = auc_ci,
            cutoff    = cutoff,
            se        = se,
            sp        = sp,
            direction = dir_str,
            pval      = pval_val
          ))
      },

      .plot=function(image, ggtheme, theme, ...) {
          dat <- image$state$data
          if (is.null(dat) || nrow(dat) == 0) return(FALSE)

          result <- tryCatch({
              vars  <- self$options$vars
              nVars <- length(vars)
              key   <- image$key
              split <- self$options$splitROC && !is.na(key)

              levels<- c("")
              if (!is.na(key)) {
                if (self$options$sepROC) {
                  grp    <- self$options$groups
                  grps   <- jmvcore::select(dat, grp)
                  levels <- base::levels(grps[!is.na(grps[[grp]]), grp])
                  vars <- levels
                } else {
                  vars   <- c(key)
                }
              }

              # data subset
              subs <- ""
              self$results$text$setContent(" ")
              if (!is.null(self$options$groups) && !self$options$sepROC) {
                dat <- dat[dat[[self$options$groups]] == self$options$selgroup, , drop=FALSE]
              }
              cls   <- jmvcore::select(dat, self$options$class)
              lev   <- base::levels(cls[!is.na(cls[[self$options$class]]), self$options$class])

              d   <- 0
              add <- FALSE
              AUC <- NULL
              DIR <- NULL
              TR  <- NULL
              TC  <- NULL
              oROC<- NULL
              dL  <- self$options$cmpDeLong

              cols <- jmvcore::colorPalette(n=nVars, theme$palette, type="fill")
              if (self$options$palBrewer!="none")
                cols <- RColorBrewer::brewer.pal(n=nVars, name=self$options$palBrewer)
              if (self$options$BW && self$options$dotline) cols <- rep(1, nVars)
              nVars <- length(vars)

              for (var in vars) {
                d <- d+1

                if (split) d <- which(sapply(self$options$vars, function(X) key %in% X))[[1]]

                if (is.na(key) || split) {
                  kk <- ""
                  dd <- dat
                  pair <- TRUE
                  frm <- stats::as.formula(jmvcore::composeFormula(self$options$class, var))
                } else if (!is.na(key)) {
                  kk  <- self$options$groups
                  dd <- dat[dat[[self$options$groups]] == var, , drop=FALSE]
                  pair <- FALSE
                  frm <- stats::as.formula(jmvcore::composeFormula(self$options$class, image$key))
                }

                roc <- pROC::roc(frm, data=dd, percent=self$options$perc)
                rci <- pROC::ci(roc, conf.level=self$options$ciWidth/100)

                AUC <- rbind(AUC, c(rci))
                l1  <- substr(lev[1],1,1); l2  <- substr(lev[2],1,1)
                if (l1==l2) {
                 l1  <- substr(lev[1],1,2); l2  <- substr(lev[2],1,2)
                }
                DIR <- rbind(DIR, ifelse(self$options$direction,
                    paste(", ", l1, roc$direction, l2, sep=""),""))

                if (dL!="none") {
                  if (dL == "w05") {
                    null_auc <- ifelse(self$options$perc, 50, 0.5)
                    variance <- pROC::var(roc, method="delong")
                    if (variance <= 0) {
                      if (roc$auc == null_auc) {
                        pval <- 1.0
                      } else {
                        pval <- 0.0001
                      }
                    } else {
                      z <- (roc$auc - null_auc) / sqrt(variance)
                      pval <- 2 * stats::pnorm(-abs(z))
                    }
                    pv <- private$.formatElement(pval)
                    pv <- gsub(" ", "0", pv)
                    if (self$options$mAst)
                      TR <- rbind(TR, paste(" ",private$.asterisk(pval)))
                    else
                      TR <- rbind(TR, paste(", p", ifelse(pval>0.001,"=",""),
                            pv, sep=""))
                    if (pval<0.05 && !self$options$BW) TC <- rbind(TC, "red")
                    else TC <- rbind(TC, "black")
                  } else if (add) {
                    rt <- pROC::roc.test(roc, oROC, paired=pair,
                            parallel=TRUE, method="d", alternative="two.sided")
                    pv <- private$.formatElement(rt$p.value)
                    pv <- gsub(" ", "0", pv)
                    if (self$options$mAst)
                      TR <- rbind(TR, paste(" ",private$.asterisk(rt$p.value)))
                    else
                      TR <- rbind(TR, paste(", p", ifelse(rt$p.value>0.001,"=",""),
                            pv, sep=""))
                    if (rt$p.value<0.05 && !self$options$BW) TC <- rbind(TC, "red")
                    else TC <- rbind(TC, "black")
                  } else {
                    TR <- rbind(TR, "")
                    TC <- rbind(TC, "black")
                  }
                  if (dL!="wcontr" || is.null(oROC)) oROC <- roc
                } else TR <- ""
                fmt  <- ifelse(self$options$perc,'%#.1f%%','%#.3f')

                fauc <- paste(.("AUC "), fmt)
                if (self$options$ciAUC)
                    fauc <- paste(fauc, " [",  sprintf(gsub("%%","",fmt), AUC[,1]),
                    "-",  sprintf(gsub("%%","",fmt), AUC[,3]), "]", sep="")
                if (self$options$perc) fauc <- paste(gsub("%%","",fauc),"%%",sep="")
                if (dL == "w05" && nVars < 2) {
                  fauc <- paste(fauc, TR[d], sep="")
                }

                main <- ifelse(nVars<2, var, "")
                main <- ifelse(self$options$direction,
                    ifelse(nVars<2, paste(main, " (", lev[1], roc$direction, lev[2], ")", sep=""),
                    paste(lev, collapse=' / ')), main)
                main <- ifelse(subs>"", paste0(main, " (", subs, ")"), main)
                main <- ifelse(is.na(key)||split, main,
                            paste0(image$key, ifelse(main>"", paste0(" {", main, "}"),"")))

                bss  <- ifelse(self$options$theBest %in% c("bestY","bssY"), "youden", "closest.topleft")
                best <- pROC::coords(roc, "best", best.method=bss, transpose=TRUE)[1]

                p <- pROC::plot.roc(frm, data=dd, col=cols[d],
                    main=main, cex.main=1.3,
                    percent=self$options$perc,
                    lty=ifelse(self$options$dotline, d, 1),
                    cex.lab=1.5, cex.axis=1.3, lwd=self$options$lwd,
                    auc.polygon=self$options$polygon, auc.polygon.col=cols[d], auc.polygon.density=15,
                    print.auc=nVars<2, print.auc.cex=1.3,
                    print.auc.pattern=fauc,
                    print.thres=ifelse(self$options$theBest=="none", FALSE, TRUE),
                    print.thres.col=cols[d],
                    print.thres.pch=19, print.thres.cex=1.3,
                    print.thres.pattern=ifelse(self$options$theBest %in% c("bssC","bssY"), paste("%.2f"," (",fmt,", ",fmt,")", sep=""), "%.2f"),
                    print.thres.best.method=bss,
                    legacy.axes=self$options$legacy,
                    xlab=ifelse(roc$percent, ifelse(self$options$legacy, .("100 - Specificity (%)"), .("Specificity (%)")), ifelse(self$options$legacy, .("1 - Specificity"), .("Specificity"))),
                    ylab=ifelse(roc$percent, .("Sensitivity (%)"), .("Sensitivity")),
                    conf.level=self$options$ciWidth/100,
                    ci=(self$options$ciAUC && self$options$ciMark && self$options$theBest!="none"),
                    ci.col=cols[d],
                    of="thresholds", thresholds=best,
                    ci.type=c("bars", "shape", "no")[2],
                    grid=!add, add=add
                    )
                if (!add) add <- TRUE
              }
              if (nVars>1) {
                  fmt <- ifelse(self$options$perc,'%#.1f','%#.3f')
                  au <- sprintf(AUC[,2], fmt=fmt)
                  ci <- paste(sprintf(AUC[,1], fmt=fmt), "-", sprintf(AUC[,3], fmt=fmt), sep="")
                  if (self$options$ciAUC) au <- paste(au, " [", ci, "]", sep="")
                  tit <- paste(kk, .(" (AUC"), 
                    ifelse(self$options$ciAUC, jmvcore::format(.(" [{prc}% CI]"), prc=self$options$ciWidth),""),
                    ifelse(self$options$perc, "%",""), ")", sep="")
                  if (self$options$perc) au <- paste(au, "%", sep="")

                  legend(title=tit, "bottomright",
                    cex=1.2, lwd=self$options$lwd, col=cols,
                    bg=ifelse(self$options$polygon || self$options$legBox, "white", "transparent"),
                    box.lwd=ifelse(self$options$polygon || self$options$legBox, 1, 0),
                    lty=1:ifelse(self$options$dotline, length(self$options$vars), 1),
                    text.col=TC,
                    legend=paste(vars, " (", au, ")", DIR, TR, sep="")
                    )
              }

              print(p)
              return(TRUE)
          }, error = function(e) {
              return(FALSE)
          })
          return(result)
      },

      .errorCheck = function() {
          if (is.null(self$options$class)) return(FALSE)
          class  <- self$options$class
          column <- self$data[[class]]

          if (length(levels(column)) != 2) {
            jmvcore::reject(
              jmvcore::format(
                .("The classifying variable '{}' does not have exactly two levels; ROC classification can only be performed on classifying variables with two levels."),
                  class
                 ),
              code=''
            )
          }
      },

      .asterisk=function(pval) {
	if (pval < .001)     return("***")
	else if (pval < .01) return("**")
	else if (pval < .05) return("*")
      },

      .formatElement = function(val) {
          if (is.na(val)) return("")
          if (val < 0.001) return("< .001")
          return(sprintf("%.3f", val))
      }
    )
)
