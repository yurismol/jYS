# This file is a generated template, your changes will not be overwritten

mCORClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "mCORClass",
  inherit = mCORBase,
  private = list(

     .init=function() {
        #if (grepl("Russian", Sys.getlocale(), fixed=TRUE)) options(OutDec=",")
        #Sys.setlocale('LC_ALL', 'russian')
        matrix <- self$results$matrix
        vars   <- self$options$vars
        nVars  <- length(vars)
        ciw    <- self$options$ciWidth
        mtord <- FALSE

	#grp    <- self$options$group
	#if (!is.null(grp)) {
        #  grps  <- jmvcore::select(self$data, grp)
        #  groupLevels <- base::levels(grps[!is.na(grps[[grp]]), grp])
	#} else {
	#  groupLevels <- c("")
	#}

        #for (g in groupLevels) {
          for (i in seq_along(vars)) {
            var <- vars[[i]]
            matrix$addColumn(name=paste0(var, '[r]'), title=var,
                type='number', format='zto')
            matrix$addColumn(name=paste0(var, '[cil]'), title=var,
                type='number', format='zto', visible='(method=="pearson" && ci)')
            matrix$addColumn(name=paste0(var, '[ciu]'), title=var,
                type='number', format='zto', visible='(method=="pearson" && ci)')
            matrix$addColumn(name=paste0(var, '[p]'), title=var,
                type='number', format='zto,pvalue', visible='(pval)')
            matrix$addColumn(name=paste0(var, '[n]'), title=var,
                type='integer', visible='(n)')
          }
        #}

        for (i in seq_along(vars)) {
            var <- vars[[i]]
            values <- list()

            for (j in seq(i, nVars)) {
                v <- vars[[j]]
                values[[paste0(v, '[r]')]]   <- ''
                values[[paste0(v, '[cil]')]] <- ''
                values[[paste0(v, '[ciu]')]] <- ''
                values[[paste0(v, '[p]')]]   <- ''
                values[[paste0(v, '[n]')]]   <- ''
            }

            values[[paste0(var, '[r]')]]   <- '\u2014'
            values[[paste0(var, '[cil]')]] <- '\u2014'
            values[[paste0(var, '[ciu]')]] <- '\u2014'
            values[[paste0(var, '[p]')]]   <- '\u2014'
            values[[paste0(var, '[n]')]]   <- '\u2014'

            values[['.stat[ciu]']] <- jmvcore::format(.('{ciWidth}% CI Upper'), ciWidth=ciw)
            values[['.stat[cil]']] <- jmvcore::format(.('{ciWidth}% CI Lower'), ciWidth=ciw)
            rw <- matrix$setRow(rowKey=var, values)
  
        }
        hyp <- self$options$get('hyp')
        flag <- self$options$get('flag')
        if (hyp=='pos') {
            matrix$setNote('hyp', .('H\u2090 is positive correlation'))
            hyp <- 'greater'
            if (flag)
                matrix$setNote('flag', .('* p < .05, ** p < .01, *** p < .001, one-tailed'))
        } else if (hyp=='neg') {
            matrix$setNote('hyp', .('H\u2090 is negative correlation'))
            hyp <- 'less'
            if (flag)
                matrix$setNote('flag', .('* p < .05, ** p < .01, *** p < .001, one-tailed'))
        } else {
            matrix$setNote('hyp', NULL)
            hyp <- 'two.sided'
            if (flag)
                matrix$setNote('flag', '* p < .05, ** p < .01, *** p < .001')
        }
        if (self$options$adjust!='none') {
            matrix$setNote("adjust", jmvcore::format(.("Simultaneous multiple correlation comparisons using {n} method"), n=self$options$adjust))
        }
     },

     .run = function() {
        options(OutDec=",")
        matrix <- self$results$matrix
        vars   <- self$options$vars
        nVars  <- length(vars)
        hyp    <- self$options$hyp

        corr   <- matrix(, nrow=nVars, ncol=nVars); corr[1, 1] <- 1
        colnames(corr) <- vars; rownames(corr) <- vars
        corp   <- matrix(, nrow=nVars, ncol=nVars); corp[1, 1] <- 0
        colnames(corp) <- vars; rownames(corp) <- vars
        if (hyp=='pos') hyp <- 'greater'
        else if (hyp=='neg') hyp <- 'less'
        else hyp <- 'two.sided'
        dat <- self$data
        self$results$text$setContent(" ")

	# data subset
	subs <- ""
	if (!is.null(self$options$group)) {
	   subs <- paste(self$options$group, " == \"", self$options$selgroup,"\"", sep="")
           self$results$text$setContent(paste("<h2>", subs, "</h2>", sep=""))
	   sub <- paste("`", self$options$group, "`==\"", self$options$selgroup,"\"", sep="")
           dat <- subset(dat, eval(parse(text=sub)))
        }

        if (self$options$adjust!='none' && nVars>1) {
            for (i in 1:nVars) {
              rowVarName <- vars[[i]]
              rowVar <- jmvcore::toNumeric(dat[[rowVarName]])
              for (j in 1:nVars) {
                colVarName <- vars[[j]]
                colVar <- jmvcore::toNumeric(dat[[colVarName]])
                if (is.factor(rowVar) || is.factor(colVar)) mtord <- TRUE
                else mtord <- FALSE
                result <- private$.test(rowVar, colVar, hyp, mtord)
                corr[i, j] <- corr[j, i] <- result$r
                corp[i, j] <- corr[j, i] <- result$p
              }
            }
            #if (self$options$hclust) {
            #  hc  <- hclust(dist(corr, method="euclidean"),
            #                method=self$options$clustMet)
            #  vars <- vars[hc$order]
            #}
            pp <- corp[lower.tri(corp, diag=FALSE)]
            pp <- p.adjust(pp, self$options$adjust)
            corp[lower.tri(corp, diag=FALSE)] <- pp

            pp <- corp[upper.tri(corp, diag=FALSE)]
            pp <- p.adjust(pp, self$options$adjust)
            corp[upper.tri(corp, diag=FALSE)] <- pp
            #self$results$pre$setContent(corp)
        }

        if (nVars>1) {
            for (i in 2:nVars) {
                rowVarName <- vars[[i]]
                rowVar <- jmvcore::toNumeric(dat[[rowVarName]])
                ciw <- self$options$ciWidth / 100
                for (j in seq_len(i-1)) {
                    values <- list()
                    colVarName <- vars[[j]]
                    colVar <- jmvcore::toNumeric(dat[[colVarName]])

                    if (is.factor(rowVar) || is.factor(colVar)) mtord <- TRUE
                    else mtord <- FALSE

                    result <- private$.test(rowVar, colVar, hyp, mtord)

                    corr[i, j] <- result$r; corr[j, i] <- result$r; corr[i, i] <- 1
                    if (self$options$adjust=='none') {
                      #self$results$pre$setContent(paste(result))
                      corp[i, j] <- result$p;
                      corp[j, i] <- result$p;
                      corp[i, i] <- 0
                    }

                    values[[paste0(colVarName, '[r]')]]   <- result$r
                    values[[paste0(colVarName, '[cil]')]] <- result$cil
                    values[[paste0(colVarName, '[ciu]')]] <- result$ciu
                    #values[[paste0(colVarName, '[p]')]]   <- result$p
                    values[[paste0(colVarName, '[p]')]]   <- corp[i, j]
                    n <- sum(!(is.na(colVar) | is.na(rowVar)))
                    values[[paste0(colVarName, '[n]')]]   <- n

                    matrix$setRow(rowNo=i, values)

                    if (mtord)
                        matrix$addFootnote(rowNo=i, paste0(colVarName, '[r]'), .('Pearson correlation cannot be calculated for non-numeric values'))
                    flag <- self$options$get('flag')
                    if (flag) {
                        if (mtord)
                            {}  # do nothing
                        else if (result$p < .001)
                            matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '***')
                        else if (result$p < .01)
                            matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '**')
                        else if (result$p < .05)
                            matrix$addSymbol(rowNo=i, paste0(colVarName, '[r]'), '*')
                    }
               }
            }
            cor   <- NULL
            cor$r <- corr
            cor$p <- corp
            cor$s <- subs
            image <- self$results$treeplot
            if (self$options$hclust && nVars>2) {
              hc <- hclust(dist(cor$r, method="euclidean"), method=self$options$clustMet)
              cor$hc <- hc
              cor$r <- cor$r[hc$order, hc$order]
              cor$p <- cor$p[hc$order, hc$order]
              image$setState(cor)
            } else {
              image$setState(NULL)
            }
            image <- self$results$plot
            image$setState(cor)
         }
       },

      .test=function(var1, var2, hyp, mtord) {
        options(OutDec=",")
        results <- list()
        suppressWarnings({
            if (mtord) {
                results$r <- NaN
                results$rp <- NaN
                results$rciu <- NaN
                results$rcil <- NaN
            } else {
                ciw <- self$options$ciWidth / 100
                res <- try(cor.test(as.numeric(var1), as.numeric(var2),
                           alternative=hyp, method=self$options$method,
                           conf.level=ciw, na.action=na.omit))
                if (!base::inherits(res, 'try-error')) {
                    results$r   <- res$estimate
                    results$p   <- res$p.value
                    results$cil <- res$conf.int[1]
                    results$ciu <- res$conf.int[2]
                }
                else {
                    results$r   <- NaN
                    results$rp  <- NaN
                    results$cil <- NaN
                    results$ciu <- NaN
                }
            }

        }) # suppressWarnings

        return(results)
      },

      .plot=function(image, ggtheme, theme, ...) {
	  vars  <- self$options$vars
          nVars <- length(self$options$vars)
          key   <- image$key
          if (is.null(image$state) || nVars<2) return(FALSE)
          c <- image$state

          if (!self$options$hclust && self$options$plotOrder!="original") {
            order <- corrplot::corrMatOrder(c$r, order=self$options$plotOrder)
            c$r   <- c$r[order, order]
            c$p   <- c$p[order, order]
          }

	  vars  <- colnames(c$r)

	  col <- c("#053061", "#2166AC", "#4393C3",
		"#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582",
		"#D6604D", "#B2182B", "#67001F")
	  cols <- hcl.colors(10, palette="Blue-Red 3")
	  if (self$options$signif=="all") {
		insig <- "pch"
		sigl  <- 1.2
	  } else {
		insig <- "blank"
		sigl  <- as.double(self$options$signif)
	  }
	  plotMetU <- self$options$plotMetU
	  plotMetL <- self$options$plotMetL
	  pm <- plotMetU
	  if (pm=="empty") pm = "circle"
	  sl <- sigl
	  ULe <- plotMetU==plotMetL && plotMetU=="empty"
	  if (plotMetU!=plotMetL || ULe) sl=as.double(0)

	  ncex <- 1.8-0.055*nVars
          p <- corrplot::corrplot(c$r, p.mat=c$p,
		tl.cex=1, tl.col="black", diag=FALSE,
		mar=c(0, 0, 1, 0), main=c$s,
		col=col, number.cex=ncex,
		cl.pos=self$options$clPos, cl.cex=1.1,
		sig.level=sl,
		insig=ifelse(plotMetU!=plotMetL || ULe, "blank", insig), pch="",
		method=pm,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="full")

	  if ((plotMetU!=plotMetL && plotMetU!="empty") && !ULe) {
            corrplot::corrplot(c$r, p.mat=c$p, add=TRUE, cl.pos="n", diag=FALSE,
		col=col, number.cex=ncex, tl.pos="n",
		sig.level=sigl, insig=insig, pch="",
		method=plotMetU,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="upper")
          }

	  if ((plotMetU!=plotMetL && plotMetL!="empty") && !ULe) {
            corrplot::corrplot(c$r, p.mat=c$p, add=TRUE, cl.pos="n", diag=FALSE,
		col=col, number.cex=ncex, tl.pos="n",
		sig.level=sigl, insig=insig, pch="",
		method=plotMetL,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="lower")
          }

          par(mar=c(0,0,1,0))
	  lines(c(nVars+0.5, 0.5), rev(c(nVars+0.5, 0.5)), lwd=3, col="lightgrey")
	  posClust <- list()
          if (self$options$hclust && nVars>2) {
            hc <- c$hc
            ct <- cutree(hc, k=ifelse(self$options$numClust>nVars,nVars,self$options$numClust))
            ct <- ct[hc$order]
            ct <- c(ct[!duplicated(ct)], ct[length(ct)])
            posClust <- pmatch(names(ct), vars, nomatch=nVars)
            corrplot::corrRect(p, name=names(ct), lwd=2, add=TRUE)
          } else if (self$options$clustMan>"" && nVars>2) {
            vec <- as.numeric(strsplit(self$options$clustMan, ",")[[1]])
            posClust <- c(1, vec, nVars)
            corrplot::corrRect(p, posClust, lwd=2, add=TRUE)
	  }

          # clusters numbers
          if (length(posClust)>0) {
            coor <- vector()
            io   <- 0
            nCl  <- length(posClust)
            posClust[nCl] <- posClust[nCl]+1
            for (i in posClust[-1]) {
              cw   <- i-io
              coor <- append(coor, cw/2+io)
              io   <- i-1
            }
            Num<- diag(1:(nCl-1))
            dn <- lower.tri(Num, diag=FALSE)
            r  <- (nCl):(nCl^2)
            r  <- r[1:(length(r)/2)]
            Num[dn] <- r; Num <- t(Num); Num[dn] <- r; Num <- t(Num)
            k <- 1
            for (i in coor) {
              for (j in coor) {
                text(j, nVars-i+1, Num[k], cex=1.4, font=4, col="black")
                k <- k + 1
                #lines(c(nVars+0.5, 0.5), rev(c(nVars+0.5, 0.5)), lwd=2, col="lightgreen")
              }
            }

            splAt  <- function(x, pos) unname(split(x, findInterval(x, pos)))
            vec    <- splAt(1:nVars, posClust)
            cmb    <- combn(1:(length(vec)), 2)
            #self$results$pre$setContent(cmb)

            images <- self$results$get('rplots')
            #images <- groups$get(key=i)$assump$resPlots
            #images <- self$results$rplots

            for (i in 1:ncol(cmb)) {
              cm  <- cmb[,i]
              crr <- list()
	      if (length(vec[[cm[1]]])>length(vec[[cm[2]]])) {
                m1 <- vec[[cm[2]]]; m2 <- vec[[cm[1]]]
              } else {
                m1 <- vec[[cm[1]]]; m2 <- vec[[cm[2]]]
              }
              
              crr$r <- c$r[m1, m2, drop=FALSE]
              crr$p <- c$p[m1, m2, drop=FALSE]
	      crr$m <- c$s

	      key <- paste(cm[1], cm[2], sep="_")
              image <- images$addItem(key)
              nm <- jmvcore::format(.("Matrix #{i}"), i=i+nCl-1)
              image$setTitle(nm)
              image$setVisible(visible=TRUE)
              #image$setStatus(c('complete', 'error', 'inited', 'running')[1])
              image$setState(crr)
            }
          }

	  print(p)
	  return(TRUE)
      },

      .rplot=function(image, ggtheme, theme, ...) {
          if (is.null(image$state)) return(FALSE)
          cor   <- image$state
	  col <- c("#053061", "#2166AC", "#4393C3",
		"#92C5DE", "#D1E5F0", "#FDDBC7", "#F4A582",
		"#D6604D", "#B2182B", "#67001F")
	  plotMetU <- self$options$plotMetU
	  ULe  <- plotMetU=="empty"
	  if (ULe) pm = "circle"
	  else pm <- plotMetU
	  if (self$options$signif=="all") {
		insig <- "pch"
		sigl  <- 1.2
	  } else {
		insig <- "blank"
		sigl  <- as.double(self$options$signif)
	  }
          nrow <- nrow(cor$r)
          ncol <- ncol(cor$r)
	  ncex <- 1.8-0.055*max(ncol, nrow)
          #self$results$pre$setContent(nrow)
	  p <- corrplot::corrplot(cor$r, p.mat=cor$p,
		tl.cex=1.5, tl.col="black",
		mar=c(0, 0, ifelse(cor$m>"",1,0), 0), main=cor$m,
		col=col, number.cex=ncex,
		cl.pos="n",	#ifelse(nrow<3,"n",self$options$clPos),
		cl.cex=1.1, cl.ratio=0,
		sig.level=sigl,
		insig=ifelse(ULe, "blank", insig), pch="",
		method=pm,
                addshade=c("negative", "positive", "all")[3], shade.lwd=3,
		type="full")

	  if (self$options$clPos=="r") {
            vert = TRUE
            xlim = c(ncol+0.5, ncol+1.1)
            ylim = c(0.5, nrow+0.5)
	  } else {
            vert = FALSE
            xlim = c(0.5, ncol+0.5)
            ylim = c(0, 0.6)
          }
#	  if (self$options$clPos!="n" && nrow<3)
#	    corrplot::colorlegend(col, labels=c(seq(-1,1,.2)), align="c",
#		cex=1.1, vertical=vert, addlabels=TRUE,
#		xlim=xlim, ylim=ylim, ratio.colbar=0.15)
	  print(p)
	  return(TRUE)
      },

      .treeplot=function(image, ggtheme, theme, ...) {
          nVars  <- length(self$options$vars)
          if (is.null(image$state) || nVars<3) return(FALSE)
          nClust <- ifelse(self$options$numClust>nVars,nVars,self$options$numClust)
          hc <- image$state$hc
          main <- image$state$s
          if (self$options$clustCol) {
            hd <- as.dendrogram(hc)
            cols <- jmvcore::colorPalette(n=nVars, theme$palette, type="fill")
            hd <- dendextend::color_branches(hd, k=nClust)#, col=cols)
            hd <- dendextend::color_labels(hd, k=nClust)	#, col=cols)
            hd <- dendextend::set(hd, "branches_lwd", 3)
            hd <- dendextend::set(hd, "labels_cex", 1.5)
            if (main>"") opar <- par(mar=c(10,0,1,0))
            else opar <- par(mar=c(10,0,0,0))
            p <- plot(hd, axes=FALSE, main=main)
          } else {
            if (main>"") opar <- par(mar=c(0,0,1,0))
            else opar <- par(mar=c(0,0,0,0))
            p <- plot(hc, col=1:10, axes=FALSE, ann=TRUE, cex=1.5,
                 main=main, lwd=2, hang=-1)
          }

          cutree_k_to_h <- function(tree, k) {
            n1 <- nrow(tree$merge)
            n  <- n1 + 1
            mean(tree$height[c(n-k, n-k+1L)])
          }
          abline(h=cutree_k_to_h(hc, nClust), col="red", lwd=2, lty=2)

	  print(p)
	  return(TRUE)
      }
))
