
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
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
      },

      .plot=function(image, ggtheme, theme, ...) {
	  vars  <- self$options$vars
	  nVars <- length(vars)
          key   <- image$key
          split <- self$options$splitROC && !is.na(key)
	  dat   <- image$state$data

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
	  #if (!is.null(self$options$groups) && (!self$options$sepROC || is.na(key))) {
	  if (!is.null(self$options$groups) && !self$options$sepROC) {
	    subs <- paste(self$options$groups, "==\"", self$options$selgroup,"\"", sep="")
            #self$results$text$setContent(paste("<h2>", subs, "</h2>", sep=""))
	    sub <- paste("`", self$options$groups, "`==\"", self$options$selgroup,"\"", sep="")
            dat <- subset(dat, eval(parse(text=sub)))
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
		#https://earlglynn.github.io/RNotes/package/RColorBrewer/index.html
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
              frm <- stats::as.formula(jmvcore:::composeFormula(self$options$class, var))
	    } else if (!is.na(key)) {
	      kk  <- self$options$groups
	      sub <- paste("`", self$options$groups, "`==\"", var,"\"", sep="")
	      dd <- subset(dat, eval(parse(text=sub)))
	      pair <- FALSE
              frm <- stats::as.formula(jmvcore:::composeFormula(self$options$class, image$key))
	    }

	    #self$results$pre$setContent(frm)

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
              if (add) {
                rt <- pROC::roc.test(roc, oROC, paired=pair,
			parallel=TRUE, method="d", alternative="two.sided")
		pv <- jmvcore:::formatElement(rt$p.value, p=TRUE, zto=FALSE, sf=3, dp=3)
		pv <- gsub(" ", "0", pv)
		if (self$options$mAst)
		  TR <- rbind(TR, paste(" ",private$.asterisk(rt$p.value)))
		else
		  TR <- rbind(TR, paste(", p", ifelse(rt$p.value>0.001,"=",""),
			pv, sep=""))
                #TR <- rbind(TR, paste(", p=", signif(rt$p.value, 2), sep=""))
                if (rt$p.value<0.05 && !self$options$BW) TC <- rbind(TC, "red")
                else TC <- rbind(TC, "black")
              } else {
                TR <- rbind(TR, "")
                TC <- rbind(TC, "black")
              }
	      if (dL!="wcontr" || is.null(oROC)) oROC <- roc
            } else TR <- ""
	    fmt  <- ifelse(self$options$perc,'%#.1f%%','%#.3f')

	    fauc <- paste("AUC ", fmt)
	    if (self$options$ciAUC)
		fauc <- paste(fauc, " [",  sprintf(gsub("%%","",fmt), AUC[,1]),
		"-",  sprintf(gsub("%%","",fmt), AUC[,3]), "]", sep="")
	    if (self$options$perc) fauc <- paste(gsub("%%","",fauc),"%%",sep="")

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
		print.auc=nVars<2, print.auc.cex=1.5,
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
		#ci.col=ifelse(ci.type=="bars", par("fg"), "gainsboro"),
		grid=!add, add=add
		)
	    if (!add) add <- TRUE
	  }
	  #if (self$options$ciAUC && split) {
		#ciobj <- pROC::ci.se(p, specificities=seq(0, 100, 5))
		#plot(ciobj, type="shape", col="#1c61b6AA")
		#plot(pROC::ci(p, of="thresholds", thresholds = "best")) # add one threshold
	  #}
	  if (nVars>1) {
	      fmt <- ifelse(self$options$perc,'%#.1f','%#.3f')
	      au <- sprintf(AUC[,2], fmt=fmt)
	      ci <- paste(sprintf(AUC[,1], fmt=fmt), "-", sprintf(AUC[,3], fmt=fmt), sep="")
	      if (self$options$ciAUC) au <- paste(au, " [", ci, "]", sep="")
	      tit <- paste(kk," (AUC", 
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
      }
    )
)
