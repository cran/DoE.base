##*******************************

## auxiliary functions

## function for checking matrix for being 0
null.check <- function(mat){
  isTRUE(all.equal(mat, 0*mat, check.attributes=FALSE))
}
## function for checking orthogonality and full rank
orth.check <- function(mat){
  ## TRUE, if orthogonal full column rank matrix
  cm <- crossprod(mat)
  if (nrow(cm)==1) return(TRUE)
  else {
    dd <- diag(cm)
    if (any(round(dd,8)==0)) return(FALSE)
    return(isTRUE(all.equal(diag(dd), cm, check.attributes=FALSE)))
  }
}

## generic
halfnormal <- function(x, ...){
    UseMethod("halfnormal")
    }

halfnormal.default <- function(x, labs, codes=NULL, pch=1, alpha=0.05, 
                       xlab="absolute effects", large.omit=0, plot=TRUE, 
                       crit=NULL, ...){
  ## function returns codes of significant effects
  effects <- abs(x)
  if (is.null(codes)) codes <- labs
  labord <- order(effects)
  effects <- effects[labord]
  n <- length(effects)
  labs <- labs[labord][1:(n-large.omit)]
  codes <- codes[labord][1:(n-large.omit)]
  effects <- effects[1:(n-large.omit)]
  n <- n - large.omit
  if (!identical(codes, labs)){ 
    haupteff <- setdiff(1:length(labs), grep(":", labs)) 
    legende <- paste(codes[haupteff], labs[haupteff], sep="=",collapse=", ")
  }
  else legende <- ""
  if (length(pch)>1) {
    if (length(pch)<n) stop("pch must have length 1 or length n")
    pch <- pch[labord]
  }
  ui <- qnorm(0.5+ppoints(n, a=1/2)/2)    ## modified August 14 2013
  codes <- paste(rep("  ",n), codes, sep="")
  
  if (is.null(crit)) crit <- ME.Lenth(effects, alpha=alpha)$ME
    
  nlab <- sum(effects > crit)
  if (plot){
  plot(effects, ui, ylab = "Half-normal scores", xlab = xlab, sub=legende, pch=pch, ...)
  
  if (nlab > 0)
  text(effects[(n - nlab + 1):n], ui[(n - nlab + 1):n], 
       codes[(n - nlab + 1):n], adj=0, xpd=NA)
  }
  if (nlab > 0) aus <- labs[(n - nlab + 1):n] else aus <- character(0)
  aus
}

halfnormal.lm <- function(x, labs=NULL, code=FALSE, pch=NULL, 
            alpha=0.05, xlab="absolute effects", 
            large.omit = 0, plot=TRUE,
            keep.colons=!code, ME.partial=FALSE, 
            external.pe=NULL, external.center=FALSE, 
            contr.center="contr.poly", pch.set=c(1,16,8), 
            scl=NULL, method="Lenth", ...){
  ## function for plotting effects from linear model
  ## on half-normal effects plot
  ## ME.partial=FALSE (TRUE restricts to main effects and intercept)
  ## the function will orthogonally supplement missing degrees of freedom
  ## will add orthogonal pure error points from external replication sets
  ## will add external nonlinearity effect from these points, if external.center=TRUE
  ## user is responsible to only request this, when reasonable
  ## contr.center can be one of "contr.poly" or "contr.helmert"
  ## pch.set sets default point characters for experimental effects, 
  ## structural error points (incl. nonlinearity), 
  ## and pure error points
  ## 
  ## large.omit allows to zoom in on smaller effects
  ## scl gives the desired squared column length (default: overall run number N)
  ## method can be "Lenth", "LW98" or "EM08"
  
  linmod <- x
  if (!"lm" %in% class(linmod)) stop("x must be a linear model object")
  
  if (!length(pch.set)==3) stop("pch.set must have 3 elements")
  if (!is.numeric(pch.set)) stop("pch.set must be numeric")
  if (!all(pch.set%%1==0)) stop("pch.set must be integer-valued")
  if (!is.logical(keep.colons)) stop("keep.colons must be logical")
  if (!is.logical(external.center)) stop("external.center must be logical")
  if (is.null(external.pe)) if (external.center) stop("external.center must not be TRUE without external.pe specified")
  if (!contr.center %in% c("contr.poly", "contr.helmert")) stop("invalid contr.center")
  if (!is.numeric(large.omit)) stop("large.omit must be a number")
  if (!large.omit >= 0) stop("large.omit must be non-negative")
  if (!large.omit%%1==0) stop("large.omit must be an integer")
  if (!is.null(scl)){
  if (!is.numeric(scl)) stop("scl must be numeric")
  if (!length(scl)==1) stop("scl must be a single number")
  if (!scl>0) stop("scl must be positive")
  }
  if (!method %in% c("Lenth","LW98","EM08")) stop("invalid method")

  ## initialize  
  orth.supp <- FALSE  ## are additional columns needed for saturating the array?
  p <- linmod$rank
  mm <- model.matrix(linmod)
  ## preparation for separating lack of fit and pure error effects
  pats <- apply(mm, 1, function(obj) paste(obj, collapse=":"))
  pats <- as.factor(pats)
  
  N <- nrow(mm)
  dfr <- linmod$df.residual
  nce <- 0 ## initialize number of external points (center or other)
  if (dfr > p) 
    warning("halfnormal not recommended for models with more residual df than model df")
  if (dfr > 0) orth.supp <- TRUE
  
  piv <- linmod$qr$pivot
  cols.in <- piv[1:p]  ## model matrix columns for estimated coefficients incl intercept
                       ## however, all effects have to be considered with ALL their columns
  cols.out <- numeric(0)
  if (length(piv) > p) cols.out <- piv[(p+1):length(piv)]
  
  mod.terms <- terms(linmod)
  mod.effs <- attr(mod.terms,"term.labels")  ## both complete in original effect order, 
  mod.orders <- attr(mod.terms,"order")      ## before pivoting, excluding intercept
  if (ME.partial) {
    mod.effs <- c("(Intercept)", mod.effs)
    mod.orders <- c(0,mod.orders)
  }
  neffs <- length(mod.orders)                ## number of effects (including confounded ones)
  nme <- sum(mod.orders<=1)                  ## number of main effects or main effects with intercept
  resp <- linmod$model[,attr(mod.terms,"response")]  ## response data
  
  ## check for intercept
  if (!attr(mod.terms,"intercept")==1) stop("only applicable for models with intercept")

  asgn <- linmod$assign        ## including the 0 for the intercept 
  if (ME.partial){ 
    asgn <- asgn + 1           ## include intercept in effect numbering
    dfs <- table(asgn)         ## tabulate numbers of effects columns
    }        
  else
    dfs <- table(asgn)[-1]       ## excluding intercept
  if (!length(dfs) == neffs) stop("something went wrong")
  
  ## original column order, referring to position including intercept 
  segments <- mapply(":",cumsum(c(0, dfs[-neffs])) + 1, cumsum(dfs))
  if (!ME.partial) segments <- lapply(segments, function(obj) obj+1)
  else segments <- as.list(segments)
  
  names(segments) <- mod.effs
  mecols <- unlist(segments[which(mod.orders<=1)]) 
        ## model matrix columns that hold main effects
        ## or main effects and intercept in case of ME.partial
  
  ## complete aliasing check for main effects
  if (length(piv) > p) 
    if (length(intersect(piv[(p+1):max(piv)], mecols)) > 0)
      stop("completely aliased main effect")
  
  ## partial aliasing check for main effects
  if (!ME.partial)
  if (!orth.check(mm[,mecols])) stop("partially aliased main effect")

  ali <- stats::alias(linmod, partial = TRUE)
  ## create list for aliased later effects
  ## initialize
  ali.effs <- vector("list", neffs)
  names(ali.effs) <- mod.effs
  ## populate
    ali <- round(crossprod(mm),8)
    ali <- ali - diag(diag(ali))  ## elminate non-zero diagonal elements
    namboth <- 1:ncol(ali)
    colnames(ali) <- rownames(ali) <- namboth
    for (i in 1 : neffs){
      cur <- segments[[i]]
      cur.in <- intersect(cols.in, cur)
      cur.out <- intersect(cols.out, cur)
      ## assuming that effects that were completely pivoted out
      ##    are behind all relevant aliased effects
      if (length(cur.in)==0) ali.effs[[i]] <- integer(0)
      else {
        ## column ids of coefficient contrasts aliased with any from cur effect
        hilf <- apply(abs(ali[, as.character(cur), drop = FALSE]), 1, max)
              ## row maxima of absolute entries
        hilf <- setdiff(namboth[which(hilf > 0)], cur)
              ## column numbers for other effects aliased with current
        hilf <- unique(setdiff(asgn[hilf],1:i))
              ## later effects aliased with this
        names(hilf) <- mod.effs[hilf]
        ali.effs[[i]] <- hilf      
      }    
    }
  ### prerequisites are fixed now
  
  segnew <- segments  ## list of eventually active columns
                      ## column numbers of mm are kept unchanged during algorithm
                      ## dimension reduction done later with segnew
  prcnew <- vector("list", neffs)  ## initialized as NULL each
  names(prcnew) <- mod.effs
  for (i in 1 : neffs){
    hilfx <- mm[, segments[[i]], drop=FALSE]
      ## algorithm step 1a
      if (null.check(hilfx)) segnew[[i]] <- numeric(0)
      else{
        ## at least one df for the effect
        pick <- 1 : dfs[i]       ## allow later use of pick
                                 ## regardless of prcomp use
          ## checks unnecessary for main effects and Intercept
          ## except for partial aliasing of main effects
            nami <- paste(as.character(mod.effs[i]), pick, sep="")
        if (i > nme || ME.partial){
          if (!orth.check(hilfx)){
            ## less than full column rank, or non-orthogonal
            colnames(mm)[segments[[i]]] <- nami     ## replace column names
            hilf <- prcomp(hilfx)
            pick <- round(hilf$sdev,8) > 0
            segnew[[i]] <- segments[[i]][pick]  ## prepare later pick
            colnames(hilf$x) <- nami
            mm[, segments[[i]]] <- hilf$x                   ## replace all columns
            prcnew[[i]] <- hilf$rotation[, pick, drop=FALSE]     ## store for docu
            hilfx <- hilf$x[, pick, drop=FALSE]                ## reduce dimension
          }
          else{
            if (i>1 && mod.effs[[i]] %in% unlist(ali.effs[1:(i-1)])) 
                prcnew[[i]] <- diag(dfs[i])   
               ## cover the situation for which no orthogonalization 
               ## or dimension reduction is needed after projecting out other effects
          }
        }
        ## ensure squared column length N or scl
        sql <- colSums(hilfx ^ 2)
        if (is.null(scl)) scl <- N
        if (!isTRUE(all.equal(sql, rep(scl, ncol(hilfx)), check.attributes=FALSE))){
           hilfx <- hilfx %*% diag(sqrt(scl/sql), ncol(hilfx))
           mm[,segments[[i]][pick]] <- hilfx
        }
        ## projecting out from later effects (step 1b)
        if (length(ali.effs[[i]]) > 0){
          hilfy <- mm[,unlist(segments[ali.effs[[i]]]), drop=FALSE]
          if (ncol(hilfy) > 0)
          mm[,unlist(segments[ali.effs[[i]]])] <- 
              hilfy - hilfx%*%solve(crossprod(hilfx),crossprod(hilfx,hilfy))
        }
    }
  }
  ## collate new mm
  mm <- mm[,unlist(segnew)]
  
  if (!ME.partial) mm <- cbind(1, mm)
  ## orthogonally supplement, if necessary
  dflof <- 0
  dfpe <- 0
  if (orth.supp){
    ## project out all potential experimental effects including lack of fit 
    mm.hilf <- model.matrix(~pats)
    dfpe <- N - ncol(mm.hilf)
    if (dfpe > 0){
     ## add pure error columns
     supp.pe <- diag(N) - mm.hilf%*%solve(crossprod(mm.hilf), t(mm.hilf))
     hilf <- prcomp(supp.pe)
     supp.pe <- hilf$x[, round(hilf$sdev,8)>0, drop=FALSE]
     supp.pe <- supp.pe %*% diag(sqrt(scl/colSums(supp.pe^2)), nrow=dfpe)
     mm.hilf <- cbind(mm, supp.pe)
    }
    else mm.hilf <- mm
    supp <- diag(N) - mm.hilf%*%ginv(crossprod(mm.hilf))%*%t(mm.hilf)
    hilf <- prcomp(supp)
    supp <- hilf$x[, round(hilf$sdev,8)>0, drop=FALSE]
    if (!ncol(supp) == dfr - dfpe) stop("something unexpected happened")
    dflof <- dfr - dfpe
    supp <- supp %*% diag(sqrt(scl/colSums(supp^2)), nrow=dflof)

#    loftest <- round(colSums(abs(t(model.matrix(~pats))%*%supp)),4) > 0
#    dflof <- sum(loftest)
    if (dflof > 0 && dfr > dflof){
       supp <- cbind(supp, supp.pe)
       namsupp <- c(paste("lof",1:dflof,sep=""),paste("e",1:(dfr-dflof),sep=""))
    }
    else{
       if (dflof==0) namsupp <- paste("e", 1:dfr, sep="")
       else namsupp <- paste("lof", 1:dflof, sep="")
    }
    colnames(supp) <- namsupp
    mm <- cbind(mm, supp)
  } 
  projectout <- vector("list", neffs)
  names(projectout) <- mod.effs
  for (i in 1:neffs) projectout[[i]] <- which(sapply(ali.effs, function(obj) i %in% obj))
  coeff <- solve(crossprod(mm), crossprod(mm, resp))[-1]
  if (is.null(labs) || code) 
    labs <- colnames(mm)[-1]
    
    
  ## coding
  ## replace main effect names by codes in all effects
  ################################################################################
      if (code){
            max.order <- max(mod.orders)
            no.factors <- nme
            factor.label <- attr(x$terms, "term.labels")[mod.orders == 1]
            faclet <- c(LETTERS[-9],letters[-9])
            factor.code <- faclet[1:nme]
            for (i in 1:nme) labs <- gsub(factor.label[i], factor.code[i], labs)
            
            ## legend preparation
            ## removed block treatment (in comparison to DanielPlot function)
            texto <- paste(factor.code[1], "=", factor.label[1])
            for (i in 2:nme) {
                texto <- paste(texto, ", ", factor.code[i], "=", 
                  factor.label[i])
            }
    }
    ##########################################################################
  
  if (!keep.colons) labs <- sapply(sapply(labs,strsplit,":",fixed=TRUE),paste,collapse="")
  
  names(coeff) <- labs
  
  ###############################################
  ### handle EXTERNAL pure error or center points
  ###############################################
  if (!is.null(external.pe)){
    if (!is.numeric(external.pe)) {
      warning("external.pe must be numeric, option was ignored")
      break
    }
    else{
      nce <- length(external.pe)
      if (external.center){
        nonlinear <- (mean(external.pe) - mean(resp))*sqrt(nce/(N+nce))*sqrt(N/scl)
        coeff <- c(coeff, nonlinear)
        if (length(labs) == N - 1) labs <- c(labs, "nonlinear")
      }
      
      if (contr.center=="contr.poly")
        hilf <- contr.poly(nce) * sqrt(N)
      else 
        hilf <- contr.XuWu(nce) / sqrt(nce)  * sqrt(N/scl)
      extpts <- solve(crossprod(hilf),crossprod(hilf,external.pe))
      if (length(labs) %in% c(N - 1, N)) 
            labs <- c(labs, paste("pe", 1 : (nce-1), sep = ""))
      coeff <- c(coeff, extpts)
    }
    names(coeff) <- labs
  }
  
  ## automate error point symbol labeling
  if (is.null(pch) || length(pch==1)){
    if (is.null(pch)) pch <- pch.set[1]
    adderr <- dfr - dflof
    addnonlin <- dflof
    if (nce > 0) {
      adderr <- adderr + nce - 1
      if (external.center) addnonlin <- addnonlin + 1  ## prepare for external center point plus other augmentation
                                                       ## not yet functional 
    }
    if (external.center)
      pch <- c(rep(pch, p - 1), rep(pch.set[2], addnonlin-1), rep(pch.set[3], adderr-nce+1), pch.set[2], rep(pch.set[3], nce-1))   
    else
      pch <- c(rep(pch, p - 1), rep(pch.set[2], addnonlin), rep(pch.set[3], adderr))
  }
  
  crit <- NULL
  ## obtain standard error, dfe and critical value for the other methods
  if (method %in% c("LW98","EM08")){ 
     errpos <- pch==pch.set[3]
     if (sum(errpos)==0)
        warning("no pure error degrees of freedom, method choice was invalid")
     else{
     sterr <- sqrt(sum(coeff[errpos]^2)/adderr)
     if (method=="LW98")
        crit <- CME.LW98(coeff[!errpos], sterr, adderr)
     if (method=="EM08")
        crit <- CME.EM08(coeff[!errpos], sterr, adderr)
     if (as.character(alpha) %in% names(crit$CME)) crit <- crit$CME[as.character(alpha)]
     else crit <- qt(1-alpha/2, sum(!errpos)/3)*crit$CPSE
     }
   }
  
  signif <- halfnormal(coeff, labs=labs, codes=labs, pch=pch, alpha=alpha, large.omit=large.omit, crit=crit, ...)
  ## add legend
  if (code) mtext(side = 1, line = par("mar")[1]-1, texto, cex = 1)

  projected <- sapply(projectout, function(obj) length(obj)>0)
  projected <- names(projected)[projected]
  ali.complete <- sapply(segnew, function(obj) length(obj)==0)
  
  if (!all(nulls <- sapply(projectout[!ali.complete], function(obj) length(obj)==0))) 
    for (i in which(!nulls)){
      hilf <- paste(strsplit(mod.effs[i],":",fixed=TRUE)[[1]],collapse="")
      cat(paste("Creation of", hilf,"\n"))
      cat(paste("Projected out:", 
          paste(sapply(sapply(names(projectout[[i]]),strsplit,":",fixed=TRUE),paste,collapse=""),collapse=","),"\n"))
      if (!is.null(prcnew[[i]])){
        if (nrow(prcnew[[i]])>1){
        cat("Effects columns are the following linear combinations of residuals:\n")
        colnames(prcnew[[i]]) <- paste(hilf, 1:ncol(prcnew[[i]]), sep="")
        print(round(prcnew[[i]],4))
        }
      }
    }
  if (sum(ali.complete) > 0) {
    cat("\nThe following effects are completely aliased:\n")
    print(names(ali.complete[ali.complete]), quote=FALSE)
  }
  if (length(signif)==0) cat("no significant effects\n")
  else {
    cat("significant effects:\n")
    print(signif)
    }
  invisible(list(coef=coeff, mm=mm, mod.effs=mod.effs, res=projectout[projected], LCs=prcnew[projected], alpha=alpha, method=method, signif=signif, pchs=pch))
}

halfnormal.design <- function(x, labs=NULL, code=FALSE, pch=NULL, 
            alpha=0.05, xlab="absolute effects", 
            large.omit = 0, plot=TRUE,
            keep.colons=!code, ME.partial=FALSE, 
            external.pe=NULL, external.center=FALSE, 
            contr.center="contr.poly", pch.set=c(1,16,8), 
            scl=NULL, method="Lenth", ...){

 if (!"design" %in% class(x)) stop("x must be of class design")
 
 halfnormal(lm(x, use.center=TRUE), labs=labs,code=code, pch=pch, alpha=alpha, xlab=xlab,
    large.omit=large.omit, plot=plot, keep.colons=keep.colons, ME.partial=ME.partial,
    external.pe=external.pe, external.center=external.center,
    contr.center=contr.center, pch.set=pch.set, scl=scl, method=method, ...)
}