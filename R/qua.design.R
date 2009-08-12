qua.design <- function(design, quantitative=NA, contrasts=character(0), ...){
   ## function to allow switching between numeric and character for all types of design
   ## (some are always numeric, though, e.g. lhs

   if (!"design" %in% class(design)) stop("design must be of class design")
   di <- design.info(design)
   if (di$type %in% c("rsm","lhs")) stop("qua.design does not work for purely quantitative designs")
   fn <- names(di$factor.names)
   qu.old <- di$quantitative
   if (is.null(qu.old)){
       qu.old <- rep(NA, di$nfactors)
       names(qu.old) <- fn
       }
   
   if (!(identical(quantitative,"all") | identical(quantitative,"none") | 
        length(quantitative) %in% c(0,di$nfactors) | (!is.null(names(quantitative))) | 
        identical(quantitative,NA)))
      stop("quantitative must be all, none, NA, a named vector or a vector of length nfactors")
   if (identical(quantitative,"all")) quantitative <- rep(TRUE, di$nfactors)
   if (identical(quantitative,"none")) quantitative <- rep(FALSE, di$nfactors)
   if (identical(quantitative,NA)) quantitative <- rep(NA, di$nfactors)
   if (!all(is.na(quantitative) | is.logical(quantitative)))
        stop("all elements of quantitative must be NA or TRUE or FALSE")
   if (!is.null(names(quantitative))){
       if (length(setdiff(names(quantitative),names(di$factor.names)))>0)
          stop("all names of quantitative must be factor names of the design")
       qu <- quantitative
       quantitative <- qu.old
       quantitative[names(qu)] <- qu
       }
   if (is.null(names(quantitative))) 
        names(quantitative) <- names(di$factor.names)
   di$quantitative <- quantitative
   
   
   hilf <- options("warn")
   options(warn=-1)
   
   nonnum <- sapply(di$factor.names, function(obj) any(is.na(as.numeric(as.character(obj)))))
      ## nonnum are not coercible to numeric
      options(warn=hilf$warn)
  
   
   ## determine default contrasts
   nlevels <- di$nlevels
   if (is.null(nlevels)) nlevels <- rep(2,di$nfactors) ## ist das wirklich immer 2, wenn nicht angegeben?
   defcontrasts <- rep("contr.treatment",di$nfactors)
   defcontrasts[which(!nonnum)] <- "contr.poly"
   defcontrasts[which(nlevels==2)] <- "contr.FrF2"
   names(defcontrasts) <- fn  ## per default, everything is a factor
   
   ## these are currently factors
   nowfactors <- names(di$factor.names)[which(sapply(design,"is.factor"))]
   
   if (length(contrasts)>0){ 
      if (is.null(names(contrasts))) stop("contrasts must be a named vector")
      if (length(setdiff(names(contrasts),fn))>0) stop("invalid factor names for contrasts")
      if (any(sapply(contrasts, function(obj) !is.function(eval(parse(text=obj))))))
          stop("invalid contrast names")
      }
   
   if (any(which(quantitative) %in% which(nonnum)))
      stop("some inherently qualitative factors were wrongly declared quantitative")
   else {
     ## finally, the work starts
     hilf <- undesign(design)
     desnum <- desnum(design)
     ro <- run.order(design)
    
     ## make factor names correct
     di$factor.names[which(quantitative)] <- lapply(di$factor.names[which(quantitative)], "as.numeric")
     di$factor.names[which(!quantitative)] <- lapply(di$factor.names[which(!quantitative)], "as.character")
     di$factor.names[which(is.na(quantitative) & !nonnum)] <- lapply(di$factor.names[which(is.na(quantitative) & !nonnum)], "as.numeric")
     di$factor.names[which(is.na(quantitative) & nonnum)] <- lapply(di$factor.names[which(is.na(quantitative) & nonnum)], "as.character")

     ## assign data as required
     for (i in 1:di$nfactors){
         ## NA
         if (is.na(quantitative[fn[i]])){
            hilf[,fn[i]] <- factor(hilf[,fn[i]], levels = di$factor.names[[i]])
            contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(defcontrasts[fn[i]],"(",nlevels[i],")",sep="")))
            if (defcontrasts[fn[i]]=="contr.poly") 
            contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(defcontrasts[fn[i]],"(",nlevels[i],", scores=sort(di$factor.names[[i]]))",sep="")))
         }
         else{
         ## quantitative (possible, otherwise error above)
         if (quantitative[fn[i]]) hilf[,fn[i]] <- as.numeric(as.character(hilf[,fn[i]]))
         
         ## qualitative
         if (!quantitative[fn[i]]){
            ## only change if wasn't factor before or change in contrast requested 
            if (fn[i] %in% names(contrasts) | !(fn[i] %in% nowfactors) ){
               ## no new contrast requested, i.e. was quantitative before
               if (!fn[i] %in% names(contrasts)){
                       hilf[,fn[i]] <- factor(hilf[,fn[i]], levels=di$factor.names[[i]])
                       contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(defcontrasts[fn[i]],"(",nlevels[i],")",sep="")))
                       if (defcontrasts[fn[i]]=="contr.poly") 
                         contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(defcontrasts[fn[i]],"(",nlevels[i],", scores=sort(di$factor.names[[i]]))",sep="")))
                  }
               else{
                      if (!fn[i] %in% nowfactors) hilf[,fn[i]] <- factor(hilf[,fn[i]], levels=di$factor.names[[i]])
                      ## now is a factor
                      
                      if (fn[i] %in% names(contrasts)){
                         contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(contrasts[fn[i]],"(",nlevels[i],")",sep="")))
                         if (contrasts[fn[i]]=="contr.poly") 
                         contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(contrasts[fn[i]],"(",nlevels[i],", scores=sort(di$factor.names[[i]]))",sep="")))
                         }
                      else{
                         if (!fn[i] %in% nowfactors) 
                         contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(defcontrasts[fn[i]],"(",nlevels[i],")",sep="")))
                         if (defcontrasts[fn[i]]=="contr.poly") 
                         contrasts(hilf[,fn[i]]) <- eval(parse(text=paste(defcontrasts[fn[i]],"(",nlevels[i],", scores=sort(di$factor.names[[i]]))",sep="")))
                      }
                      hilf[,fn[i]] <- factor(hilf[,fn[i]], levels=di$factor.names[[i]])
                      
                  }
            }
         }
         }
     }
     desnum <- model.matrix(formula(paste("~",paste(fn,collapse="+"))),data=hilf)[,-1]
     if (length(setdiff(colnames(hilf),fn))>0){ 
               anhaeng <- as.matrix(hilf[,setdiff(colnames(hilf),fn),drop=FALSE])
               storage.mode(anhaeng) <- "numeric"
               desnum <- cbind(desnum, anhaeng)
               }
     class(hilf) <- c("design","data.frame")
     desnum(hilf) <- desnum
     run.order(hilf) <- ro
     design.info(hilf) <- di
   }
   hilf
}