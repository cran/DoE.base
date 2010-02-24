length4 <- function(design, with.blocks=FALSE){
    ## function to calculate generalized words of length 4
    ## according to Xu and Wu 2001 Annals

    ## it might be helpful to locate non-zeros
    ## this is so far not done
    n <- nrow(design)
    if (!(is.data.frame(design) | is.matrix(design))) stop("design must be a data frame or a matrix")
    if (is.matrix(design)) design <- as.data.frame(design)
    if (!"design" %in% class(design)){ 
        for (i in 1:ncol(design)){ 
            design[,i] <- factor(design[,i])
            contrasts(design[,i]) <- "contr.helmert"
        }
        nlevels <- sapply(as.list(design), function(obj) nlevels(obj))
        ##names(nlevels) <- colnames(design)
        fo <- formula("~.", data=design)
        }
    else{
        di <- design.info(design)
        nlevels <- di$nlevels
        if (is.null(nlevels)){
            if (length(grep("FrF2",di$type))>0 | length(grep("pb",di$type))>0 )
                nlevels <- rep(2,length(di$factor.name))
        }
        ## orthogonal contrasts
        design <- change.contr(design, "contr.helmert")
    
        ## if blocked and requested, accomodate blocks
        if (with.blocks & !is.null(di$block.name)){
          if (!is.factor(design[[di$block.name]])) design[[di$block.name]] <- factor(design[[di$block.name]])
          contrasts(design[[di$block.name]]) <- "contr.helmert"
          fo <- formula(paste("~",paste(c(di$block.name,names(di$factor.names)),collapse="+")), data=design)
          nlevels <- c(di$nblocks,nlevels)
        }
        else
          fo <- formula(paste("~",paste(names(di$factor.names),collapse="+")), data=design)
    }

    ## create model matrix
        
    mm <- model.matrix(fo,design)
    ## store column allocation to factors
    zuord <- attr(mm, "assign")
    mm <- sqrt(n)*mm %*% diag(1/sqrt(colSums(mm^2)))
    ## remove intercept column
    mm <- mm[,-1]
    zuord <- zuord[-1]
    ##colnames(mm) <- zuord
    nfac <- max(zuord)
    if (nfac<4) return(0)

    ## 4fi columns
    quadruples <- nchoosek(nfac, 4)
    anz <- 0
    for (i in 1:ncol(quadruples))
       anz <- anz+prod(nlevels[quadruples[,i,drop=FALSE]]-1)

    mm4 <- matrix(NA,nrow(design), anz)
    ##colnames(mm4) <- rep("sp",ncol(mm4))

    zaehl <- 1

    for (i in 1:(max(zuord)-3)){
     icols <- which(zuord==i)
      for (j in (i+1):(max(zuord)-2)){
        jcols <- which(zuord==j)
      for (k in (j+1):(max(zuord)-1)){
        kcols <- which(zuord==k)
      for (l in (k+1):(max(zuord))){
        lcols <- which(zuord==l)
          for (a in icols){
           for (b in jcols){
             for (c in kcols){
             for (d in lcols){
              mm4[,zaehl] <- mm[,a]*mm[,b]*mm[,c]*mm[,d]
              ##colnames(mm4)[zaehl] <- paste(i,j,k,l,sep=":")
              zaehl <- zaehl+1
           }}}}
       }
    }}}
    sum(colSums(mm4)^2)/(n^2)
}