length3 <- function(design, with.blocks=FALSE){
    ## function to calculate number of generalized words of length 3
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
    else {
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
    ## normalize columns to Euclidean norm sqrt(n)
    mm <- sqrt(n)*mm %*% diag(1/sqrt(colSums(mm^2)))
    ## remove intercept column
    mm <- mm[,-1]
    zuord <- zuord[-1]
    ##colnames(mm) <- zuord   ## takes too long
    nfac <- max(zuord)
    if (nfac < 3) return(0)

    ## 3fi columns
    ## store all in one matrix, faster but requires large matrix
    triples <- nchoosek(nfac, 3)
    anz <- 0
    for (i in 1:ncol(triples))
       anz <- anz+prod(nlevels[triples[,i,drop=FALSE]]-1)
    mm3 <- matrix(NA,nrow(design), anz)
    ## colnames(mm3) <- rep("sp",ncol(mm3))   ## takes too long

    zaehl <- 1   ## column accessor

    for (i in 1:(nfac - 2)){
     icols <- which(zuord==i)
      for (j in (i+1):(nfac - 1)){
        jcols <- which(zuord==j)
      for (k in (j+1):(nfac)){
        kcols <- which(zuord==k)
          for (a in icols){
           for (b in jcols){
             for (c in kcols){
              mm3[,zaehl] <- mm[,a]*mm[,b]*mm[,c]
              ##colnames(mm3)[zaehl] <- paste(i,j,k,sep=":")
              zaehl <- zaehl+1
           }}}
       }
    }}
    sum(colSums(mm3)^2)/(n^2)
}