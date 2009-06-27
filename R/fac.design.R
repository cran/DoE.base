## full factorials of all kinds

## eventually in the wrapper package

fac.design <- function(nlevels=NULL, nfactors=NULL, factor.names = NULL, 
        replications=1, repeat.only = FALSE, randomize=TRUE, seed=NULL){
        ## nlevels either length 1 (if all equal) or numeric vector of length nfactors, 
        ## factor.names analogous to FrF2 (character vector or named list of levels)

        ## vector version of nlevels is sufficient
        ## list version of factor.names is sufficient
        ## scalar nlevels together with nfactors is sufficient
        
        ## if more than one of the entries are given:
        ## compatibility checks necessary
        
        ## factor levels are 1:entry of nlevels, except for 2-level factors only, where they become -1 and 1

      ### check integer numbers
      creator <- sys.call()
      if (!is.null(nlevels)) if (!all(floor(nlevels)==nlevels)) 
           stop("nlevels must be an integer number or a vector of integer numbers.")
      if (!is.null(nfactors)) if (!floor(nfactors)==nfactors) 
           stop("nfactors must be an integer number.")
      if (!is.null(seed)) if (!floor(seed)==seed) 
           stop("seed must be an integer number.")
      if (!floor(replications)==replications) 
           stop("replications must be an integer number.")
      ### check compatibilities of level number and factor number specifications
      ### and specify unspecified ones of these
      if (is.null(nlevels) & !is.list(factor.names)) 
             stop("If factor.names does not specify the factor levels, nlevels must be given!")
      if (is.null(nlevels) & is.list(factor.names)) if (!min(hilf <- sapply(factor.names,length))>1) 
             stop("If factor.names does not specify the levels for all factors, nlevels must be given!")
      if (!(is.null(nlevels) | is.null(nfactors))) if (length(nlevels)>1 & !nfactors==length(nlevels))
                          stop("nfactors does not match the length of nlevels.")
      if (is.null(nlevels)) {nlevels <- hilf
                      if (!is.null(nfactors)) if (!length(nfactors)==length(nlevels))
                          stop("nfactors does not match the number of entries in factor.names.")}
      if (!(is.null(nlevels) | is.null(factor.names))) {
                      if (length(nlevels)>1 & !(length(factor.names)==length(nlevels)))
                          stop("length of factor.names and length of nlevels do not match.")
                      if (length(nlevels)==1) nlevels <- rep(nlevels,length(factor.names))
      if (is.list(factor.names)){ 
                             if (!(all(nlevels==sapply(factor.names,length) | sapply(factor.names,length)==1)))
                                 stop("Entries in nlevels do not match entries in factor.names.") 
                           }}
      if (is.null(nfactors)) nfactors <- length(nlevels)
      if (length(nlevels)==1) nlevels <- rep(nlevels, nfactors)
      if (is.null(factor.names) | !is.list(factor.names)) {
                 hilf <- NULL
                 if (!is.null(factor.names)) hilf <- factor.names
                 factor.names <-  rep(list(numeric(0)),nfactors)
                 if (!is.null(hilf)) names(factor.names) <- hilf 
                 else if (nfactors<=50) names(factor.names) <- Letters[1:nfactors] 
                       else names(factor.names) <- paste("F",1:nfactors,sep="")
                 for (i in 1:nfactors) factor.names[i] <- list(1:nlevels[i])
             }
      if (is.list(factor.names)) if (any(sapply(factor.names,length)==1)) 
                 for (i in 1:nfactors) if (length(factor.names[[i]])==1) factor.names[[i]] <- 1:nlevels[i]
      design <- expand.grid(factor.names)
      nruns <- nrow(design)
      row.names(design) <- 1:nruns 
      if (all(nlevels==2)) {factor.names[1:nfactors] <- rep(list(c(-1,1)),nfactors)
                 desnum <- as.matrix(expand.grid(factor.names))
                 row.names(desnum) <- 1:nrow(design) 
             } else desnum <- NULL

      rand.ord <- rep(1:nrow(design),replications)
      if (replications > 1 & repeat.only) rand.ord <- rep(1:nrow(design),each=replications)
      if (randomize & !is.null(seed)) set.seed(seed)
      if (randomize & !repeat.only) for (i in 1:replications) 
                  rand.ord[((i-1)*nrow(design)+1):(i*nrow(design))] <- sample(nrow(design))
      if (randomize & repeat.only) rand.ord <- rep(sample(1:nrow(design)), each=replications)
      aus <- design[rand.ord,]
      orig.no <- orig.no.rp <- rownames(aus)
      if (replications>1) {
           if (repeat.only) orig.no.rp <- paste(orig.no.rp,rep(1:replications,nruns),sep=".")
           else orig.no.rp <- paste(orig.no.rp,rep(1:replications,each=nruns),sep=".")
        }

      if (is.null(desnum)) desnum <- model.matrix(lm(1:nrow(aus)~.,data=aus)) else
           desnum <- desnum[rand.ord,]
      rownames(aus) <- rownames(desnum) <- 1:nrow(aus)

      attr(aus,"desnum") <- desnum
      attr(aus,"run.order") <- data.frame("run.no.in.std.order"=orig.no,"run.no"=1:nrow(aus),"run.no.std.rp"=orig.no.rp)
      attr(aus,"design.info") <- list(type="full factorial", 
          nruns=nruns, nfactors=nfactors, nlevels=nlevels, factor.names=factor.names,
          replications=replications, repeat.only=repeat.only, 
          randomized=randomize, seed=seed, creator=creator)
      class(aus) <- c("design","data.frame")
      aus
}
