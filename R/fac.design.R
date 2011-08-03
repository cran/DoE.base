## full factorials of all kinds

## eventually in the wrapper package

fac.design <- function(nlevels=NULL, nfactors=NULL, factor.names = NULL, 
        replications=1, repeat.only = FALSE, randomize=TRUE, seed=NULL, 
        blocks=1, block.gen=NULL, block.name="Blocks", bbreps=replications, 
        wbreps=1){
        ## nlevels either length 1 (if all equal) or numeric vector of length nfactors, 
        ## factor.names analogous to FrF2 (character vector or named list of levels)

        ## vector version of nlevels is sufficient
        ## list version of factor.names is sufficient
        ## scalar nlevels together with nfactors is sufficient
        
        ## if more than one of the entries are given:
        ## compatibility checks necessary
        
        ## factor levels are 1:entry of nlevels, except for 2-level factors only, where they become -1 and 1
        
        ## block generation:
        ## if only blocks is given and is a prime or a product of distinct primes, 
        ##      highest possible confounding is used, if block.gen=NULL
        ## if more than one combination of the same prime is needed, 
        ##      block.gen is required 
        ## block.gen must be a list or a matrix of numbers from 0 to p-1
        ##      or a vector (treated as one-row matrix)
        ##      appropriately matched to the columns of factorize.design

      ### check integer numbers
      creator <- sys.call()
      if (!is.null(nlevels)){ 
           if (!is.numeric(nlevels)) stop("nlevels must be numeric")
           if (!all(floor(nlevels)==nlevels)) 
              stop("nlevels must be an integer number or a vector of integer numbers.")
           if (any(nlevels < 2)) 
              stop("nlevels must not contain entries smaller than 2")
           }
      if (!is.null(nfactors)) if (!floor(nfactors)==nfactors) 
           stop("nfactors must be an integer number.")
      if (!is.null(seed)) if (!floor(seed)==seed) 
           stop("seed must be an integer number.")
      if (!floor(replications)==replications) 
           stop("replications must be an integer number.")
      if (identical(blocks,1) & !identical(wbreps,1)) 
           stop("wbreps must not differ from 1, if blocks = 1.")
      if (bbreps > 1  & identical(blocks,1) & !replications > 1) 
        stop("Use replications, not bbreps, for specifying replications for unblocked designs.")
      ### check compatibilities of level number and factor number specifications
      ### and specify unspecified ones of these
      if (is.null(nlevels) & !is.list(factor.names)) 
             stop("If factor.names does not specify the factor levels, nlevels must be given!")
      if (is.null(nlevels) & is.list(factor.names)) if (!min(hilf <- sapply(factor.names,length))>1) 
             stop("If factor.names does not specify at least two levels for each factor, nlevels must be given!")
      if (!(is.null(nlevels) | is.null(nfactors))) if (length(nlevels)>1 & !nfactors==length(nlevels))
                          stop("nfactors does not match the length of nlevels.")
      if (is.null(nlevels)) {nlevels <- hilf
                      if (!is.null(nfactors)) if (!nfactors==length(nlevels))
                          stop("nfactors does not match the number of entries in factor.names.")}
      if (!(is.null(nlevels) | is.null(factor.names))) {
                      if (length(nlevels)>1 & !(length(factor.names)==length(nlevels)))
                          stop("length of factor.names and length of nlevels do not match.")
                      if (length(nlevels)==1) nlevels <- rep(nlevels,length(factor.names))
                      }
      if (is.null(nfactors)) nfactors <- length(nlevels)
      if (nfactors==1) stop("one factor only is not covered by fac.design")
      if (length(nlevels)==1) nlevels <- rep(nlevels, nfactors)
      if (is.list(factor.names)){ 
                             if (!(all(nlevels==sapply(factor.names,length) | sapply(factor.names,length)==1)))
                                 stop("Entries in nlevels do not match entries in factor.names.") 
            if (is.null(names(factor.names))){ if (nfactors<=50) names(factor.names) <- Letters[1:nfactors] 
                       else names(factor.names) <- paste("F",1:nfactors,sep="")
                           }}
      if (is.null(factor.names) | !is.list(factor.names)) {
                 ## null or character vector
                 hilf <- NULL
                 if (!is.null(factor.names)) hilf <- factor.names
                 factor.names <-  rep(list(numeric(0)),nfactors)
                 if (!is.null(hilf)) names(factor.names) <- hilf 
                 else if (nfactors<=50) names(factor.names) <- Letters[1:nfactors] 
                       else names(factor.names) <- paste("F",1:nfactors,sep="")
                 for (i in 1:nfactors) factor.names[i] <- list(1:nlevels[i])
             }
      if (is.list(factor.names)){ 
            if (is.null(names(factor.names))){ if (nfactors<=50) names(factor.names) <- Letters[1:nfactors] 
                       else names(factor.names) <- paste("F",1:nfactors,sep="")}
            if (any(sapply(factor.names,length)==1)) 
                 for (i in 1:nfactors) if (length(factor.names[[i]])==1) factor.names[[i]] <- 1:nlevels[i]
                 }
      ## make names valid under all circumstances
      names(factor.names) <- make.names(names(factor.names), unique=TRUE)
      
      ## check validity of blocking request
      if (!identical(blocks,1)){
        if (!is.numeric(blocks)) stop("blocks must be numeric")
        if (!round(blocks)==blocks) stop("blocks must be integer")
        if (!is.numeric(bbreps)) stop("bbreps must be an integer number.")
        if (!is.numeric(wbreps)) stop("wbreps must be an integer number.")
        ## pre-process needs for numbers of blocks and numbers of levels
        need.gen <- factorize.default(blocks)
        hilfl <- factorize.default(nlevels)
        names(hilfl) <- names(factor.names)
        lengths <- sapply(hilfl, length)
        collevs <- unlist(hilfl)
        FUNC <- function(X, Y) rep(Y, X)
        pseudo.belongs <- unlist(mapply(FUNC, lengths, names(hilfl)))
        if (is.null(block.gen)){
            ## unspecified block generators
            if (any(table(need.gen)>1))
              stop("For this number of blocks, block.gen must be specified (see documentation)")
            ## now only one instance of each prime needed
            ## block.gen can only be specified after creating the design
            ## definite inacceptability can be checked now
            for (i in unique(need.gen)) 
                if (sum(sapply(hilfl, function(obj) i %in% obj)) == 0)
                   stop("This number of blocks cannot be accomodated orthogonally to main effects.")
                else if (sum(sapply(hilfl, function(obj) i %in% obj)) == 1)
                   stop("Blocks would be confounded with main effects!") 
            block.gen <- t(sapply(need.gen, function(obj) as.numeric(collevs==obj)))
            for (i in 1:length(need.gen))
                if (length(unique(pseudo.belongs[which(block.gen[i,]>0)]))==1)
                   stop("Blocks would be confounded with main effects")
            ## there are now block generators that have been checked out 
            ## to come from separate factors within each prime group
        }
        else{
            ## specified block generators
            if (! is.numeric(block.gen)) 
                stop("If given, block.gen must be a numeric matrix, or a numeric vector.")
            if (!is.matrix(block.gen)) 
                block.gen <- matrix(block.gen, nrow=1)
            if (!nrow(block.gen)==length(need.gen))
               stop(nrow(block.gen)," block generators specified, ", length(need.gen), " would be needed")
            if (!ncol(block.gen)==length(collevs))
               stop("coefficients for ", ncol(block.gen), " pseudo-factors were specified, ",
                  length(collevs), " would be needed")
               }
            ## continue of checking
            if (!nrow(block.gen)==length(need.gen)) 
                 stop(nrow(block.gen)," block generators specified, ", length(need.gen), " would be needed")
            ## identify relevant prime groups
            ung <- unique(need.gen)
            pg <- vector(mode="list", length=length(ung))
            
            for (i in 1:nrow(block.gen)){
                hilf <- block.gen[i,,drop=TRUE]
                chilf <- which(hilf>0)
                if (length(lev <- unique(collevs[chilf]))>1) 
                    stop("each block generator must address pseudo factors with the same number of levels only")
                if (!lev %in% ung)
                    stop("the ", i, "th generator is not compatible with the requested number of blocks")
                pg[[which(ung==lev)]] <- rbind(pg[[which(ung==lev)]],hilf)
            }
            ## pg should now be list of separated generator matrices per prime
            ## check for correct number of elements 
            ## for correct element types
            ## and for too severe confounding
            for (i in 1:length(pg)){
                 if (!table(need.gen)[i] == nrow(pg[[i]]))
                     stop("something went wrong, wrong number of generators for prime ", ung[i])
                 if (any(!pg[[i]] %in% 0:(ung[i]-1)))
                     stop("wrong entries in block.gen for prime ", ung[i])
                 ### check confounding using conf.set function from conf.design
                 hilf <- conf.set(pg[[i]], ung[i])
                 if (!is.matrix(hilf)) hilf <- matrix(hilf, nrow=1)
                 for (j in 1:nrow(hilf)) {
                      hilf2 <- length(unique(pseudo.belongs[which(hilf[j,]>0)]))
                      if (hilf2==1) stop("confounding of blocks with main effects")
                      if (hilf2==2) warning("confounding of blocks with 2-factor interactions")
                 }
            }
        }
      
      nruns <- prod(sapply(factor.names,"length"))
      cat("creating full factorial with ", nruns, " runs ...\n")

      design <- try(expand.grid(factor.names))
      if ("try-error" %in% class(design)) 
          stop("design with ", nruns, " runs could not be generated")
      row.names(design) <- 1:nruns 
      ## process blocking request (check feasibility for block.gen)
        if (!is.null(block.gen)){ 
              pseudo <- factorize.data.frame(design, long=TRUE)
              for (i in 1:length(pg)){
                  ## calculated block contributor for each relevant prime
                  pg[[i]] <- (pseudo%*%t(pg[[i]]))%%ung[i]
              }
      ## not elegant but works
      ## join does not necessarily work because pg elements can be matrices
       blockcol <- rep("",nruns)
               for (i in 1:length(pg))
                  for (j in 1:ncol(pg[[i]]))
                      blockcol <- paste(blockcol, pg[[i]][,j],sep="")
               blockcol <- as.factor(as.numeric(as.factor(blockcol)))
              ## attach this column to the design only later,
              ## because this prevents case distinctions
          }
      ## end of blocking
                desnum <- NULL
                quant <- sapply(factor.names, "is.numeric")
                for (i in 1:nfactors){
                    if (!is.factor(design[,i]))
                       design[,i] <- factor(design[,i],levels=factor.names[[i]]) 
                    if (nlevels[i]==2) contrasts(design[,i]) <- contr.FrF2(2)
                    else if (quant[i]) contrasts(design[,i]) <- contr.poly(nlevels[i],scores=factor.names[[i]])
                }
      
      ## prepend block column, if needed
      if (!identical(blocks, 1)){ 
         design <- cbind(blockcol, design)
         colnames(design) <- c(block.name, names(factor.names))
         design <- design[ord(data.frame(blockcol)),]
         }

      ## simple randomization situations
      if (identical(blocks, 1)){ 
          rand.ord <- rep(1:nrow(design),replications)
          if (replications > 1 & repeat.only) rand.ord <- rep(1:nrow(design),each=replications)
          if (randomize & !is.null(seed)) set.seed(seed)
          if (randomize & !repeat.only) for (i in 1:replications) 
                      rand.ord[((i-1)*nrow(design)+1):(i*nrow(design))] <- sample(nrow(design))
          if (randomize & repeat.only) rand.ord <- rep(sample(1:nrow(design)), each=replications)
          aus <- design[rand.ord,]
      }
      else{
         ## blocked randomization
          #### implement randomization and replication for blocks
          ## bbreps=replications, wbreps=1
          ## if ((!repeat.only) & !randomize)
              nblocks <- blocks
              blocksize <- nrow(design)%/%nblocks
              
              rand.ord <- rep(1:nruns, bbreps * wbreps)
              if ((!repeat.only) & !randomize)
                 for (i in 0:(nblocks-1))
                    for (j in 1:wbreps)
                    rand.ord[(i*blocksize*wbreps+(j-1)*blocksize+1):((i+1)*blocksize*wbreps+j*blocksize)] <- 
                         (i*blocksize+1):((i+1)*blocksize)
                    rand.ord <- rep(rand.ord[1:(nruns*wbreps)],bbreps)
              if (repeat.only & !randomize)
                    for (j in 1:wbreps)
                    rand.ord[(i*blocksize*wbreps + (j-1)*blocksize + 1) : 
                          (i*blocksize*wbreps + j*blocksize)] <- 
                               sample((i%%nblocks*blocksize+1):(i%%nblocks*blocksize+blocksize))
              if (wbreps > 1 & repeat.only) rand.ord <- rep(1:nruns,bbreps, each=wbreps)
              if ((!repeat.only) & randomize)
                 for (i in 0:(nblocks*bbreps-1))
                    for (j in 1:wbreps)
                    rand.ord[(i*blocksize*wbreps + (j-1)*blocksize + 1) : 
                          (i*blocksize*wbreps + j*blocksize)] <- 
                               sample((i%%nblocks*blocksize+1):(i%%nblocks*blocksize+blocksize))
                               
              if (repeat.only & randomize)
                 for (i in 0:(nblocks*bbreps-1))
                    rand.ord[(i*blocksize*wbreps + 1) : 
                          ((i+1)*blocksize*wbreps)] <- rep(sample((((i%%nblocks)*blocksize)+1):
                                        ((i%%nblocks+1)*blocksize)),each=wbreps)
              aus <- design[rand.ord,]
      }
      ## extract run number in standard order
      ## remove uniqueness appendix
      orig.no <- orig.no.rp <- sapply(strsplit(rownames(aus),".",fixed=TRUE),function(obj) obj[1])
      ## row added 27 01 2011 (for proper ordering of design)
      orig.no.levord <- sort(as.numeric(orig.no),index=TRUE)$ix
      if (blocks > 1) {
            orig.no.levord <- sort(100000*as.numeric(aus[[block.name]])+as.numeric(orig.no),index=TRUE)$ix
            orig.no <- paste(orig.no,aus[[block.name]], rep(1:blocksize,nblocks)[rand.ord], sep=".")
            }
      orig.no.rp <- orig.no
      if (bbreps * wbreps > 1){
           if (bbreps > 1) {
                ## !repeat.only covers all blocked cases and the repeat.only standard cases
                ## since bbreps stands in for replications
                if (repeat.only & identical(blocks,1))
                orig.no.rp <- paste(orig.no.rp, rep(1:bbreps,nruns),sep=".")
                else
                orig.no.rp <- paste(orig.no.rp, rep(1:bbreps,each=nruns*wbreps),sep=".")
           }
           if (wbreps > 1){
                ## blocked with within block replications
                if (repeat.only) 
                     orig.no.rp <- paste(orig.no.rp, rep(1:wbreps,nruns*bbreps),sep=".")
                else orig.no.rp <- paste(orig.no.rp, rep(1:wbreps,each=blocksize,nblocks*bbreps),sep=".")
           }
    }

      if (is.null(desnum)) desnum <- model.matrix(1:nrow(aus)~.,data=aus)[,-1,drop=FALSE] else
           desnum <- desnum[rand.ord,]
      rownames(aus) <- rownames(desnum) <- 1:nrow(aus)

      di <- list(type="full factorial", 
          nruns=nruns, nfactors=nfactors, nlevels=nlevels, factor.names=factor.names,
          replications=replications, repeat.only=repeat.only, 
          randomize=randomize, seed=seed, creator=creator)
      if (blocks>1){ 
            di$type <- "full factorial.blocked"
            di$block.name=block.name
            di$bbreps <- bbreps
            di$wbreps <- wbreps
            if (di$wbreps==1) di$repeat.only <- FALSE
            di$nblocks <- nblocks
            di$blocksize <- blocksize
            di$block.gen=block.gen
      }

      attr(aus,"desnum") <- desnum
      ## change 27 Jan 2011: leave orig.no as a factor, but with better-ordered levels
      orig.no <- factor(orig.no, levels=unique(orig.no[orig.no.levord]))
      attr(aus,"run.order") <- data.frame("run.no.in.std.order"=orig.no,"run.no"=1:nrow(aus),"run.no.std.rp"=orig.no.rp)
      attr(aus,"design.info") <- di
      class(aus) <- c("design", "data.frame")
      aus
}
