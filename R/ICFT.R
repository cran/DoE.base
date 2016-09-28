ICFT <- function (design, digits = 3, with.blocks = FALSE, 
     conc = TRUE, recode=TRUE) 
{
    ### function for detailed inspection for 
    ### a single combination of factors in terms of their 
    ### ICs
    
    ### for overall inspection of entire designs, consider function ICFTs
    
    if ("design" %in% class(design)) {
        fn <- names(factor.names(design))
        if (with.blocks) 
            fn <- c(fn, design.info(design)$block.name)
        design <- design[, fn]
        nfac <- length(fn)
    }
    else {
        nfac <- ncol(design)
        fn <- 1:nfac
    }

    nlev <- levels.no(design)
    dfs <- nlev - 1

    if (!is.data.frame(design)) design <- as.data.frame(design)
    if (recode)
    for (i in 1:nfac){
       design[[i]] <- factor(design[[i]])
       contrasts(design[[i]]) <- contr.XuWu(nlev[i])
    }

    k <- nfac
    N <- nrow(design)
 
        ns <- 1
        auswahl <- 1
        selproj <- sel <- matrix(1:k, ncol=1)
        selproj <- paste(selproj, collapse = ":")
        names(auswahl) <- selproj
        
        hilf2 <- design
        mm <- model.matrix(formula(substitute(~.^km1, 
                    list(km1 = k))), data = hilf2)
        mm <- mm[,-(1:(ncol(mm) - prod(dfs)))]
        hilfc <- svd(mm)

      ## spans of constant sv2s: from to cumcount
        sv2s <- hilfc$d^2
        mean.u2s <- colMeans(hilfc$u)^2
        ICs <- sv2s*mean.u2s

      ## check for and cure ambiguities        
        hilf2 <- table(round(sv2s,6))
        cumcounts <- cumsum(rev(hilf2))
        from <- c(1, cumcounts[-length(hilf2)]+1) 
        multi <- which(cumcounts-from > 0)

      if (length(multi)>0){ 
        for (i in multi){
        ## rectify ambiguities
        bereich <- from[i]:cumcounts[i]       
        li <- length(bereich)
        rot <- HouseholderRotToOne(colMeans(hilfc$u)[bereich])$rot
        #hilf2 <- rep(0, li) ## initialize output vector
        if (conc){ 
            #hilf2[1] <- sum((sv2s*mean.u2s)[bereich])
            Q <- t(rot)  ## matrix concentrating on first component
          }
          else {
            ## even case 
            #hilf2 <- rep(mean((sv2s*colMeans(hilfc$u)^2)[bereich]),li)
            Q <- rect_simplex(li)
            Q <- t(rot)%*%Q
            }
         hilfc$u[,bereich] <- hilfc$u[,bereich]%*%Q
         hilfc$v[,bereich] <- hilfc$v[,bereich]%*%Q
         }

      ## redo after rotations
      mean.u2s <- colMeans(hilfc$u)^2
      ICs <- sv2s*mean.u2s
      }
        
        rund <- round(ICs, digits)
 
        ICFT <- table(unlist(rund))
        ICFT <- cbind(IC = as.numeric(names(ICFT)), frequency = ICFT)
        rownames(ICFT) <- rep("", nrow(ICFT))
        aus <- list(ICFT = ICFT,
                    ICs = ICs, 
                    sv2s = sv2s, 
                    mean.u2s = mean.u2s, 
                    mm = mm,
                    u = hilfc$u,
                    v = hilfc$v)
    aus
}
