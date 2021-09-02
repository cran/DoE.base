oa_feasible <- function(nruns, nlevels, strength=2, verbose=TRUE, returnbound=FALSE){
    aus <- TRUE
    nlev <- nlevels
    if (!all(is.numeric(c(nruns,nlev, strength)))) stop("nruns, nlev and strength must be numeric")
    if (!all(c(nruns,nlev)>0)) stop("nruns, nlev and strength must be positive")
    if (!all(c(nruns,nlev, strength)%%1==0)) stop("nruns, nlev and strength must be integer")
    if (!(length(nruns)==1 && length(strength==1))) stop("nruns and strength must be scalar")
    nfac <- length(nlev)
    if (!nfac >=2) stop("too few factors")
    if (nfac < strength) stop("strength larger than number of factors")

    ## tabulation of nlevels would be useful for large nfac
    ## since this is not the situation for this package, not pursued
    ## because it sincerely complicates things
    tablev <- table(nlev)
    s <- as.numeric(names(tablev))  ## the different levels
    fs <- tablev                    ## their frequencies
    nlevelmix <- length(s)          ## their number
    u <- strength%/%2               ## for calculation of Rao bound

    retbound <- sum(nlevels) - length(nlevels) + 1   ## df bound
    ## handle simpler pure level situation first
    if (nlevelmix == 1){
      ## pure level designs
      if (!nruns%% s^strength==0) {
        if (verbose) cat("nruns is not divisible by", s^strength, fill=TRUE)
        if (!returnbound) return(FALSE) else retbound <- max(retbound, s^strength)
      }
      ## Bierbrauer et al.s bound for 2-level (Thms 7.1 and 7.3 with Cor 7.4 (and abstract))
      if (s==2){
        bound <- round(s^nfac - min(nfac*2^(nfac-1)/(strength+1), (nfac+1)*2^(nfac-2)/(u+1)), 2)
        if (nruns < bound){
          if (verbose) cat("nruns is smaller than Bierbrauer et al.s bound ", bound, " for 2-level designs", fill=TRUE)
          if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
          }
      }
      ## Bierbrauer's bound
      bound <- round(s^nfac*(1-(s-1)*nfac/(s*(strength+1))), 2)
      if (nruns < bound) {
        if (verbose) cat("nruns is smaller than Bierbrauer's bound", bound, " for pure level designs", fill=TRUE)
        if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
      }
      ## Rao's bound
      ii <- 0:(strength%/%2)
      bound <- sum(choose(nfac, ii)*(s-1)^ii)
      ## odd strengths
      if (strength %% 2 == 1) bound <- bound + choose(nfac-1,u)*(s-1)^(u + 1)
      if (nruns < bound){
        if (verbose) cat("nruns is smaller than Rao's bound", bound, "for pure level designs", fill=TRUE)
        if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
      }
      ## Bush bound
      if (nruns==s^strength){
        ## HSS Theorem 2.19
        if (s <= strength) bound <- strength + 1
        else{
          ## s > strength
            bound <-  s + strength - 1
            if (3 <= strength && s%%2 == 1) bound <- s + strength - 2
        }
        if (nfac > bound){
          if (verbose) cat("more than ", bound, " factors,", " Bush bound violated", fill=TRUE)
          return(FALSE)
        }
      }
      ## Bose Bush bounds for strengths 2 and 3, not for 2-level
      if (nruns > s^strength && !s==2){
        ## HSS theorems 2.8 and 2.11
        ## for s==2, lambda-1 is always a multiple of s-1
        lambda <- nruns%/%(s^strength)
        b <- (lambda - 1)%%(s-1)
        if (b > 0 && strength %in% c(2,3)){
          a <- (lambda-1)%/%(s-1)
          theta <- (sqrt(1+4*s*(s-1-b)) - (2*s-2*b-1))/2
          if (strength==2) bound <- lambda*(s + 1) + a - floor(theta) - 1 
          if (strength==3) bound <- lambda*(s + 1) + a - floor(theta) 
          if (nfac > bound){
            if (verbose) cat("more than ", bound, " factors,", " Bose/Bush bound violated", fill=TRUE)
            return(FALSE)
          }
        }
      }
    if (returnbound) return(retbound)
    }
    else{
      ## mixed level designs
      ## Bierbrauer's mixed level bound, Theorem 2.2 of Diestelkamp 2004
      ## this bound is stronger than Raos for very large values of strength
      ##    (for which computations of Rao's bound in the current implementation
      ##    will be prohibitive)
      ## and it is proven only for strength > (SM-1)*nfac/SM - 1
      sT <- sum(nlev)
      sM <- max(nlev)
      sm <- min(nlev)
      if(strength > (sM-1)*nfac/sM - 1){
      bound <- floor(sm^nfac*(1 - max(0, (sT - nfac)/(sT + (strength - nfac + 1)*sM))))
      if (nruns < bound){
        if (verbose) cat("nruns is smaller than Diestelkamp's mixed level version", 
                         " of Bierbrauer's bound", bound, fill=TRUE)
        if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
      }
      }

      ## Rao's mixed level bound, Theorem 3.2 of Diestelkamp 2004
    bound <- sum(nlev - 1) + 1   ## df for up to main effects
    if (strength >= 2 && nruns < bound){
      if (verbose) cat("nruns is smaller than the ", bound, " df needed for main effects", fill=TRUE)
        if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
      }
    if (strength == 3){
      ## add worst-case df for 2fi
      bound <- bound + max(sapply(1:nfac, function(obj) (nlev[obj] - 1)*sum(nlev[-obj] - 1)))
      if (nruns < bound){
        if (verbose) cat("nruns is smaller than Rao's bound", bound, "for strength 3", fill=TRUE)
        if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
      }
    }
    if (strength>=4){
      for (ii in 2:u){
        ## df of strength ii effects
        sets <- nchoosek(nfac, ii)
        prods <- sapply(1:ncol(sets), function(obj){
          ## obj is a column number of sets
          ## product of dfs in set
          prod(nlev[sets[,obj]] - 1)
        })
        dazu <- sum(prods)

        ## increase dazu for the last contribution
        ## in case of odd strength
        ## by different summand
        if (strength == 2 * ii + 1){
          hilf <- sapply(1:ncol(sets), function(obj){
            (max(nlev[setdiff(1:nfac, sets[,obj])]) - 1)*prods[obj]
          })
          dazu <- dazu + max(hilf)
          }
        bound <- bound + dazu
      }
      if (nruns < bound){
        if (verbose) cat("nruns is smaller than Rao's bound ", bound, " for strength ", strength, fill=TRUE)
          if (!returnbound) return(FALSE) else retbound <- max(retbound, bound)
      }
    }
    ## end of Rao's mixed level bound

      ## condition on LCM
      clcm <- numbers::mLCM(apply(matrix(nlev[nchoosek(nfac,strength)], nrow=strength),
                       2, prod))
        if (!nruns %% clcm == 0){
          if (verbose) cat("nruns is not divisible by ", clcm, fill=TRUE)
          if (!returnbound) return(FALSE) else retbound <- max(retbound, (retbound%/%clcm + 1)*clcm)
        }
    if (returnbound) {
    if (verbose) cat("need at least ", retbound, " runs for strength ", strength, ", ", 
                     "full factorial would need ", prod(nlev), " runs", fill=TRUE)
    return(retbound)
    }
 }
 ## no violation of criteria for oa with required strength was found
 if (verbose) cat("no violation of necessary criteria", " for strength ", strength, " was found", fill=TRUE)
 TRUE
}
