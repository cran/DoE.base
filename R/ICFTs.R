ICFTs <- function (design, digits = 3, resk.only = TRUE, 
    kmin = NULL, kmax = ncol(design), detail = FALSE, with.blocks = FALSE, 
    conc = TRUE) 
{
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
    for (i in 1:nfac){
       design[[i]] <- factor(design[[i]])
       contrasts(design[[i]]) <- contr.XuWuPoly(nlev[i])
    }

    ks <- which(round(GWLP(design, kmax = kmax)[-1], 8) > 0)
    N <- nrow(design)
    if (length(ks) == 0) {
        hilf <- list(list(ICFT = cbind(IC = 0, frequency = sum(nlev) - 
            kmax), IC1 = 0))
        names(hilf) <- kmax
        return(hilf)
    }
    k <- min(ks)
    if (k < 2) 
        stop("resolution of design must be at least 2")
    kminset <- FALSE
    if (is.null(kmin)) 
        kmin <- k
    else {
        if (!kmin == min(ks)) kminset <- TRUE
        redu <- ks[ks >= kmin]
        message(paste("Check sets of sizes ", paste(redu, collapse = ",")))
        if (length(redu) == 0) 
            return()
        kmin <- min(redu)
        k <- kmin
        ks <- redu
    }
    if (k >= kmin) {
#        if (!"design" %in% class(design)) {
#            if (!is.data.frame(design)) 
#                design <- as.data.frame(design)
#            faktoren <- sapply(design, is.factor)
#            keinfaktor <- which(!faktoren)
#            if (length(keinfaktor) > 0) 
#                for (i in keinfaktor) design[[i]] <- as.factor(design[[i]])
#        }
        k <- kmin
        ns <- choose(nfac, k)
        auswahl <- 1:ns
        selproj <- sel <- nchoosek(nfac, k)
        GWLPs <- round(apply(selproj, 2, function(obj) GWLP(design[, 
            obj])[-1]), 4)
        selproj <- apply(selproj, 2, function(obj) paste(obj, 
            collapse = ":"))
        names(auswahl) <- selproj
 #       ergproj <- rep(NA, length(selproj))
 #       dimnames(erg3) <- list(factor = fn, others = apply(sel, 
 #           2, function(obj) paste(obj, collapse = ":")), 1:(max(nlev) - 
 #           1))
        if (resk.only) {
                reskproj <- apply(GWLPs, 2, function(obj) all(obj[-k] == 
                  0))
                if (all(!reskproj)) {
                  message("no projections with resolution ", 
                    k, " or higher")
                  return()
          }}
        
            berechn <- lapply(auswahl, function(obj) {
                hilf2 <- design[, sel[, obj]]
                mmX <- model.matrix(formula(substitute(~.^km1, 
                    list(km1 = k))), data = hilf2)
                mmX <- mmX[,-(1:(ncol(mmX) - prod(dfs[sel[, obj]])))]
                hilfc <- svd(mmX)
                hilf2 <- table(round(hilfc$d^2,6))
                cumcounts <- cumsum(rev(hilf2))
                from <- c(1, cumcounts[-length(hilf2)]+1) 
                hilf2 <- rep(0, prod(dfs[sel[, obj]])) ## initialize output vector
                if (conc) 
                hilf2[from] <- sapply(1:length(from), 
                   function(obj) sum((hilfc$d^2*colMeans(hilfc$u)^2)[from[obj]:cumcounts[obj]])
                   )
                else {
                   for (i in 1:length(from)){
                         bereich <- from[i]:cumcounts[i]       
                         hilf2[bereich] <- mean((hilfc$d^2*colMeans(hilfc$u)^2)[bereich])
                }}
                list(hilf2, hilfc$d^2, colMeans(hilfc$u)^2)
            })
        
        ICs <- lapply(berechn, function(obj) obj[[1]])
        sv2s <- lapply(berechn, function(obj) obj[[2]]) 
        mean.u2s <- lapply(berechn, function(obj) obj[[3]]) 
        rund <- lapply(ICs, function(obj) round(obj,digits))
 
        ICFT <- table(unlist(rund))
        ICFT <- cbind(IC = as.numeric(names(ICFT)), frequency = ICFT)
        rownames(ICFT) <- rep("", nrow(ICFT))
        aus <- list(ICFT = ICFT)

        if (detail) 
            aus <- c(aus, list(ICs = rund, sv2s = sv2s, mean.u2s = mean.u2s ))

        if (!resk.only || kminset){
            aus <- list(aus); names(aus) <- k
            if (!resk.only){
            ks <- kmin:kmax
            if (length(ks)>1){
                 for (k in (kmin+1):kmax){
                    ns <- choose(nfac, k)
                    auswahl <- 1:ns
                    selproj <- sel <- nchoosek(nfac, k)
                    GWLPs <- round(apply(selproj, 2, function(obj) GWLP(design[, 
                       obj])[-1]), 4)
                    selproj <- apply(selproj, 2, function(obj) paste(obj, 
                       collapse = ":"))
                    names(auswahl) <- selproj
        
            berechn <- lapply(auswahl, function(obj) {
                hilf2 <- design[, sel[, obj]]
                mmX <- model.matrix(formula(substitute(~.^km1, 
                    list(km1 = k))), data = hilf2)
                mmX <- mmX[,-(1:(ncol(mmX) - prod(dfs[sel[, obj]])))]
                hilfc <- svd(mmX)
                hilf2 <- table(round(hilfc$d^2,6))
                cumcounts <- cumsum(rev(hilf2))
                from <- c(1, cumcounts[-length(hilf2)]+1) 
                hilf2 <- rep(0, prod(dfs[sel[, obj]])) ## initialize output vector
                if (conc) 
                hilf2[from] <- sapply(1:length(from), 
                   function(obj) sum((hilfc$d^2*colMeans(hilfc$u)^2)[from[obj]:cumcounts[obj]])
                   )
                else {
                   for (i in 1:length(from)){
                         bereich <- from[i]:cumcounts[i]       
                         hilf2[bereich] <- mean((hilfc$d^2*colMeans(hilfc$u)^2)[bereich])
                }}
                list(hilf2, hilfc$d^2, colMeans(hilfc$u)^2)
            })
                     
           ICs <- lapply(berechn, function(obj) obj[[1]])
           sv2s <- lapply(berechn, function(obj) obj[[2]]) 
           mean.u2s <- lapply(berechn, function(obj) obj[[3]]) 
           rund <- lapply(ICs, function(obj) round(obj,digits))
 
           ICFT <- table(unlist(rund))
           ICFT <- cbind(IC = as.numeric(names(ICFT)), frequency = ICFT)
           rownames(ICFT) <- rep("", nrow(ICFT))
           ausn <- list(ICFT = ICFT)
           if (detail) 
              ausn <- c(ausn, list(ICs = rund, sv2s = sv2s, mean.u2s = mean.u2s ))
           ausn <- list(ausn)
           names(ausn) <- k
           aus <- c(aus, ausn)
                 }
             }
           }}
    }
    else aus <- list(ICFT = NULL)
    aus
}
