oa.maxGR <- function (ID, nlevels)
{
    tab.needed <- table(nlevels)
    GR <- 3

    ## retrieve child array or array identified by character string
          ## gsub for case where ID is character string
    IDname <- gsub("\"","",deparse(substitute(ID)))
    if (all(IDname %in% oacat$name)){
    if (!exists(IDname))
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    else if (is.character(ID))
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    }

    ## identify match between available and requested levels
    nlevID <- apply(ID, 2, max)
    tab.available <- table(nlevID)[names(tab.needed)]
    if (any(is.na(names(tab.available)))) stop("not all levels can be accomodated")
    col.lists <- lapply(names(tab.needed), function(obj) which(nlevID ==
        as.numeric(obj)))
    spielraum <- tab.available - tab.needed
         if (any(spielraum < 0))
             stop("design does not have enough factors with ",
                  paste(names(spielraum)[which(spielraum<0)], collapse=" and "), " levels")

    ## provide candidate column list to be looped through
    cand.lists <- mapply(nchoosek, tab.available, tab.needed, SIMPLIFY=FALSE)
    cand.lists <- mapply(function(obj1, obj2) matrix(obj1[obj2],
        nrow = nrow(obj2), ncol = ncol(obj2)), col.lists, cand.lists,
        SIMPLIFY = FALSE)

    ## provide full factorial for all combinations of subsets,
    ## e.g. combining each variant of 3 2-level factors with each variant of 4 3-level factors
    hilf <- lapply(cand.lists, function(obj) 1:ncol(obj))
    hilf <- expand.grid(hilf)

    ## initialize curMax
    curMax <- -Inf
    MaxVariants <- numeric(0)
    MaxProj <- vector("list",0)
    for (i in 1:nrow(hilf)) {
        spalten <- c(unlist(mapply(function(obj1, obj2) obj1[,
            obj2], cand.lists, hilf[i, ])))
        cur3 <- GR(ID[, spalten], digits=4)
        if (cur3$GR == curMax){
            MaxVariants <- rbind(MaxVariants, spalten)
            MaxProj <- c(MaxProj, list(cur3$RPFT))
            }
        else if (cur3$GR > curMax) {
            curMax <- cur3$GR
            MaxVariants <- matrix(spalten, nrow = 1)
            MaxProj <- list(cur3$RPFT)
        }
    }
    rownames(MaxVariants) <- 1:nrow(MaxVariants)
    list(GR = curMax, column.variants = MaxVariants, RPFTs = MaxProj)
}

oa.maxGR.min34 <- function(ID, nlevels, maxGR=NULL){
    ## retrieve child array or array identified by character string
          ## gsub for case where ID is character string
    IDname <- gsub("\"","",deparse(substitute(ID)))
    if (all(IDname %in% oacat$name)){
    if (!exists(IDname))
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    else if (is.character(ID))
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    }
    ## determine oa.maxGR, if not handed to the function from previous call
     if (is.null(maxGR)) maxGR <- oa.maxGR(ID, nlevels)

     if (!is.list(maxGR)) stop("maxGR must be a list")
     if (!all(c("GR","column.variants","RPFTs") %in% names(maxGR)))
         stop("maxGR is not of the appropriate form")
     reso <- floor(maxGR$GR)
     variants <- maxGR$column.variants
     RPFTs <- maxGR$RPFTs
     if (maxGR$GR==5) {
         hilf <- c("3"=0,"4"=0)
         return(list(GWP=hilf, column.variants=variants, complete=TRUE))
     }
     else{
     ## rARs for the optimized GR designs (with numerical inaccuracies from rounding in RPFTs)
     
     approx.lengthRs <- sapply(RPFTs, function(obj) round(sum(apply(obj,1,"prod")),1))
     cands <- which(approx.lengthRs==min(approx.lengthRs))

     ## more exact rARs for the crudely optimized GR designs
    lengthRs <- rep(NA, length(cands))
    variants <- variants[cands,]
    if (reso == 3)
    for (i in 1:length(cands)) lengthRs[i] <- round(length3(ID[,variants[i,],drop=FALSE], rela=TRUE),4)
    else
    for (i in 1:length(cands)) lengthRs[i] <- round(length4(ID[,variants[i,],drop=FALSE], rela=TRUE),4)
    variants <- variants[lengthRs==min(lengthRs),,drop=FALSE]
    maxGR$column.variants <- variants
    maxGR$RPFTs <- TRUE
    maxGR$GR <- min(lengthRs)
    names(maxGR$GR) <- paste(reso, "relative", sep=".")
    names(maxGR) <- c(paste("GWP",reso,sep=""), "column.variants", "complete")
     if (reso==3)
     return(oa.min34(ID, nlevels=nlevels, min3=maxGR, rela=TRUE))
     else return(maxGR)
     }
     }
