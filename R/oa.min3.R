oa.max3 <- function (ID, nlevels) 
{
    tab.needed <- table(nlevels)

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
    for (i in 1:nrow(hilf)) {
        spalten <- c(unlist(mapply(function(obj1, obj2) obj1[, 
            obj2], cand.lists, hilf[i, ])))
        cur3 <- round(length3(ID[, spalten]), 4)
        if (cur3 == curMax) 
            MaxVariants <- rbind(MaxVariants, spalten)
        else if (cur3 > curMax) {
            curMax <- cur3
            MaxVariants <- matrix(spalten, nrow = 1)
        }
    }
    rownames(MaxVariants) <- 1:nrow(MaxVariants)
    list(GWP3 = curMax, column.variants = MaxVariants, complete = TRUE)
}

oa.min3 <- function (ID, nlevels, all = FALSE) 
{
    tab.needed <- table(nlevels)
    
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
    
    ## initialize curMin
    curMin <- Inf
    MinVariants <- numeric(0)
    for (i in 1:nrow(hilf)) {
        spalten <- c(unlist(mapply(function(obj1, obj2) obj1[, 
            obj2], cand.lists, hilf[i, ])))
        cur3 <- round(length3(ID[, spalten]), 4)
        if (cur3 == curMin) 
            MinVariants <- rbind(MinVariants, spalten)
        else if (cur3 < curMin) {
            curMin <- cur3
            MinVariants <- matrix(spalten, nrow = 1)
            if (curMin == 0 & !all) 
                return(list(GWP3 = 0, column.variants = matrix(spalten, 
                  nrow = 1), complete = FALSE))
        }
    }
    rownames(MinVariants) <- 1:nrow(MinVariants)
    list(GWP3 = curMin, column.variants = MinVariants, complete = TRUE)
}
