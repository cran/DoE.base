P4.4 <- function(ID, digits=4, rela=FALSE){
    ## function to calculate pattern of numbers of generalized length 4 words
    ## for projections into four factors
    if (!is.logical(rela)) stop("rela must be TRUE or FALSE")

    ## retrieve child array or array identified by character string
          ## gsub for case where ID is character string
    IDname <- gsub("\"","",deparse(substitute(ID)))
    if (all(IDname %in% oacat$name)){ 
    if (!exists(IDname)) 
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    else if (is.character(ID)) 
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    }

    if (rela & !(isTRUE(all.equal(length2(ID),0)) & isTRUE(all.equal(length3(ID),0)))) 
        stop(IDname, " is not a strength 3 array, \nP4.4 with rela=TRUE is inadequate.")
        

    if (!(is.data.frame(ID) | is.matrix(ID))) stop("ID must be a data frame or a matrix")
    if (is.matrix(ID)) ID <- as.data.frame(ID)
    if (!ncol(ID)>=4) return(NULL)  ## no projections onto 4 factors
    nlevels <- sapply(ID, function(obj) length(unique(obj)))
    hilf <- rep(0, choose(ncol(ID), 4))
    waehl <- nchoosek(ncol(ID),4)
    hilf <- apply(waehl, 2, function(obj) length4(ID[,obj]))
    if (rela) {
       div <- apply(waehl, 2, function(obj) min((nlevels[obj]-1)))
       aus <- table(round(hilf/div,digits))
       aus <- cbind(length4.rela=as.numeric(names(aus)),frequency=aus)
       rownames(aus) <- rep("",nrow(aus))
    }
    else{
       aus <- table(round(hilf,digits))
    ## formatting the table for output
       aus <- cbind(length4=as.numeric(names(aus)),frequency=aus)
       rownames(aus) <- rep("",nrow(aus))
    }
    aus  ## is a matrix
}