P4.3 <- function(ID, digits=4){
    ## function to calculate pattern of numbers of generalized length 3 words
    ## for projections into four factors

    ## retrieve child array or array identified by character string
          ## gsub for case where ID is character string
    IDname <- gsub("\"","",deparse(substitute(ID)))
    if (all(IDname %in% oacat$name)){ 
    if (!exists(IDname)) 
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    else if (is.character(ID)) 
          ID <- eval(parse(text=paste("oa.design(",IDname,")")))
    }

    if (!(is.data.frame(ID) | is.matrix(ID))) stop("ID must be a data frame or a matrix")
    if (is.matrix(ID)) ID <- as.data.frame(ID)
    if (!ncol(ID)>=4) return(NULL)  ## no projections onto 4 factors
    hilf <- rep(0, choose(ncol(ID), 4))
    waehl <- nchoosek(ncol(ID),4)
    hilf <- apply(waehl, 2, function(obj) length3(ID[,obj]))
    aus <- table(hilf)
    ## formatting the table for output
    aus <- cbind(length3=round(as.numeric(names(aus)),digits),frequency=aus)
    colnames(aus) <- c("length3","frequency")
    rownames(aus) <- rep("",nrow(aus))
    aus  ## is a matrix
}