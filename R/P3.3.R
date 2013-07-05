P3.3 <- function (ID, digits = 4, rela = FALSE, detailed=FALSE){
    ## function to calculate pattern of numbers of generalized length 3 words
    ## for projections into three factors
    if (!is.logical(rela))
        stop("rela must be TRUE or FALSE")

    ## retrieve child array or array identified by character string
          ## gsub for case where ID is character string
    IDname <- gsub("\"", "", deparse(substitute(ID)))
    if (all(IDname %in% oacat$name)) {
        if (!exists(IDname))
            ID <- eval(parse(text = paste("oa.design(", IDname,
                ")")))
        else if (is.character(ID))
            ID <- eval(parse(text = paste("oa.design(", IDname,
                ")")))
    }
    if (rela & !(isTRUE(all.equal(length2(ID), 0))))
        stop(IDname, " is not an orthogonal array, \nP3.3 with rela=TRUE is inadequate.")
    if (!(is.data.frame(ID) | is.matrix(ID)))
        stop("ID must be a data frame or a matrix")
    if (is.matrix(ID))
        ID <- as.data.frame(ID)
    if (!ncol(ID) >= 3)
        return(NULL)
    hilf <- length3(ID, J=TRUE)
    fhilf <- factor(names(hilf), levels=unique(names(hilf))) ## bug fix 11 Feb 2013
                       ## hilf was in unexpected order before, 
                       ## yielding wrong calculations for rela in designs 
                       ## with many columns due to character instead of 
                       ## numeric sorting
    hilf <- sapply(split(hilf, fhilf), function(obj) sum(obj^2))
    if (rela) {
       # divisors are created from nlevels by relative numbers
        waehl <- nchoosek(ncol(ID), 3)
        nlevels <- sapply(ID, function(obj) length(unique(obj)))
        div <- apply(waehl, 2, function(obj) min((nlevels[obj] -
            1)))
    }
    aus <- table(round(hilf, digits))
    if (rela) aus <- table(round(hilf/div, digits))
    ## formatting the table for output
    aus <- cbind(length3 = as.numeric(names(aus)), frequency = aus)
    if (rela) colnames(aus) <- c("length3.rela", "frequency")
    rownames(aus) <- rep("", nrow(aus))
    ## attaching attributes
    if (!rela) {
       attr(aus, "A3") <- A3 <- sum(hilf)
       if (detailed & A3 > 0) attr(aus, "detail") <- round(hilf, digits)
       }
    else {
      attr(aus, "rA3") <- rA3 <- sum(hilf/div)
      if (rA3 > 0) attr(aus, "GR") <- round(3+1-sqrt(max(hilf/div)), digits)
          else attr(aus, "GR") <- ">=4"
      attr(aus, "A3") <- sum(hilf)
      if (detailed & rA3 > 0) attr(aus, "detail") <- round(hilf/div, digits)
    }
    aus
}
