P4.4 <- function (ID, digits = 4, rela = FALSE)
{
    ## function to calculate pattern of numbers of generalized length 4 words
    ## for projections into four factors
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
    if (rela & !(isTRUE(all.equal(length2(ID), 0)) & isTRUE(all.equal(length3(ID),
        0))))
        stop(IDname, " is not a strength 3 array, \nP4.4 with rela=TRUE is inadequate.")
    if (!(is.data.frame(ID) | is.matrix(ID)))
        stop("ID must be a data frame or a matrix")
    if (is.matrix(ID))
        ID <- as.data.frame(ID)
    if (!ncol(ID) >= 4)
        return(NULL)
    hilf <- length4(ID, J=TRUE)
    hilf <- sapply(split(hilf, names(hilf)), function(obj) sum(obj^2))
    if (rela) {
        waehl <- nchoosek(ncol(ID), 4)
        nlevels <- sapply(ID, function(obj) length(unique(obj)))
        div <- apply(waehl, 2, function(obj) min((nlevels[obj] -
            1)))
    }
    aus <- table(round(hilf, digits))
    if (rela) aus <- table(round(hilf/div, digits))
    ## formatting the table for output
    aus <- cbind(length4 = as.numeric(names(aus)), frequency = aus)
    if (rela) colnames(aus) <- c("length4.rela", "frequency")
    rownames(aus) <- rep("", nrow(aus))
    ## attaching attributes
    if (!rela) attr(aus, "A4") <- sum(hilf)
    else {
      attr(aus, "rA4") <- sum(hilf/div)
      attr(aus, "GR") <- round(4+1-sqrt(max(hilf/div)),2)
      attr(aus, "A4") <- sum(hilf)
      }
    aus
}
