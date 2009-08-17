make.generators <- function(name,liste){
       g <- length(liste)
       k <- length(name)-g
       generators <- rep(list(""),g)
       for (i in 1:g) generators[[i]] <- formula(paste(name[k+i],"~",paste(name[liste[[i]]],collapse="*"),sep=""))
       generators
   }
   
make.formulas <- function(orignames, factor.names){
   eval(parse(text=paste("list(",
                paste(paste(orignames,"~(",names(factor.names),"-",sapply(factor.names,"mean"),")/",
                       sapply(factor.names,function(obj) (obj[2]-obj[1])/2), sep=""),collapse=","),
                ")",sep="")))
}

ord <- function(matrix, decreasing=FALSE){
    ## determines ordering vector that orders matrix 
    ##      w.r.t. first columns, second column etc.
    text <- "order(matrix[,1]"
    if (ncol(matrix)>1)
    for (i in 2:ncol(matrix)) text <- paste(text,",matrix[,",i,"]",sep="")
    text <- paste(text,", decreasing=",decreasing,")")
    eval(parse(text=text))
}


des.recode <- function (var, recodes, as.factor.result, char) 
{
    recode.list <- rev(strsplit(recodes, ";")[[1]])
    is.fac <- is.factor(var)
    if (missing(as.factor.result)) 
        as.factor.result <- is.fac
    if (missing(char)) char <- FALSE
    result <- var
    if (is.fac) 
        result <- as.character(result)
        
    for (term in recode.list){
        set <- eval(parse(text = strsplit(term, "=")[[1]][1]))
        if (!char)
        target <- eval(parse(text = strsplit(term, "=")[[1]][2]), 
            envir = parent.frame(), enclos = sys.frame(0))
        else 
        target <- strsplit(term, "=")[[1]][2]
        for (val in set){
            if (is.na(val)) 
                result[is.na(var)] <- target
            else result[var == val] <- target
        }
    }
    if (as.factor.result) 
        result <- as.factor(result)
    result
}

## accessor functions for attributes of orthogonal arrays
origin<-function(ID) attr(ID,"origin")
## not needed, as it is available in package base
#comment<-function(ID) attr(ID,"comment")
