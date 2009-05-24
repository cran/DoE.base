"[.design" <- function(x,i,j,drop.attr=TRUE,drop=FALSE){
    #   if (missing(i)) i <- 1:nrow(x)
    #   if (missing(j)) j <- 1:ncol(x)
    
    if (missing(j)){
       if (identical(sort(i),1:nrow(x)) | identical(i,rep(TRUE, nrow(x)))) {
           ## only rearrange rows
           class(x) <- "data.frame"
           x<-x[i,]
           attr(x,"run.order") <- attr(x,"run.order")[i,]
           attr(x,"desnum") <- attr(x,"desnum")[i,]
           class(x) <- c("design","data.frame")
           return(x)}
       else{
           ## subset rows
       if (!drop.attr){
         design.info <- attr(x, "design.info")
         desnum <- attr(x, "desnum")
         run.order <- attr(x, "run.order")
         class(x) <- "data.frame"
           aus <- x[i,j,drop=drop]
           class(aus) <- c("design","data.frame")
           attr(aus, "desnum") <- desnum[i,j,drop=drop]
           attr(aus, "run.order") <- run.order[i,,drop=FALSE]
           attr(aus, "design.info") <- list(type="subset of design", 
               subset.rows=i, orig.design.info = design.info)
       }
       else{
           attr(x, "desnum") <- NULL
           attr(x, "run.order") <- NULL
           attr(x, "design.info") <- NULL
           class(x) <- "data.frame"
           aus <- x[i,,drop=drop]
         }
         }
       }
    else { class(x) <- "data.frame"
           aus <- x[i,j,drop=drop]}
          
       aus
       }

desnum <- function(design){
     if (!"design" %in% class(design)) stop("desnum is applicable for class design only.")
     else attr(design,"desnum")
 }
 
run.order <- function(design){
     if (!"design" %in% class(design)) stop("run.order is applicable for class design only.")
     else attr(design,"run.order")
 }
 
design.info <- function(design){
     if (!"design" %in% class(design)) stop("design.info is applicable for class design only.")
     else attr(design,"design.info")
 }