show.oas <- function(name="all", nruns = "all", nlevels = "all", factors="all", show = 10){
    ## preparation: check nlevels and nfactors and unite into one single case
    if (!(identical(nlevels,"all") | identical(factors,"all")) )
        stop("nlevels and factors must not be specified simultaneously")
    if (!identical(nlevels,"all")){
        if (!is.numeric(nlevels)) stop("nlevels must be numeric")
        if (!all(nlevels%%1 == 0)) stop("all entries of nlevels must be integer")
        if (any(nlevels < 2)) stop("all entries of nlevels must be at least 2")
        hilf <- table(nlevels)
        factors  <- list(nlevels=as.numeric(names(hilf)), number=hilf)
        nlevels <- "all"
        }
    zeige <- oacat
    if (!identical(name,"all")){ 
        if (!is.character(name)) stop("name must be character")
        if (length(name)==0) stop("At least one name must be given.")
        if (sum(oacat$name %in% name)==0)
                 stop("none of the requested names found")
        if (!sum(oacat$name %in% name)==length(name))
                 warning("not all requested names found")
        zeige <- zeige[zeige$name %in% name,]
      }
    if (!identical(nruns, "all")){
        if (!is.numeric(nruns)) stop("nruns must be numeric")
        if (!all(nruns %% 1 == 0)) stop("nruns must be integer")
        if (!length(nruns) %in% c(1,2)) stop("nruns must have one or two elements")
        if (length(nruns)==1) zeige <- zeige[zeige$nruns==nruns,]
             else zeige <- zeige[zeige$nruns>=min(nruns) & zeige$nruns<=max(nruns),]
    }
    if (!identical(factors,"all")){
        if (!is.list(factors)) stop("factors must be a list")
        if (!length(factors)==2) stop("factors must have the element vectors nlevels and number")
        if (!identical(names(factors), c("nlevels","number")))
             stop("factors must have the element vectors nlevels and number")
        stufen <- factors$nlevels
        anzahl <- factors$number
        if (!length(stufen)==length(anzahl)) stop("factors$nlevels and factors$number must have the same length")
        if (!(is.numeric(stufen) & is.numeric(anzahl))) stop("factors$nlevels and factors$number must be numeric")
        for (i in 1:length(stufen))
           zeige <- zeige[zeige[,paste("n",stufen[i],sep="")]>=anzahl[i],]
    }
    if (nrow(zeige)>0){
        if (show < nrow(zeige))
        cat(nrow(zeige), " designs found, \nthe first ", show, " are listed\n")
        else
        cat(nrow(zeige), " designs found\n")
    for (i in 1:min(nrow(zeige),show)){
#        print(as.character(zeige[i,1]), quote=FALSE)
        cat(as.character(zeige[,1])[i],"\n")
        #if (!zeige$comment=="") cat(zeige$comment,"\n")
    }}
    else{ 
      cat("no such orthogonal array found\n")
      cat("you may be able to construct one from the parent arrays that are available\n")
      cat("automatic creation of child arrays for increasing the number of available arrays is currently under development\n")
    }
}