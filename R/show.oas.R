show.oas <- function(name="all", nruns = "all", nlevels = "all", factors="all", show = 10, parents.only = FALSE){
    ## preparation: check nlevels and factors and unite into one single case
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
    ## exclude or include child arrays 
    if (parents.only) zeige <- oacat[oacat$lineage=="",] 
        else zeige <- oacat
    ## treat name subsetting
    if (!identical(name,"all")){ 
        if (!is.character(name)) stop("name must be character")
        if (length(name)==0) stop("At least one name must be given.")
        if (sum(oacat$name %in% name)==0)
                 stop("none of the requested names found")
        if (!sum(oacat$name %in% name)==length(name))
                 warning("not all requested names found")
        zeige <- zeige[zeige$name %in% name,]
      }
    ## treat nruns subsetting
    if (!identical(nruns, "all")){
        if (!is.numeric(nruns)) stop("nruns must be numeric")
        if (!all(nruns %% 1 == 0)) stop("nruns must be integer")
        if (!length(nruns) %in% c(1,2)) stop("nruns must have one or two elements")
        if (length(nruns)==1) zeige <- zeige[zeige$nruns==nruns,]
             else zeige <- zeige[zeige$nruns>=min(nruns) & zeige$nruns<=max(nruns),]
    }
    ## treat factors (or nlevels which has been previously transformed into factors)
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
    ## treat the resulting data frame
    if (nrow(zeige)>0){
        ## make show="all" numeric
        if (show=="all") show <- nrow(zeige)
        ## display information, if not suppressed
        if (show > 0){
            if (show < nrow(zeige))
            cat(nrow(zeige), " designs found, \nthe first ", show, " are listed\n")
            else
            cat(nrow(zeige), " designs found\n")
            print(zeige[1:min(show,nrow(zeige)),,drop=FALSE][c("name","nruns","lineage")], quote=FALSE)
        }
        ## return information for further use
        invisible(zeige[c("name","nruns","lineage")])
    }
    else{ 
      cat("no such orthogonal array found\n")
      if (parents.only) cat("choose parent.only=FALSE in order to see which further arrays up to 143 runs can be manually constructed in what way.\n",
      "automatic creation of child arrays for increasing the number of available arrays is currently under development\n")
    }
}