
direct.sum <- function (D1, D2, ..., tiebreak = letters)
{
    ## taken from package conf.design by Bill Venables
    ## commented out class "design"
    ## added code for keeping contrasts
    l <- list(...)
    if (length(l))
        return(Recall(D1, Recall(D2, ..., tiebreak = tiebreak[-1]),
            tiebreak = tiebreak))
    f1 <- lapply(D1, "is.factor")
    cc <- function(x) {if (is.factor(x)) contrasts(x) else NULL}
    c1 <- lapply(D1, "cc")
    E1 <- lapply(D1, function(x, n2) rep(x, rep(n2, length(x))),
        nrow(D2))
    f2 <- lapply(D2, "is.factor")
    c2 <- lapply(D2, "cc")
    E2 <- lapply(D2, function(x, n1) rep(x, n1), nrow(D1))
    D <- c(E1, E2)
    if (any(i <- duplicated(names(D))))
        names(D)[i] <- paste(names(D)[i], tiebreak[1], sep = "")
    ## reinstate contrasts for factors to previous state
    ff <- function(x, f, c){
         if (f) contrasts(x) <- c
         x}
    D <- mapply("ff", D, f=c(f1,f2), c=c(c1,c2),SIMPLIFY=FALSE)
    D <- as.data.frame(D)
 #   class(D) <- c("design", class(D))
    D
}



cross.design <- function (design1, design2, ..., randomize=TRUE, seed=NULL)
{
    creator <- sys.call()
    workhorse <- function(design1, design2, ...){
    ## taken from package conf.design by Bill Venables
    ## needed as inner function, since postprocessing is not to be racalled
    nam <- deparse(substitute(design2))  ## just in case, for one-column design2
    l <- list(...)
    if (length(l))
        return(Recall(design1, Recall(design2, ...)))
    if (!"design" %in% class(design1)){ 
            warning("design1 is not of class design, all design information is lost")
            cn <- NULL
            if (is.numeric(design2)) cn <- nam
            if (!is.data.frame(design2)) design2 <- data.frame(design2)
            if (is.null(cn)) colnames(design2) <- nam
            aus <- direct.sum(design1, design2, ..., tiebreak = letters)
            if (randomize){ 
               if (!is.null(seed)) set.seed(seed)
               aus <- aus[sample(nrow(aus)),]
               }
            return(aus)
        }
    else{
        di1 <- design.info(design1)
        if (di1$type=="FrF2.blocked") stop("crossing blocked designs is not supported")
        ro1 <- run.order(design1)
        if (di1$repeat.only) stop("only last design can have repeat.only replications")
        des1 <- undesign(design1)
        desn1 <- desnum(design1)
    if ("design" %in% class(design2)){
        di2 <- design.info(design2)
        if (any(di2$type=="FrF2.blocked")) stop("crossing blocked designs is not supported")
        ro2 <- run.order(design2)
        des2 <- undesign(design2)
        desn2 <- desnum(design2)
    }else
        if (is.data.frame(design2) | is.matrix(design2) | is.list(design2) | is.array(design2))
             stop("design2 must be a vector or a data frame of class design")
        else {  ##redo vector into design object
            tab <- table(design2)
            repl <- 1
                if (!min(tab)==max(tab)) type2 <- "vector.unbalanced"
                     else if (max(tab)>1) {type2 <- "vector.replicated"
                                           repl <- max(tab)
                                           }
                          else type2 <- "vector"
            nruns <- length(design2)
            if (type2=="vector.replicated") nruns <- round(nruns/repl)
            nfactors <- 1
            factor.names <- list(names(tab))
            names(factor.names) <- nam 
            ro2 <- data.frame(run.no.in.std.order = match(design2, names(tab)),
                         run.no = 1:length(design2))
            ro2$run.no.std.rp <- ro2$run.no.in.std.order
               if (!type2=="vector") for (i in 1:length(design2))
                    ro2$run.no.std.rp[i] <- paste(ro2$run.no.std.rp[i],
                         cumsum(ro2$run.no.in.std.order==ro2$run.no.std.rp[i])[i],".")
            desn2 <- NULL
            if (is.numeric(design2))
                         desn2 <- matrix(design2,ncol=1, dimnames=list(NULL, nam))
            if (is.character(design2)) design2 <- factor(design2, levels=unique(design2))
            des2 <- design2 <- as.data.frame(design2)
            if (!is.null(nam)) colnames(des2) <- colnames(design2) <- nam
            class(design2) <- c("design","data.frame")
            if (is.null(desn2))
               desnum(design2) <- desn2 <- model.matrix(as.formula(paste("~",nam)),design2)[,-1,drop=FALSE]
               else desnum(design2) <- desn2
               run.order(design2) <- ro2
               design.info(design2) <- di2 <- list(type=type2, nruns=nruns,
                      nfactors=nfactors, factor.names=factor.names,
                      replications=repl, repeat.only=NULL,
                      randomize=NULL, seed=NULL, creator=nam)
            }
    if (any(duplicated(c(colnames(design1),colnames(design2)))))
       stop ("duplicated factor names are not permitted when crossing designs")
    D <- as.data.frame(direct.sum(des1,des2))
    class (D) <- c("design", "data.frame")
    Dn <- as.matrix(direct.sum(as.data.frame(desn1), as.data.frame(desn2)))
    rownames(Dn) <- rownames(D)
    desnum(D) <- Dn
    touter <- function(obj1,obj2,FUN,...) t(outer(obj1,obj2,FUN=FUN,...))
    ro <- as.data.frame(mapply("touter",ro1, ro2, "paste", sep="_"),"t")
    run.order(D) <- ro
    
    ## create reasonable content for design.info
    ## accomodate randomize and replications
    
    ## function for combining design info from two designs
    cc <- function(d1,d2){
         if (is.list(d1) & is.list(d2)) return(list(d1,d2))
         if (is.list(d1) & !is.list(d2)) return(c(d1,list(d2)))
         if (is.list(d2) & !is.list(d1)) return(c(list(d1),d2))
         if (!(is.list(d1) | is.list(d2))) return(c(d1,d2))
         }
    ## different for seed because seed can be NULL in some or all designs, 
    ## and c does not leave a position for NULL
    cc.seed <- function(d1,d2){
         if (is.list(d1) & is.list(d2)) return(list(d1,d2))
         if (is.list(d1) & !is.list(d2)) return(c(d1,list(d2)))
         if (is.list(d2) & !is.list(d1)) return(c(list(d1),d2))
         if (!(is.list(d1) | is.list(d2))) return(c(list(d1),list(d2)))
         }
    cc.alias <- function(d1,d2){
         if (is.list(d1) & is.list(d2)) return(list(d1,d2))
         if (is.list(d1) & !is.list(d2)) return(list(d1,list(d2)))
         if (is.list(d2) & !is.list(d1)) return(list(list(d1),d2))
         if (!(is.list(d1) | is.list(d2))) return(list(list(d1),list(d2)))
         }
    di <- vector("list") ## empty list
    
    for (nn in union(names(di1), names(di2))){
         if (nn=="seed") di[[nn]] <- cc.seed(di1[[nn]],di2[[nn]])
         if (nn=="aliased") di[[nn]] <- cc.alias(di1[[nn]],di2[[nn]])
         if (!nn %in% c("seed","aliased"))
            di[[nn]] <- cc(di1[[nn]],di2[[nn]])
      }
    
    ## manually combine infos that otherwise do not work properly both interim and finally
    di$factor.names <- c(factor.names(design1),factor.names(design2))
    if (!is.list(di$selected.columns)) 
        di$selected.columns <- list(di1$selected.columns ,di2$selected.columns )
    if (is.null(di$cross.nruns)) di$cross.nruns <- di$nruns
       else di$cross.nruns <- c(di1$nruns, di$cross.nruns)
    di$nruns <- prod(di$nruns)
    if (is.null(di$cross.replications)) di$cross.replications <- di$replications
       else di$cross.replications <- c(di1$replications, di$cross.replications)
    di$replications <- prod(di$replications)

    design.info(D) <- di
    if (any(di$repeat.only) & di$replications>di2$replications) 
        warning("repeat.only replications and proper replications mixed in one crossed design, this does not work with any post-processing!")
       }
    D
    }
    D <- workhorse(design1, design2, ...)
    
    ## no postprocessing, if design1 was not a design
    if (!"design" %in% class(D)) return(D) 
    
    ## postprocessing for designs
    di <- design.info(D)
    
    ## modify design info
    
    ## still missing: adaptation of things like alias information!
    di$cross.nfactors <- di$nfactors
    di$nfactors <- sum(di$nfactors)
    di$cross.types <- di$type
    di$type <- "crossed"
    di$cross.randomize <- di$randomize
    di$cross.seed <- di$seed
    di$randomize <- randomize
    di$seed <- seed
    if (all(sapply(di$selected.columns, "is.null"))) di$selected.columns <- NULL
    di$cross.selected.columns <- di$selected.columns
    di$selected.columns <- NULL
    if (is.null(di$seed)) di["seed"] <- list(NULL)
    di$creator <- list(original=di$creator, modify=creator)
    di$cross.repeat.only <- di$repeat.only
    di$repeat.only <- any(di$cross.repeat.only)
    ## postprocess alias list, if exists
    if (!is.null(di$aliased)){
          dia <- di$aliased
          if (di$nfactors<=50){ 
                    nam <- Letters[1:di$nfactors]
                    sepchar <- ""
                    }
                    else{ 
                    nam <- paste("F",1:di$nfactors,sep="")
                    sepchar <- ":"
                    }
          for (i in 1:length(di$aliased)){
             if (!is.null(dia[[i]])){
             if (di$cross.nfactors[i] <=50){ 
                    namalt <- Letters[1:di$cross.nfactors[i]]
                    sepcharalt <- ""
                    }
                    else{ 
                    namalt <- paste("F",1:di$cross.nfactors[i],sep="")
                    sepcharalt <- ":"
                    }
             ## replace backward so that overlap between namalt and nam 
             ## is not problematic (namalt>=nam)
             if (i>1)
             nami <- nam[(1:di$cross.nfactors[i])+sum(di$cross.nfactors[1:(i-1)])]
             else nami <- nam[1:di$cross.nfactors[1]]
             if (!all(namalt==nami)){
             for (j in length(namalt):1){ 
                 ## identical sepchar
                 if (sepcharalt==":" | sepchar==""){
                    ## the following code relies on the fact tha both sepchars are equal 
                    ## (which they should be, since >50 factors cannot occur in individual design if not also in crossed)
                    
                    dia[[i]][["legend"]] <- sub(paste("^",namalt[j],"=",sep=""),paste(nami[j],"=",sep=""),dia[[i]][["legend"]])
                    ## main
                    ## factor before equal or next factor
                    dia[[i]][["main"]] <- gsub(paste(namalt[j],"([=",sepcharalt,"[:alpha:]]{1})",sep=""),
                            paste(nami[j],"\\1",sep=""),dia[[i]][["main"]])
                    ## last factor
                    dia[[i]][["main"]] <- sub(paste(namalt[j],"$",sep=""), nami[j], dia[[i]][["main"]])
                    
                    ## now fi2
                    dia[[i]][["fi2"]] <- gsub(paste(namalt[j],"([=",sepcharalt,"[:alpha:]]{1})",sep=""),
                            paste(nami[j],"\\1",sep=""),dia[[i]][["fi2"]])
                    dia[[i]][["fi2"]] <- gsub(paste(namalt[j],"$",sep=""), nami[j],dia[[i]][["fi2"]])
                    
                    ## finally fi3
                    if (!is.null(dia[[i]][["fi3"]])){
                        dia[[i]][["fi3"]] <- gsub(paste(namalt[j],"([=",sepcharalt,"[:alpha:]]{1})",sep=""),
                            paste(nami[j],"\\1",sep=""),dia[[i]][["fi3"]])
                        dia[[i]][["fi3"]] <- gsub(paste(namalt[j],"$",sep=""), nami[j],dia[[i]][["fi3"]])
                        }
                    }
                 else{
                    ## sepcharalt "" and sepchar ":"
                    ## also means that alt is an individual letter, while new is not
                    ## only letter that could be problematic: F
                    dia[[i]][["legend"]] <- sub(paste("^",namalt[j],sep=""),paste(nami[j],sep=""),dia[[i]][["legend"]])
                    dia[[i]][["main"]] <- sub(paste("^",namalt[j],sep=""),paste(nami[j],sep=""),dia[[i]][["main"]])
                    dia[[i]][["main"]] <- gsub(paste("=",namalt[j],sep=""),paste("=",nami[j],sepchar,sep=""),dia[[i]][["main"]])
                    dia[[i]][["main"]] <- gsub(paste(namalt[j],"=",sep=""),paste(nami[j],"=",sep=""),dia[[i]][["main"]])
                    dia[[i]][["main"]] <- gsub(paste(namalt[j],"([[:alpha:]{1}])",sep=""),paste(nami[j],sepchar,"\\1",sep=""),dia[[i]][["main"]])
                    dia[[i]][["main"]] <- gsub(paste(namalt[j],"$",sep=""),nami[j],dia[[i]][["main"]])
                    dia[[i]][["fi2"]] <- sub(paste("^",namalt[j],sep=""),paste(nami[j],sepchar,sep=""),dia[[i]][["fi2"]])
                    dia[[i]][["fi2"]] <- sub(paste("=",namalt[j],sep=""),paste("=",nami[j],sepchar,sep=""),dia[[i]][["fi2"]])
                    dia[[i]][["fi2"]] <- sub(paste(namalt[j],"=",sep=""),paste(nami[j],"=",sep=""),dia[[i]][["fi2"]])
                    dia[[i]][["fi2"]] <- gsub(paste(namalt[j],"([[:alpha:]{1}])",sep=""),paste(nami[j],sepchar,"\\1",sep=""),dia[[i]][["fi2"]])
                    dia[[i]][["fi2"]] <- sub(paste(namalt[j],"$",sep=""),nami[j],dia[[i]][["fi2"]])
                    if (!is.null(dia[[i]][["fi3"]])){
                    dia[[i]][["fi3"]] <- sub(paste("^",namalt[j],sep=""),paste(nami[j],sepchar,sep=""),dia[[i]][["fi3"]])
                    dia[[i]][["fi3"]] <- sub(paste("=",namalt[j],sep=""),paste("=",nami[j],sepchar,sep=""),dia[[i]][["fi3"]])
                    dia[[i]][["fi3"]] <- sub(paste(namalt[j],"=",sep=""),paste(nami[j],"=",sep=""),dia[[i]][["fi3"]])
                    dia[[i]][["fi3"]] <- gsub(paste(namalt[j],"([[:alpha:]{1}])",sep=""),paste(nami[j],sepchar,"\\1",sep=""),dia[[i]][["fi3"]])
                    dia[[i]][["fi3"]] <- sub(paste(namalt[j],"$",sep=""),nami[j],dia[[i]][["fi3"]])
                    }
                    }
                    
         }
        }}}
         di$aliased <- dia 
        }
    design.info(D) <- di
    ## now randomize if requested
    if (randomize){ 
        if (!is.null(seed)) set.seed(seed)
        if (!di$repeat.only) D <- D[sample(nrow(D)),]
        else {repl.repeatonly <- di$cross.replications[length(di$cross.replications)]
               D <- D[rep(repl.repeatonly*(sample(1:round(nrow(D)/repl.repeatonly))-1),each=repl.repeatonly) + 
                    rep(1:repl.repeatonly,round(nrow(D)/repl.repeatonly))]}
        }
    D
}

