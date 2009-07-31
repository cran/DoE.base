paramtowide <- function(design, constant=NULL, ...){
    if (!"design" %in% class(design)) stop("design must be of class design")
    di <- design.info(design)
    if (!length(grep("param",di$type))>0)
         stop("this function works for parameter designs only (inner / outer array)")
    if (length(grep("wide",di$type))>0)
         stop("this design is already in wide format")
    
    hilf <- design
    ro <- run.order(hilf)
    ro$run.no.outer <- sapply(lapply(strsplit(as.character(ro$run.no),"_",fixed=TRUE),"rev"),function(obj) obj[1])
    ro$run.no <- sapply(strsplit(as.character(ro$run.no),"_",fixed=TRUE),function(obj) obj[1])
    ro$run.no.std.rp <- sapply(strsplit(as.character(ro$run.no.std.rp),"_",fixed=TRUE),function(obj) obj[1])
    ro$run.no.in.std.order <- sapply(strsplit(as.character(ro$run.no.in.std.order),"_",fixed=TRUE),function(obj) obj[1])
    nouter <- length(table(ro$run.no.outer))
    hilf$run.no <- ro$run.no
    hilf$run.no.outer <- ro$run.no.outer
    if (is.null(response.names(hilf)))
       response.names(hilf) <- "y"  ## new variable with missings
    fn <- names(factor.names(hilf))
    desnum <- desnum(hilf)
    di <- design.info(hilf)  ## redo in case response names have been updated
    fninner <- names(di$inner)
    fnouter <- names(di$outer)
    des.outer <- design[,fnouter][1:nouter,]
    ## extracted all design info now
    hilf <- undesign(hilf) 
    constant <- c(constant)
    hilf <- hilf[,setdiff(colnames(hilf),fnouter)]
    aus <- reshape(hilf, v.names=c(setdiff(colnames(hilf), c("run.no.outer","run.no",fn,constant))), 
           idvar=c("run.no",fninner),timevar="run.no.outer", direction="wide")
    ro <- reshape(ro, v.names=c("run.no.in.standard.order"), 
             idvar="run.no",timevar="run.no.outer",direction="wide")
    ## bring back into class design 
    ## adjust replication info and response names
    ## provide additional design.info for reshaping back to long
        rnlong <- di$response.names
        restlong <- setdiff(colnames(hilf),c(fn, rnlong, constant, "run.no", "run.no.outer"))
        di$response.names <- c(t(outer(rnlong, attr(aus,"reshapeWide")$times, paste, sep=".")))
        di$format <- "innerouterWide"
        di$type <- paste(di$type,"wide",sep="")
        di$responselist <- as.data.frame(t(outer(rnlong, attr(aus,"reshapeWide")$times, paste, sep=".")),
               stringsAsFactors = FALSE)
        colnames(di$responselist) <- rnlong
        if (length(restlong)>0){
           di$restlist <- as.data.frame(t(outer(restlong, attr(aus,"reshapeWide")$times, paste, sep=".")),
               stringsAsFactors = FALSE)
           colnames(di$restlist) <- restlong
        }
        if (!is.null(di$restlist)) restlnames <- c(as.matrix(di$restlist)) else restlnames <- NULL
        cn <- colnames(aus)
        vor <- cn[1:(min(which(cn %in% c(di$response.names,restlnames)))-1)]
        rest <- setdiff(cn, c(di$response.names, restlnames, vor))
        aus <- aus[,c(vor,di$response.names,restlnames, rest)]
    ### ??? switch off warning here for NA from transformation ???
        desnum <- cbind(model.matrix(as.formula(paste("~",paste(fninner,collapse="+"))),data=aus)[,-1], 
            as.matrix(as.data.frame(lapply(aus[,setdiff(colnames(aus),c(fninner, "run.no"))], "as.numeric"))))
        aus <- aus[,setdiff(colnames(aus),"run.no")]
        ## bring column names of desnum into line with those of design for FrF2 and pb designs
        ##    model.matrix has messed them up for the factors
        if (substr(di$type,1,4)%in%c("FrF2","pb")) colnames(desnum) <- colnames(aus)
        ## remove reshape wide information
        ## leave run.order attribute in the changed form, if not too annoying to distinguish these cases
        attr(aus, "reshapeWide") <- NULL
        attr(ro, "reshapeWide") <- NULL
    ## bring ro back to normal column content (additional columns do not add info)
    ro$run.no.std.rp <- ro$run.no.in.std.order
    ro <- ro[,c("run.no.in.std.order","run.no","run.no.std.rp")]
    rownames(aus) <- rownames(desnum) <- rownames(ro) <- ro$run.no
    ## attach attributes to design again
    class(aus) <- c("design", class(aus))
    desnum(aus) <- desnum
    run.order(aus) <- ro
    di$nruns <- di$cross.nruns[1]
    di$outer <- des.outer
    di$cross.nruns <- NULL
    di$factor.names <- di$factor.names[1:di$cross.nfactors[1]]
    di$nfactors <- di$cross.nfactors[1]
    di$cross.nfactors <- NULL
    design.info(aus) <- di
    aus
 }
