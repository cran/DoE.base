export.design <- function(design, response.names="y", path=".", legend=NULL, type="html", OutDec=options("OutDec")$OutDec, ...){
     if (!is.character(response.names)) stop("response.names must be a character vector of response names")
     if (!type %in% c("html","csv")) stop("type must be one of html or csv")
     if (!OutDec %in% c(".",",")) stop("OutDec must be one of . or ,")
     if (!(is.null(legend) | is.data.frame(legend))) stop("legend must be a data frame with legend information")
     desname <- deparse(substitute(design))
     if (!"design" %in% class(design)) stop("design must be of class design")
     if (!desname %in% ls(envir=.GlobalEnv)) stop("design must refer to a stored object of class design and cannot be created on the fly")
     responses <- matrix("", nrow=nrow(design), ncol=length(response.names))
     colnames(responses) <- response.names
     df <- cbind(run.order(design), design, responses)
     if (type=="html"){
         if (is.null(legend)){
             fn <- design.info(design)$factor.names
             ncols <- max(sapply(fn, "length"))
             for (i in 1:length(fn)) if (length(fn[[i]])<ncols) fn[[i]] <- c(fn[[i]], rep("",ncols-length(fn[[i]])))
             cn <- c("Factor", paste("Level",1:ncols,sep=""))
             legend <- data.frame(names(fn))
             for (i in 1:ncols) legend <- cbind(legend,sapply(fn, function(obj) obj[i]))
             colnames(legend) <- cn
         }
         hilf <- matrix("",nrow=nrow(df)-nrow(legend),ncol=ncol(legend))
         colnames(hilf) <- colnames(legend)
         df <- cbind(df, "_"=rep("",nrow(df)))
         df <- cbind(df, rbind(legend,hilf))
         html(df,file=paste(path,paste(desname,"html",sep="."),sep="/"),OutDec=OutDec, ...)
     }
     else {
         if (OutDec==",") write.csv2(df, file=paste(path,paste(desname,"csv",sep="."),sep="/"))
         else write.csv(df, file=paste(path,paste(desname,"csv",sep="."),sep="/"))
     }
     ## still export design as an image rda under the same name
     save(list=desname, file=paste(path,paste(desname,"rda",sep="."),sep="/") )
}