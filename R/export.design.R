export.design <- function(design, response.names=NULL, path=".", filename=NULL, 
          legend=NULL, type="html", OutDec=options("OutDec")$OutDec, replace=FALSE, ...){
     if (!(is.null(response.names) | is.character(response.names))) 
         stop("response.names must be a character vector of response names or NULL")
     if (!is.logical(replace)) stop("replace must be logical")
     if (!type %in% c("rda","html","csv")) stop("type must be one of rda, html or csv")
     if (!OutDec %in% c(".",",")) stop("OutDec must be one of . or ,")
     if (!(is.null(legend) | is.data.frame(legend))) stop("legend must be a data frame with legend information")
     desname <- deparse(substitute(design))
     if (!"design" %in% class(design)) stop("design must be of class design")
     if (!desname %in% ls(envir=.GlobalEnv)){ 
          if (is.null(filename)) stop("If design is calculated on the fly, filename must be given") 
          design <- eval(parse(text=desname)) ## calculate once and for all
          desname <- filename                 ## storage name  
          assign(desname, design, envir=.GlobalEnv)
          }
     if (is.null(filename)) filename <- desname
#          stop("design must refer to a stored object of class design and cannot be created on the fly")
     if (!is.null(response.names)){
         responses <- matrix("", nrow=nrow(design), ncol=length(response.names))
         colnames(responses) <- response.names
         df <- cbind(run.order(design), design, responses)
     }
     else df <- cbind(run.order(design), design)
     ## prepare check for existence of file
     if (!replace){ 
           lf <- tolower(list.files(path=path))
           if (tolower(paste(filename,"rda",sep=".")) %in% lf) 
               stop("file ", paste(filename,"rda","."), " exists in the chosen directory and is not replaced (replace=FALSE). Change directory, filename, or replace option.")
         }
     if (type=="html"){
         if (!replace){ 
           if (tolower(paste(filename,"html",sep=".")) %in% lf) 
               stop("file ", paste(filename,"html","."), " exists and is not replaced (replace=FALSE). Change directory, filename, or replace option.")
         }
         if (is.null(legend)){
             fn <- design.info(design)$factor.names
             ncols <- max(sapply(fn, "length"))
             for (i in 1:length(fn)) if (length(fn[[i]])<ncols) fn[[i]] <- c(fn[[i]], rep("",ncols-length(fn[[i]])))
             cn <- c("Factor", paste("Level",1:ncols,sep=""))
             legend <- data.frame(names(fn))
             for (i in 1:ncols) legend <- cbind(legend,sapply(fn, function(obj) obj[i]))
             colnames(legend) <- cn
             if (!is.call(design.info(design)$creator)){
                 fn <- design.info(design)$creator$faclablist
                 if (!all(fn=="")){
                      ## provision for extra error check columns for screening designs
                      if (nrow(legend) > length(fn)) fn <- c(fn, rep("error checking column", nrow(legend)-length(fn)))
                      legend <- cbind(legend, FactorLabel=fn)
                    }
                 }
         }
         hilf <- matrix("",nrow=nrow(df)-nrow(legend),ncol=ncol(legend))
         colnames(hilf) <- colnames(legend)
         df <- cbind(df, "_"=rep("",nrow(df)))
         df <- cbind(df, rbind(legend,hilf))
         html(df,file=paste(path,paste(filename,"html",sep="."),sep="/"),OutDec=OutDec, ...)
     }
     if (type=="csv") {
         if (!replace){ 
           if (tolower(paste(filename,"csv",sep=".")) %in% lf) 
               stop("file ", paste(filename,"csv","."), " exists and is not replaced (replace=FALSE).  Change directory, filename, or replace option.")
         }
         if (OutDec==",") write.csv2(df, file=paste(path,paste(filename,"csv",sep="."),sep="/"))
         else write.csv(df, file=paste(path,paste(filename,"csv",sep="."),sep="/"))
     }
     ## still export design as an image rda under the same name
     save(list=desname, file=paste(path,paste(filename,"rda",sep="."),sep="/") )
}