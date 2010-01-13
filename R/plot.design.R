plot.design <- function(x, y=NULL, select=NULL, ...){
      xnam <- deparse(substitute(x))
      if (!"design" %in% class(x))
         graphics:::plot.design(x, y, ...)
      else{
         if (is.null(design.info(x)))
             graphics:::plot.design(x, y,...)
         else{
             ## now designs generated with suite DoE.base etc.
             di <- design.info(x)
             ## first process select 
             if (is.null(select)) select <- names(di$factor.names)
             else if (is.numeric(select)){ 
                if (!all(select %in% 1:di$nfactors))
                   stop("If numeric, select must contain integer numbers from 1 to the number of factors")
                select <- names(di$factor.names)[select]
                }
             if (is.character(select)){
                if (all(select %in% Letters[1:di$nfactors]) & !all(names(di$factor.names) %in% Letters[1:di$nfactors]))
                    select <- names(di$factor.names)[which(Letters %in% select)]
                if (!all(select %in% names(di$factor.names)))
                    stop("select is invalid")
             }
             
             
             graphics <- FALSE
             table <- FALSE
             if (is.null(di$quantitative)){
                 if(!(is.null(y) & is.null(di$response.names))) graphics <- TRUE
                 else table <- TRUE
             }
             else{ 
              if (all(is.na(di$quantitative)) | !any(di$quantitative==TRUE)){
                 if(!(is.null(y) & is.null(di$response.names))) graphics <- TRUE
                 else table <- TRUE
                 }
             }
             if (graphics){
                   if (is.null(y)) y <- x[,response.names(x)]
                   else {
                      if (is.character(y)){ 
                         if (!all(y %in% colnames(x))) stop("invalid names in y")
                         y <- x[,y]
                         }
                      }
                  if (is.data.frame(y)) y <- as.matrix(y)
                  if (!is.numeric(y)) stop("columns in y must be numeric")
                  
                   graphics:::plot.design(x[,c(select)], y, ...)
                   }
             if (table){
                   mosaic(table(x[,select]), ...)
             }
             if (!(table | graphics)) {if (is.null(y)) plot(undesign(x)[,select], ...)
                  else if (is.character(y)) plot(x[,c(select,y)], ...)
                       else plot(cbind(x[,select],y), ...)
             }
         }
      }
}

