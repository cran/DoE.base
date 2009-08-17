plot.design <- function(x, y=NULL, ...){
      xnam <- deparse(substitute(x))
      if (!"design" %in% class(x))
         graphics:::plot.design(x, y, ...)
      else{
         if (is.null(design.info(x)))
             graphics:::plot.design(x, y,...)
         else{
             ## now designs generated with suite DoE.base etc.
             di <- design.info(x)
             graphics <- FALSE
             table <- FALSE
             if (is.null(di$quantitative) | all(is.na(di$quantitative))){
                 if(!(is.null(y) & is.null(di$response.names))) graphics <- TRUE
                 else table <- TRUE
             }
             else if (!any(di$quantitative==TRUE)){
                 if(!(is.null(y) & is.null(di$response.names))) graphics <- TRUE
                 else table <- TRUE
                 }
             if (graphics){
                   if (is.null(y)) y <- response.names(x)
                   else (
                      if (is.character(y)){ 
                         if (!all(y %in% colnames(x))) stop("invalid names in y")}
                      else{
                        if (is.data.frame(y)) y <- as.matrix(y)
                        if (!is.numeric(y)) stop("columns in y must be numeric")
                      }
                   )
                   graphics:::plot.design(x, y, ...)
                   }
             if (table){
                   mosaic(table(x), ...)
             }
             if (!(table | graphics)) {if (is.null(y)) plot(undesign(x), ...)
                  else if (is.character(y)) plot(x[,c(names(di$factor.names),y)], ...)
                       else plot(cbind(x[,names(di$factor.names)],y), ...)
             }
         }
      }
}

