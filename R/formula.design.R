formula.design <- function(x, ..., response = NULL, degree = NULL){
      ## open: correct formula for blocked FrF2 designs in all cases
      ## either via block name variable or via individual 2-level variables with full degree
      
      ## open: aggregation of repeat.only designs
      ## open: aggregation of parameter designs
      
      ## error checks
      if (!"design" %in% class(x)) stop("This function is applicable to class design objects only.")
      if (!(is.null(degree) | is.numeric(degree))) stop("degree must be numeric or NULL")
      if (is.numeric(degree)){ 
           if(!degree > 0) stop("degree must be positive")
           if (!degree == round(degree)) stop("degree must be an integer number")
           }
      
      ## identify and check response candidates
      respnam <-design.info(x)$response.names
      respnamOK <- intersect(colnames(x),respnam)
      if (is.null(respnamOK) | length(respnamOK)==0)
          stop("For formula.design, the design requires at least one response to be available.")
      respnamOK <- respnamOK[which(sapply(x[,respnamOK], function(obj) all(!is.na(obj))))]
      if (length(respnamOK)==0) stop("the design does not contain any response variable with complete observations")
      respposOK <- which(design.info(x)$response.names %in% respnamOK)
      
      ## check response given by user
      if (!is.null(response)){
         if (!(is.character(response) | is.numeric(response))) 
              stop("response must be a character string of the response name or a position number")
         if (length(response)>1) stop("formula.design can only handle one response at a time")
         if (is.numeric(response)) {
               if (response < 1 | response > length(respnam) | !response==round(response)) 
                     stop("if numeric, response must be an integer from 1 to ", length(respnam))
               if (!response %in% respposOK) 
                     stop("response respnam[respposOK] is not available (missing data?)")
            }
         if (is.character(response)) {
               if (!response %in% colnames(x)) 
                     stop("response is not a column of x")
               if (!response %in% respnam) 
                     stop("response has not been declared a response variable")
               if (!response %in% respnamOK) 
                     stop("response has missing values, which precludes default analysis of the design")
            }
         }
      else response <- respnamOK[1]
      ## else: no response given by user
      
      type <- design.info(x)$type
      
      ## default degrees: 1 for pb and oa, 2 for everything else
      if (is.null(degree) & type %in% c("pb","oa")) degree <- 1
      if (is.null(degree)) degree <- 2

      factor.names <- design.info(x)$factor.names

      ## now degree is given
          if (degree==1)
             aus <- as.formula(paste(response, paste(names(factor.names),collapse="+"),sep="~"))
          if (degree > 1){ 
              if (type %in% c("pb","oa")) warning("degree > 1 is often inadequate with design types pb and oa")
              aus <- as.formula(paste(response, paste("(",paste(names(factor.names),collapse="+"),")^",degree,sep=""),sep="~"))
              if (type=="FrF2.blocked")
              aus <- as.formula(paste(response, paste("(",paste(names(factor.names),collapse="+"),")^",degree,sep=""),sep="~"))
                  ## make correct for blocked designs: block main effects, degree applied to other experimental columns only
              }
       aus   
}