print.design <- function(x,show.order=NULL, ...){
   if (!"design" %in% class(x)) stop("this function works for class design objects only")
   di <- design.info(x)
   if (is.null(show.order)) 
       show.order <- di$type %in% c("FrF2.blocked", "FrF2.blockedcenter", "FrF2.splitplot", "FrF2.splitplot.folded", "crossed") | 
           length(grep("param",di$type))>0 | di$replications>1
   if (show.order)
   print(cbind(run.order(x)[,2:3],x), ...)
   else 
   print(cbind(x), ...)
   cat("class=design, type=", di$type,"\n")
   if (show.order) message("NOTE: columns run.no and run.no.std.rp are annotation, not part of the data frame")
   if (length(grep("param",di$type))>0 & length(grep("wide",di$type))>0 ){
      cat("Outer array:\n")
      print(di$outer, ...)
      }
}

## these methods allow to use the view data button in Rcmdr with reasonable printed output
showData <- function(dataframe, 
       colname.bgcolor = "grey50", 
       rowname.bgcolor = "grey50", 
       body.bgcolor = "white", 
       colname.textcolor = "white", 
       rowname.textcolor = "white", 
       body.textcolor = "black",
       font = "Courier 12", 
       maxheight = 30, 
       maxwidth = 80, 
       title = NULL,
       rowname.bar = "left",
       colname.bar = "top",
       rownumbers = FALSE, 
       placement = "-20-40",
       suppress.X11.warnings = TRUE){
  UseMethod("showData")
}

showData.default <- relimp::showData

showData.design <- function(dataframe, colname.bgcolor = "grey50", 
       rowname.bgcolor = "grey50", 
       body.bgcolor = "white", 
       colname.textcolor = "white", 
       rowname.textcolor = "white", 
       body.textcolor = "black",
       font = "Courier 12", 
       maxheight = 30, 
       maxwidth = 80, 
       title = NULL,
       rowname.bar = "left",
       colname.bar = "top",
       rownumbers = FALSE, 
       placement = "-20-40",
       suppress.X11.warnings = TRUE) {
   datnam <- deparse(substitute(dataframe))
   if (!"design" %in% class(dataframe))
       stop("This method is for class design data frames only.")
   showData(undesign(dataframe),colname.bgcolor=colname.bgcolor, 
      rowname.bgcolor=rowname.bgcolor, body.bgcolor=body.bgcolor,
      colname.textcolor=colname.textcolor, rowname.textcolor=rowname.textcolor,
      body.textcolor=body.textcolor, font=font,maxheight=maxheight,
      maxwidth=maxwidth,title=datnam, rowname.bar=rowname.bar, colname.bar=colname.bar,
      rownumbers=rownumbers,placement=placement,suppress.X11.warnings=suppress.X11.warnings)
   }

summary.design <- function(object,...){
   di <- design.info(object)
   if (is.language(di$creator)){ 
       cat("Call:\n")
       print(di$creator, ...)
       cat("\n")
       }
   else if (length(class(di$creator))>1)
       message("design was generated with RcmdrPlugin.DoE\n\n")
       cat("Experimental design of type ", di$type,"\n")
       cat(di$nruns, " runs\n\n")
## ??? how to handle blocks from ccd ???
   blocks <- di$blocks
   if (is.null(blocks)) blocks <- 1
       if (blocks > 1){
          cat("blocked design with ", di$blocks, " blocks\n")
          if (di$bbreps>1)
             cat("each type of block independently conducted ", di$bbreps, " times\n")
          if (di$wbreps>1 & !di$repeat.only)
             cat("each run within each block independently conducted ", di$wbreps, " times\n")
          if (di$wbreps>1 & di$repeat.only)
             cat("each run measured ", di$wbreps, " times (no proper replication)\n")
       }
    else if (di$replications>1)
      if (di$repeat.only)
         cat(di$replications, " measurements per run (not proper replications)\n")
      else
         cat("each run independently conducted ", di$replications, " times\n")

#   nlevels <- di$nlevels
#   if (is.null(nlevels))
#      nlevels <- sapply(di$factor.names, "length")
#   names(nlevels) <- names(di$factor.names)

#   if (length(unique(nlevels))==1) message(di$nfactors, " factors with ", unique(nlevels), " levels each")
#   else {message(di$nfactors, " factors") 
#         message("numbers of levels:")
#         print(nlevels)
#   }
   pfn <- di$factor.names
   lfn <- max(sapply(pfn, "length"))
   pfn <- lapply(pfn, function(obj) if (length(obj)==lfn) obj else c(obj,rep("",lfn-length(obj))))
   pfn <- as.data.frame(pfn)
   cat("Factor settings:\n")
   print(pfn,...)
   if (!is.null(response.names(object))){
       cat("\nResponses:\n")
       if (is.null(di$responselist)) print(response.names(object),...)
       else print(di$responselist)
   } 
   if (length(grep("param",design.info(object)$type))>0 & length(grep("wide",design.info(object)$type))>0 ){
      cat("\nOuter array:\n")
      print(design.info(object)$outer, ...)
      }
   ## alias information for FrF2 designs
   if (substr(di$type,1,4)=="FrF2"){
      if (any(sapply(di$aliased,"length")>1)){ 
         cat("\nAlias structure:\n")
         print(di$aliased,...)}
         else cat("no aliasing of main effects or 2fis\n")
      if (di$type=="FrF2.blocked"){
        if (length(di$aliased.with.blocks)>1){ 
           cat("Aliased with block main effects:\n")
           print(di$aliased.with.blocks,...)
         }
       }
   }
   ## what for pb and oa.design?
   if (substr(di$type,1,4)=="oa"){
       cat("Generating Orthogonal Array:\n")
         print(di$generating.oa,...)
       cat("Selected Columns:\n")
         print(di$selected.columns,...)
       }
   ## nothing for pb or full factorials
   ## quality criteria for D-optimal designs
   ## quality criteria for lhs designs
   ## what for rsm designs? 
   nWPs <- di$nWPs
   if (is.null(nWPs)) nWPs <- 1
   if (nWPs > 1){ 
          cat("split-plot design: ", nWPs, " whole plots\n")
          cat("                 : first ", di$nfac.WP, " factors are whole plot factors\n")
          }

}

