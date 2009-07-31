print.design <- function(x,show.order=design.info(x)$type %in% c("FrF2.blocked", "FrF2.splitplot", "crossed", "paramwide","FrF2.paramwide","param","FrF2.param") | design.info(x)$replications>1, ...){
   if (show.order)
   print(cbind(run.order(x)[,2:3],x), ...)
   else 
   print(cbind(x), ...)
   message("class=design, type=", design.info(x)$type)
   if (show.order) message("NOTE: columns run.no and run.no.std.rp are annotation, not part of the data frame")
   if (length(grep("param",design.info(x)$type))>0 & length(grep("wide",design.info(x)$type))>0 ){
      message("Outer array:")
      print(design.info(x)$outer, ...)
      }
}

summary.design <- function(object,...){
   di <- design.info(object)
   if (is.language(di$creator)){ 
       message("Call:")
       print(di$creator, ...)
       message("\n")
       }
   else if (length(class(di$creator))>1)
       message("design was generated with RcmdrPlugin.DoE\n")
   message("Experimental design of type ", di$type)
   message(di$nruns, " runs\n")
## ??? how to handle blocks from ccd ???
   blocks <- di$blocks
   if (is.null(blocks)) blocks <- 1
       if (blocks > 1){
          message("blocked design with ", di$blocks, " blocks")
          if (di$bbreps>1)
             message("each type of block independently conducted ", di$bbreps, " times")
          if (di$wbreps>1 & !di$repeat.only)
             message("each run within each block independently conducted ", di$wbreps, " times")
          if (di$wbreps>1 & di$repeat.only)
             message("each run measured ", di$wbreps, " times (no proper replication)")
       }
    else if (di$replications>1)
      if (di$repeat.only)
         message(di$replications, " measurements per run (not proper replications)")
      else
         message("each run independently conducted ", di$replications, " times")

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
   message("Factor settings:")
   print(pfn,...)
   if (!is.null(response.names(object))){
       message("\nResponses:")
       print(response.names(object),...)
   } 
   if (length(grep("param",design.info(object)$type))>0 & length(grep("wide",design.info(object)$type))>0 ){
      message("\nOuter array:")
      print(design.info(object)$outer, ...)
      }
   ## alias information for FrF2 designs
   if (substr(di$type,1,4)=="FrF2"){
      if (any(sapply(di$aliased,"length")>1)){ 
         message("\nAlias structure:")
         print(di$aliased,...)}
         else message("no aliasing of main effects or 2fis")
      if (di$type=="FrF2.blocked"){
        if (length(di$aliased.with.blocks)>1){ 
           message("Aliased with block main effects:")
           print(di$aliased.with.blocks,...)
         }
       }
   }
   ## what for pb and oa.design?
   if (substr(di$type,1,4)=="oa"){
       message("Generating Orthogonal Array:")
         print(di$generating.oa,...)
       message("Selected Columns:")
         print(di$selected.columns,...)
       }
   ## nothing for pb or full factorials
   ## quality criteria for D-optimal designs
   ## quality criteria for lhs designs
   ## what for rsm designs? 
   nWPs <- di$nWPs
   if (is.null(nWPs)) nWPs <- 1
   if (nWPs > 1){ 
          message("split-plot design: ", nWPs, " whole plots")
          message("                 : first ", di$nfac.WP, " factors are whole plot factors")
          }

}

