print.oa <- function(x, ...){
  xnam <- deparse(substitute(x))
  if (!"oa" %in% class(x)) stop("this print method is for class oa only")
  attrs <- setdiff(names(attributes(x)), c("origin", "class", "dim", "dimnames"))
  info <- attr(x, "MIPinfo")$info
  attr(x, "MIPinfo") <- NULL
  attr(x, "history") <- NULL
  attr(x, "matrices") <- NULL
  print.default(x, ...)
  if (!is.null(info$stati)){
      cat("optimization results:\n")
      print(unlist(info$stati))
  }
  if (length(attrs) > 0){
    cat("\nfurther attribute(s)", "(accessible with attr(", xnam, ", attrname)):", fill=TRUE)
    print(attrs)
  }
}
