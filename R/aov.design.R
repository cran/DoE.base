aov <- function(formula, ...){
     UseMethod("aov")
}
aov.default <- stats::aov
aov.design <- function (formula, ..., response = NULL, degree = NULL, FUN = mean, 
    use.center = FALSE) 
{
    if (!"design" %in% class(formula)) 
        stop("aov.design works on class design objects only")
    di <- design.info(formula)
    fo <- formula(formula, ..., response = response, degree = degree, 
        FUN = deparse(substitute(FUN)), use.center = use.center)
    if (di$repeat.only | (length(grep("param", di$type)) > 0 & 
        length(grep("wide", di$type)) == 0) | (length(grep("center", 
        di$type)) > 0 & !use.center)) 
        aus <- aov(fo, data = model.frame(fo, data = NULL), ...)
    else aus <- aov(fo, data = model.frame(fo, data = formula), 
        ...)
    class(aus) <- c("aov.design", class(aus))
    aus
}

summary.aov.design <- function(object, ...){
   aus <- summary.aov(object)
   class(aus) <- c("summary.aov.design", class(aus))
   fop <- formula(terms(object))
   attributes(fop) <- NULL
   attr(aus, "formula") <- fop
   aus
}

print.summary.aov.design <- function(x, ...){
    cat("Number of observations used:", sum(x[[1]]$Df) + 1,"\n")
    cat("Formula:\n")
    print(attr(x, "formula"))
    #fop <- formula(x)
    #attributes(fop) <- NULL 
    #print(fop)
    
        stats:::print.summary.aov(x, ...)
}

print.aov.design <-function(x, ...){
    cat("Number of observations used:", nrow(model.frame(x)),"\n")
    cat("Formula:\n")
    fop <- formula(x)
    attributes(fop) <- NULL 
    print(fop)
    stats:::print.lm(x, ...)
}
