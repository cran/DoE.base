
lm <- function(formula, ...){
UseMethod("lm")
}

lm.default <- stats::lm

lm.design <- function (formula, ..., response = NULL, degree = NULL, FUN = mean, 
    use.center=FALSE){
    if (!"design" %in% class(formula)) 
        stop("lm.design works on class design objects only")
    di <- design.info(formula)
    fo <- formula(formula, ..., response = response, degree = degree, 
        FUN = deparse(substitute(FUN)), use.center=use.center)
    if (di$repeat.only | (length(grep("param", di$type)) > 0 & 
        length(grep("wide", di$type)) == 0) | (length(grep("center", di$type)) > 0 & !use.center)) 
        aus <- lm(fo, data = model.frame(fo, data = NULL), ...)
    else aus <- lm(fo, data = model.frame(fo, data = formula), ...)
    if (di$type %in% c("ccd", "bbd", "bbd.blocked", "lhs", "Dopt")) 
        lm(fo, data = model.frame(fo, data = formula), ...)
    class(aus) <- c("lm.design",class(aus))
    aus
}

summary.lm.design <- function(object, ...){
   aus <- summary.lm(object)
   class(aus) <- c("summary.lm.design", class(aus))
   aus
}

print.summary.lm.design <- function(x, ...){
    cat("Number of observations used:", sum(x$df[1:2]),"\n")
    cat("Formula:\n")
    fop <- formula(x)
    attributes(fop) <- NULL 
    print(fop)
    
        stats:::print.summary.lm(x, ...)
}

print.lm.design <-function(x, ...){
    cat("Number of observations used:", nrow(model.frame(x)),"\n")
    cat("Formula:\n")
    fop <- formula(x)
    attributes(fop) <- NULL 
    print(fop)
    stats:::print.lm(x, ...)
}

