
lm <- function(formula, ...) ergeb <- UseMethod("lm")

lm.default <- function(formula, ...) {
   ## most lines of code are for producing a call element of the result 
   ## that does also work if DoE.base is not available (i.e. does not require
   ##       function lm.default to be defined)
   ruf <- match.call()
   ergeb <- stats::lm(formula, ...)
   ruf[[1]] <- as.symbol(quote(lm))
   ergeb$call <- as.call(ruf)
   ergeb
   }

lm.design <- function (formula, ..., response = NULL, degree = NULL, FUN = mean, 
    use.center=FALSE){
   ## name of data set is stored i order to make sure that the lm object knows 
   ##     how it has been created (data element of call) 
   ##     even if package DoE.base is not loaded
   ## for the same reason, the formula is stored within the call element
    daten <- deparse(substitute(formula))
    if (!"design" %in% class(formula)) 
        stop("lm.design works on class design objects only")
    di <- design.info(formula)
    fo <- formula(formula, ..., response = response, degree = degree, 
        FUN = deparse(substitute(FUN)), use.center=use.center)
    if (di$repeat.only | (length(grep("param", di$type)) > 0 & 
        length(grep("wide", di$type)) == 0) | (length(grep("center", di$type)) > 0 & !use.center)) 
        aus <- lm(fo, data = model.frame(fo, data = NULL), ...)
    else aus <- lm(fo, data = model.frame(fo, data = formula), ...)
    if (di$type %in% c("ccd", "bbd", "bbd.blocked", "lhs")) 
        aus <- lm(fo, data = model.frame(fo, data = formula), ...)
    class(aus) <- c("lm.design",class(aus))
    ruf <- as.list(aus$call)
    ruf$data <- as.symbol(daten)
    ruf$formula <- fo
    aus$call <- as.call(ruf)
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

