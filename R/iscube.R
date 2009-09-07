iscube <- function (design, ...)
{
    if (!"design" %in% class(design))
        stop("this function is applicable for class design objects only")
    di <- design.info(design)
    if (!length(grep("center", di$type)) > 0)
        stop("this function requires a design with center points")
    ## determine center point positions
    aus <- run.order(design)$run.no.in.std.order == "0"
    if (!sum(aus) == di$ncenter * di$replications) {
        wrong <- TRUE
        if (!is.null(di$blocksize))
            if (sum(aus) == di$ncenter * di$bbreps * di$wbreps *
                di$nblocks)
                wrong <- FALSE
        if (wrong)
            stop("There is something wrong with the number of center points for this design.")
    }
    ## output cube point positions
    !aus
}
