rerandomize.design <- function(design, seed=NULL, ...){
    ## function to re-randomize a design object
    ## particularly interesting for replicated designs,
    ##    if users don't want to have the randomization in blocks
    ## or for blocked designs, if users want the blocks in randomized order
    if (!"design" %in% class(design))
        stop("the function works on class design objects only")
    di <- design.info(design)
    if (!is.null(di$response))
        stop("the design has responses already and must not be re-randomized.")
    if (!is.null(seed)){
       if (!is.numeric(seed)) stop("seed must be a number")
       if (!length(seed)==1) stop("seed must be a single number")
       }

    ro <- run.order(design)
    desnum <- desnum(design)
    design <- undesign(design)

    aw <- FALSE
    awro <- FALSE
    bl <- FALSE
    sp <- FALSE
    if (di$replications>1 & !di$repeat.only) aw <- TRUE
    if (di$replications>1 & di$repeat.only) awro <- TRUE
    if (length(grep("blocked", di$type, fixed=TRUE))>0) bl <- TRUE
    if (length(grep("splitplot", di$type, fixed=TRUE))>0) sp <- TRUE

    if (!is.null(seed)) set.seed(seed)
    if (!(bl | sp)){
      rp <- di$replications
      if (awro){
        nr <- nrow(design)%/%rp
        neworder <- sample(nr)
        neworder <- rep((neworder-1)*rp, each=rp) + rep(1:rp, nr)
        }
      else{
        nr <- nrow(design)
        neworder <- sample(nr)
        }
      }
    else {
      if (sp) {
        ps <- di$plotsize
        rp <- di$replications
        if (!awro){
          # plot <- as.numeric(getblock(design, combine=TRUE))
          nr <- nrow(design)%/%ps
          neworder <- sample(nr)
          neworder <- rep((neworder-1)*ps, each=ps) + unlist(lapply(1:nr, function(obj) sample(ps)))
        }
        else{
          # plot <- as.numeric(getblock(design)$plots)
          nr <- nrow(design)%/%(ps*di$replications)
          neworder <- sample(nr)
          neworder <- rep((neworder-1)*ps, each=ps*rp) +
               rep(unlist(lapply(1:nr, function(obj) sample(ps))), each=rp) +
               rep(1:rp, nr*ps)
        }
      }  ## end of split plot
    if (bl) {
        bs <- di$blocksize
        nb <- di$bbreps
        nw <- di$wbreps
        if (!awro) {
          nr <- nrow(design)%/%(bs*nw)
          neworder <- sample(nr)
          neworder <- rep((neworder-1)*bs*nw, each=bs*nw) + unlist(lapply(1:nr, function(obj) sample(bs*nw)))
        }
        else{
          nr <- nrow(design)%/%(bs*nw)
          neworder <- sample(nr)
          neworder <- rep((neworder-1)*bs, each=bs*nw) +
               rep(unlist(lapply(1:nr, function(obj) sample(bs))), each=nw) +
               rep(1:nw, nr*bs)
        }
    }  ## end of block
    }  ## end of block or splitplot
   design <- design[neworder,]
   ro <- ro[neworder,]
   desnum <- desnum[neworder,]
   attr(design, "desnum") <- desnum
   attr(design, "run.order") <- ro
   attr(design, "design.info") <- di
   class(design) <- c("design", "data.frame")
   design
}