corrPlot <- function(design, scale="corr", recode=TRUE, cor.out=TRUE, mm.out=FALSE, 
     main.only = TRUE, three = FALSE, run.order=FALSE, 
     frml=as.formula(ifelse(three, ifelse(run.order, "~ run.no + .^3", "~ .^3"), 
                                   ifelse(run.order, "~ run.no + .^2", "~ .^2"))),
     pal=NULL, col.grid="black", col.small="grey", lwd.grid=1.5, lwd.small=0.5, 
     lty.grid=1, lty.small=3, cex.y=1, cex.x=0.7, x.grid=NULL,
     main=ifelse(scale=="corr","Plot of absolute correlations",ifelse(scale=="R2", "Plot of squared correlations", 
        "Plot of absolute correlations of coefficient estimates")), 
     split=0, ask=(split>0), ...){
if (!"design" %in% class(design)) design <- data2design(design)
if (!scale %in% c("corr", "R2", "corr.est")) stop("invalic choice for scale")
hilf <- GWLP(design, k=5)
if (!round(hilf[2],5)==0) stop("corrPlot is applicable for balanced designs only")
if (!round(hilf[3],5)==0) res=2
else {
    if (!round(hilf[4],5)==0) res=3
        else {
             if (!round(hilf[5],5)==0) res=4
             else { 
              if (!round(hilf[6],5)==0) res=5
              else stop("The design has resolution larger than V. corrPlot is not applicable.")
           }}}

if (!is.logical(recode)) stop("recode must be logical")
if (!is.logical(main.only)) stop("main.only must be logical")
if (!is.logical(three)) stop("three must be logical")
if (!is.null(x.grid) & !is.numeric(x.grid)) stop("x.grid must be a numeric vector of positions for vertical lines")

if (recode) design <- change.contr(design, contrasts="contr.XuWu")
if (res > 2 & !run.order) suppress <- TRUE else suppress <- FALSE  ## suppress main effects on horizontal axis
if (run.order) run.no <- scale(1:nrow(design))*sqrt(nrow(design))
    

design <- design[, names(factor.names(design))] ## only done here in order to have intact design structure before
nfac <- ncol(design)

mm <- model.matrix(frml, design)

effid <- attr(mm, "assign")[-1]
cpr <- abs(cor(mm[,-1]))
if (scale=="corr.est"){
   if (!ncol(mm)==qr(mm)$r) stop("scale=corr.est not possible because of rank deficiency")
   cpr <- abs(cov2cor(solve(crossprod(mm))))[-1,-1]
}
if (mm.out || cor.out) aus <- cpr
   if (mm.out) attr(aus, "mm") <- mm
if (scale=="R2") cpr <- cpr^2
diag(cpr) <- NA

nc <- ncol(cpr)

mecols <- which(effid %in% 1:nfac)
if (three & !main.only){
   int2cols <- which(effid %in% (nfac+1):((nfac*(nfac+1))/2))
   intcols <- which(effid %in% (nfac+1):(nfac*(nfac+1)/2 + nfac*(nfac-1)*(nfac-2)/6 )) 
}
else
intcols <- setdiff(1:nc, mecols)   ## only 2-factor interactions present

if (!three) int2cols <- intcols

if (suppress & main.only) cpr <- cpr[rev(mecols), intcols]
if (suppress & !main.only) cpr <- cpr[rev(c(mecols, int2cols)), intcols]
if (!suppress & main.only) cpr <- cpr[rev(mecols), c(mecols, intcols)]
if (!suppress & !main.only) cpr <- cpr[rev(c(mecols, int2cols)), c(mecols, intcols)]

if (main.only) hsmall <- 0.5 + 0:length(mecols)
else hsmall <- 0.5 + 0:(length(mecols) + length(int2cols))
if (suppress) vsmall <- 0.5 + 0:length(intcols)
else vsmall <- 0.5 + 0:nc

if (main.only) 
   hbig <- 0.5 + c(0, which(!effid[rev(mecols[-1])]==effid[rev(mecols)[-1]]))
else 
   hbig <- 0.5 + c(0, which(!effid[rev(c(mecols,int2cols)[-1])]==effid[rev(c(mecols,int2cols))[-1]]))

#if (suppress)
#   vbig <-

if (is.null(pal)){
if (requireNamespace("RColorBrewer"))
 pal <- RColorBrewer::brewer.pal(9,"Blues")
else pal <- c("white",rev(heat.colors(9)))
}

ns <- 1
from <- 1
to <- ncol(cpr)
if (split>0){
   if (ncol(cpr)/split > 8) stop("too many plots")
   ns <- ncol(cpr)%/%split + 1
   from <- to <- rep(0,ns)
   for (i in 1:ns){
       from[i] <- 1+(i-1)*split
       to[i] <- i*split
   }
   to[ns] <- ncol(cpr)     
}

if (!is.null(x.grid)) vbig <- x.grid
else vbig <- 0

op <- par(ask=ask)
for (i in 1:ns){
print(levelplot(t(round(cpr[,from[i]:to[i]],4)), col.regions=pal, cuts=length(pal)-1, aspect="fill",
   scales=list(x=list(rot=90, tck=0, cex=cex.x), y=list(tck=0, cex=cex.y)),
   panel = function(...){
            panel.levelplot(...)
            panel.abline(h = hsmall,col=col.small, lwd=lwd.small, lty=lty.small)
            panel.abline(v = vsmall, col=col.small, lwd=lwd.small, lty=lty.small)
            panel.abline(h = hbig,col=col.grid,lwd=lwd.grid,lty=lty.grid)
            panel.abline(v = vbig,col=col.grid,lwd=lwd.grid,lty=lty.grid)
        },
   main=main, 
   xlab="",ylab=""))
}
par(op)

if (mm.out || cor.out) invisible(aus)
}
