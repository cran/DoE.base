lowerbounds <- function(nruns, nlevels, R){
  ## the function returns a vector of lower bounds
  ## for nruns^2*A_R of the choose(nfac, R) R-factor sets
  ## the entries are integer-valued
  ## if they are all zero,
  ##   the combination of nruns and nlevels
  ##   permits a design of resolution at least R+1
  nfac <- length(nlevels)
  sets <- nchoosek(nfac, R)
  sapply(1:ncol(sets), function(ii){
    pr <- prod(nlevels[sets[,ii]])
    r <- nruns%%pr
    r*(pr-r)})
}

lowerbound_chi2 <- function(nruns, nlevels){
  ## the function returns a lower bound on the chi^2 value
  ## for supersaturated designs
  ## provided by Liu and Lin 2009
  ## and can be used for obtaining a sharper bound on A_2
  ## for resolution II designs

  ## note that the bound from this function can be
  ## sharpened by incorporating the information that
  ## n^2*A_2 must be integral

  ## hilf is the published bound
  ## the function returns the sharpened bound
  qs <- sort(unique(nlevels))
  rs <- table(nlevels)
  m <- length(nlevels)
  n <- nruns
  hilf <- (n*sum(qs*rs)^2-(n*(n-1)+2*m*n)*sum(rs*qs)+m*n*(m+n-1))/(2*(n-1))
  max(0, ceiling(hilf*n)/n)
}

lowerbound_AR <- function(nruns, nlevels, R, crit="total"){
  if (!crit %in% c("total", "worst")) stop("invalid crit")
  if (crit=="total"){
    hilf <- sum(lowerbounds(nruns, nlevels, R))/nruns^2
    if (R > 2) return(hilf)
    else return(max(hilf, lowerbound_chi2(nruns, nlevels)/nruns))
  }
  ## else the lower bound for the worst AR is returned
  max(lowerbounds(nruns, nlevels, R))/nruns^2
}

#nruns <- 54
#nlevels <- c(2,3,3,3,3,3)
#oa_feasible(nruns, nlevels, 3)
#lowerbound_AR(nruns,nlevels,4, crit="worst")
