HouseholderRotToOne <- function(from){
   ### normalized vector from is rotated to (1, 0, ..., 0)
   ## used in function ICFT for both the concentrated case (rot)
   ## and the even case (t(rot) applied after rect_simplex)
   if (!is.vector(from)){
        if (is.matrix(from)){
           if (nrow(from)==1 || ncol(from)==1) from <- as.vector(from)
           else stop("rotToOne is applicable to vectors only")
       }
       else  stop("rotToOne is applicable to vectors only")
       }
   orig <- from
   lfrom <- c(sqrt(crossprod(from)))
   from <- from/lfrom
   r <- length(from)
   e1 <- c(1, rep(0, r-1))
   rot <- diag(rep(1, r))-2*tcrossprod(e1-from)/c(crossprod(e1-from))
   rot[r,] <- -rot[r,]
   return(list(rotation=rot, orig = orig, length=lfrom) )
}

rect_simplex <- function(r){
## the function returns an n-dimensional rectangular simplex
## with all lengths of vertices from the apex equal to one
## the origin is the apex
## the altitude is 1/sqrt(r) and lies on the first axis

## used for the even rotation, by postrotating the altitude to be collinear
## with the column means of U_sub

v0 <- c(-1/sqrt(r), rep(0, r-1))

hilf <- rep(0,r)
hilf[2] <- -sqrt((r-1)/r)
v1 <- hilf

if (r>2)
for (i in 2:(r-1)){
      hilf[i] <- 1/sqrt((r+2-i)*(r+1-i))
      hilf[i+1] <- -sqrt((r-i)/(r-i+1))
      assign(paste("v",i,sep=""), hilf)
}
hilf[r] <- abs(hilf[r])
assign(paste("v",r,sep=""), hilf)
Q <- matrix(NA, r,r)
for (i in 1:r)
  Q[,i] <- get(paste("v",i,sep=""))-v0
return(Q)
}
