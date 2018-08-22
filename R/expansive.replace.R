expansive.replace <- function(array1, array2, fac1 = NULL, all = FALSE){
     if (!is.matrix(array1)) stop("array1 must be a matrix")
     if (!is.matrix(array2)) stop("array2 must be a matrix")
     if (!is.null(fac1)) 
         if (!fac1 %in% 1:ncol(array1)) 
              stop("fac1 must be a column number for array1")
     l1 <- levels.no(array1)
     l2 <- levels.no(array2)
     n1 <- nrow(array1)
     n2 <- nrow(array2)
     if (!is.null(fac1)) 
         if (!l1[fac1]==n2) 
              stop("column ", fac1, " of array1 does not have ", n2, " levels") 
     if (!n2 %in% l1) 
         stop("array1 must have at least one factor with as many levels as array2 has rows")
     if (is.null(fac1)) fac1 <- min(which(l1==n2))
     if (length(setdiff(array1[,fac1], 1:n2))>0) 
         stop("the levels of column fac1 must be coded as consecutive integers starting with 1")
     if (!is.logical(all)) stop("all must be logical")
     if (all){
       ## create all potentially different versions of 2-factor full factorial replacments
       ## by partitioning according to the levels of the first factor
       ## and creating all permutation combinations for levels of the second factor 
       ##     for all but the first level of the first factor (which is fixed at permutation 1:l2_2)
       if (ncol(array2)>2) stop("all=TRUE is only implemented for array2 with two columns")
       if (!prod(l2)==n2) stop("all=TRUE is implemented for full factorial array2 only")
       l2_2 <- l2[2]
       hilf <- partitions::setparts(rep(n2%/%l2[1], l2[1])) 
            ## n2 row matrix with nsets columns containing the distinct partitions
       nsets <- ncol(hilf)
       perms <- combinat::permn(l2_2)
            ## list of length nperm = factorial(l2_2) of vectors of length l2_2
       nperm <- length(perms)
       ## obtain the levels of the second factor for levels 1:l2[1] of the first
          ## each row of arr holds a permutation combination
          arr <- expand.grid(rep(list(1:nperm), l2[1]-1))
          ## n2 x nperm^(l2[1]-1) matrix of vectors to combine with the columns of hilf
          ## after reordering according to the levels of the first factor
          p2mat <- matrix(NA, n2, nperm^(l2[1]-1))
          for (i in 1:nrow(arr)){
             p2 <- rep(NA, n2)
             p2[1:l2_2] <- 1:l2_2
             for (l in 2:l2[1])
               p2[((l-1)*l2_2+1):(l*l2_2)] <- perms[[arr[i,l-1]]]
             p2mat[,i] <- p2
          }
       ## prepare list of replacement matrices
       repla <- vector(mode="list", length=nsets*nperm^(l2[1]-1))
       for (s in 1:nsets)
          for (i in 1:ncol(p2mat)){
             cur <- hilf[,s]  ## level arrangement of factor 1
             ## obtain list of indices for current partition elements
             parts <- lapply(1:l2[1], function(obj) which(cur==obj))
             ## create current replacement matrix
             ## by adding values for the second column 
             ## in appropriate permutation
             cur <- cbind(cur, NA)
             for (j in 1:l2[1]) 
                 cur[parts[[j]],2] <- p2mat[(j-1)*l2_2+(1:l2_2),i]
             colnames(cur) <- colnames(array2)
             repla[[(s-1)*ncol(p2mat)+i]] <- cur 
             }
       ## conduct expansive replacement 
       ## with list of non-isomorphic replacement matrices
       aus <- lapply(repla, function(obj){
          hilf <- cbind(array1[, -fac1], obj[array1[,fac1],])
          class(hilf) <- c("oa", "matrix")
          hilf
       })
     }
     else {
       ## a single replacement only
       ## ??? check whether order matters
       aus <- cbind(array1[, -fac1], array2[array1[,fac1],])
       class(aus) <- c("oa", "matrix")
     }
     aus
}