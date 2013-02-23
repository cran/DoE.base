contr.XuWu <- function (n, contrasts = TRUE, sparse = FALSE) 
{
    ## function to calculate orthogonal normalized contrasts 
    ## that satisfy the XuWu normalization
    ##      Xu and Wu call these orthonormal
    ##      however, this is confusing
    if (length(n) <= 1L) {
        if (is.numeric(n) && length(n) == 1L && n > 1L) 
            levels <- seq_len(n)
        else stop("not enough degrees of freedom to define contrasts")
    }
    else levels <- n
    levels <- as.character(levels)
    if (contrasts) {
        n <- length(levels)
        cont <- array(-1, c(n, n - 1L), list(levels, NULL))
        for (j in 1:(n-1))  
        cont[1:j, j] <- - sqrt(n/(j*(j+1)))
        cont[col(cont) <= row(cont) - 2L] <- 0
        cont[col(cont) == row(cont) - 1L] <- sqrt(n*seq_len(n - 1L)/(1+seq_len(n - 1L)))
        colnames(cont) <- NULL
        
        if (sparse) 
            stats:::.asSparse(cont)
        else cont
    }
    else stats:::.Diag(levels, sparse = sparse)
}
