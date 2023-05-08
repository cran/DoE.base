\name{expansive.replace}
\alias{expansive.replace}
\title{ Expansive replacement for two orthogonal arrays }
\description{
  Expansive replacement for two orthogonal arrays 
}
\usage{
   expansive.replace(array1, array2, fac1 = NULL, all = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{array1}{ an orthogonal array, must be a matrix; \cr
      the levels of column \code{fac1} of this array are replaced by the 
      corresponding runs of \code{array2}; 
      they must be numbered with integers starting with 1}
  \item{array2}{ an orthogonal array, must be a matrix \cr
      this array is replaced for a column in \code{array1}  }
  \item{fac1}{if \code{NULL}, the first suitable column of \code{array1} 
      is replaced with \code{array2};\cr
      alternatively, a suitable column number for \code{array1} can be specified}
  \item{all}{logical; \code{TRUE} is permitted, if \code{array2} is a full factorial in two factors; 
      if \code{TRUE}, a list of permutations of the replacement array 
      \code{array2} is used for creating all potentially combinatorially 
      different outcomes, instead of a single expansive replacement based on the 
      order of \code{array2} as stated (see Details section)}
}
\details{
  This function mainly is meant for combining two orthogonal arrays
  via what Kuhfeld (2009) calls expansive replacement. 
  
  If \code{array2} is a full factorial in two factors, argument \code{all = TRUE} 
  creates a list of expanded arrays obtained by permuting the second array in all ways 
  that may lead to combinatorially different end results. With \eqn{s_1}{s1} and \eqn{s_2}{s2} the numbers 
  of levels of the factors in \code{array2}, this is the number of partitions of the 
  runs of \code{array2} into \eqn{s_1}{s1} equally-sized groups, multiplied with \eqn{s_2!^{s_1-1}}{s2!^(s1-1)} for 
  the possibilities of permuting the levels of the second factor within all but the first 
  level of the first factor. This functionality is primarily meant for the creation of 
  strength 3 arrays in combination with arrays listed in the catalogue \code{\link{oacat3}} 
  (see an example on expanding the 6-level factor in \code{L96.2.5.4.2.6.1}).
  }
\value{The function returns an object of classes \code{oa} and \code{matrix}, 
which can be used in function \code{\link{oa.design}}, or a list of such arrays, 
in case \code{all = TRUE}. 

Whether or not the object is an orthogonal array depends on the choice of suitable input 
arrays by the user. The properties of the resulting array(s) can e.g. be inspected with functions 
\code{\link{GWLP}} or \code{\link{GRind}}.
}
\author{ Ulrike Groemping }
\note{ This package is still under development. Bug reports and feature requests are welcome.}
\seealso{ See Also \code{\link{oacat}}, \code{\link{oacat3}} }
\references{
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute 
  \url{https://support.sas.com/techsup/technote/ts723b.pdf} and references therein.
}
\examples{
myL24.2.14.6.1 <- expansive.replace(L24.2.12.12.1, L12.2.2.6.1)

L96.2.6.3.1.4.2_list <- expansive.replace(L96.2.5.4.2.6.1, 
      cbind(U=rep(c(1,2),each=3), V=rep(1:3,2)), all=TRUE)
## the list of 60 resolution IV arrays can be used for design creation, 
## e.g. as follows:
\dontrun{
     ## resolution IV designs obtained from the 60 different arrays
     deslist <- lapply(L96.2.6.3.1.4.2_list, 
         function(aa) oa.design(aa, nlevels=c(2,2,2,2,3,4,4), columns="min34"))
     table(A4s <- sapply(deslist, length4))  ## a single best design exists
     best <- deslist[[which(A4s < 2)]]
     GWLP(best)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
