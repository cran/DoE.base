\name{DoE.base-package}
\alias{DoE.base-package}
\alias{DoE.base}
\docType{package}
\title{
Full factorials, orthogonal arrays and base utilities for DoE packages
}
\description{
This package creates full factorial designs and designs from orthogonal arrays. 
In addition, it provides some base utilities like an exporting function for the DoE packages FrF2, 
DoE.wrapper and RcmdrPlugin.DoE (the latter two are still under development and not on CRAN).  

Parts of the package are currently in a somewhat experimental stage, in particular the 
orthogonal array functionality and the exporting facility. 
}
\details{
Parts of the package are currently in a somewhat experimental stage, in particular the 
orthogonal array functionality and the exporting facility. 
Please contact me, if you have suggestions.

This package designs Full Factorial experiments and main effects experiments based on orthogonal arrays. 
Some aspects of functions \code{fac.design} and \code{oa.design} have been modeled after the functions 
of the same name given in Chambers and Hastie (1993) (e.g. for 
the option \code{factor.names} or for outputting a data frame with attributes). 
However, S compatibility has not been considered in devising this package.

This package is used as a basis for packages FrF2, DoE.wrapper and 
RcmdrPlugin.DoE (the latter two are not yet published). 
Package \pkg{FrF2} relies on \code{\link{fac.design}} for full factorials in 2-level factors. 
This functionality has been sufficiently tested and is not considered experimental.
}
\author{
Ulrike Groemping

Maintainer: Ulrike Groemping <groemping@bht-berlin.de>
}
\references{
    Chambers, J.M. and Hastie, T.J. (1993). \emph{Statistical Models in S}, 
    Chapman and Hall, London.
     
    Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) 
    \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
    
    Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
}
\keyword{ array }
\keyword{ design }
\seealso{
\code{FrF2-package}, \code{\link{fac.design}}, \code{\link{oa.design}}, 
\code{\link{export.design}}
}
