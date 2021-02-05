\name{DoE.base-package}
\alias{DoE.base-package}
\alias{DoE.base}
\docType{package}
\title{
Full factorials, orthogonal arrays and base utilities for DoE packages
}
\description{
This package creates full factorial designs and designs from orthogonal arrays. 
In addition, it provides some basic utilities like an exporting function for the DoE packages FrF2, 
DoE.wrapper and RcmdrPlugin.DoE, and some diagnostics for general orthogonal arrays 
(generalized word length calculations). 
}
\details{
The package is still in under development phase. 
Details about combining designs are particularly likely to be changed in the future 
(\code{param.design}, \code{cross.design}). Please contact me, if you have suggestions.

This package designs full factorial experiments (function \code{fac.design}) 
and experiments based on orthogonal arrays (\code{oa.design}). 
Some aspects of functions \code{fac.design} and \code{oa.design} have been 
modeled after the functions of the same name given in Chambers and Hastie (1993) 
(e.g. for the option \code{factor.names} or for outputting a data frame 
with attributes). However, S compatibility has not been considered in 
devising this package.

The orthogonal arrays underlying function \code{\link{oa.design}} are mainly taken from 
Kuhfeld (2009). While the 
arrays generally guarantee estimability of main effects in case there are no 
(or negligible) active interactions, some of them can also be used for 
designs for which some interactions are to be estimated, if only few of the design 
columns are used for experimentation. Optimization for such purposes 
and check of fitness for such purposes is supported, 
cf. \code{\link{generalized.word.length}}.

The package provides class \code{\link{design}} for use also by packages 
\pkg{FrF2}, \pkg{DoE.wrapper} and \pkg{RcmdrPlugin.DoE}. Furthermore, it provides 
utilities for printing, plotting, summarizing, exporting and combining 
experimental designs. 
Package \pkg{FrF2} relies on function \code{\link{fac.design}} 
for full factorials in 2-level factors. 
}
\section{Acknowledgments}{Thanks are due to Peter Theodor Wilrich for 
various useful suggestions in the early phase of this project!
}
\author{
Ulrike Groemping

Maintainer: Ulrike Groemping <groemping@bht-berlin.de>
}
\references{
    Chambers, J.M. and Hastie, T.J. (1993). \emph{Statistical Models in S}, 
    Chapman and Hall, London.
    
    Groemping, U. (2018).  R Package DoE.base for Factorial Designs. 
      \emph{Journal of Statistical Software} \bold{85}(5), 1--41.
      \url{https://www.jstatsoft.org/issue/view/v085}.
     
    Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) 
    \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
    
    Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute 
    \url{https://support.sas.com/techsup/technote/ts723b.pdf} and references therein.
}
\keyword{ array }
\keyword{ design }
\seealso{
Functions \code{\link{fac.design}}, \code{\link{oa.design}} for generating designs, 
and various functions (\code{\link{generalized.word.length}}) for optimizing and checking 
a designs properties,\cr
class \code{\link{design}} which is utilized also by packages 
\pkg{\link[FrF2:FrF2-package]{FrF2}} and \pkg{\link[DoE.wrapper:DoE.wrapper-package]{DoE.wrapper}}.\cr 
Furthermore, there are various utility functions like \code{\link{export.design}} or 
\code{\link{add.response}} and functions \code{\link{cross.design}} or \code{\link{param.design}} 
for combining designs.\cr
Finally, several \code{\link[=class-design-methods]{methods for class design objects}} 
are provided, 
especially also functions \code{\link{formula.design}} and \code{\link{lm.design}} 
for automatic generation of linear models (but beware: these are convenience functions 
that provide a quick first look but NOT necessarily the best statistical approach to analysis!).
}
