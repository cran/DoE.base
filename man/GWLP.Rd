\name{GWLP}
\alias{GWLP}
\alias{GWLP.design}
\alias{GWLP.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for fast calculation of GWLP }
\description{
  Calculates GWLP using the formulae from Xu and Wu (2001)
}
\usage{
GWLP(design, ...)
\method{GWLP}{design}(design, kmax=design.info(design)$nfactors, 
     attrib.out=FALSE, with.blocks = FALSE, digits = NULL, ...)
\method{GWLP}{default}(design, kmax=ncol(design), attrib.out=FALSE, digits = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{ a design, not necessarily of class \code{\link{design}};
      class design properties are exploited by using only factor columns 
      (or factor and block columns, if \code{with.blocks} is \code{TRUE}) }
  \item{kmax}{ the maximum word length requested }
  \item{attrib.out}{ the detail added to the output (see Value section)}
  \item{with.blocks}{ if \code{TRUE}, the block column contributes to 
       the GWLP, otherwise it does not }
  \item{digits}{ the number of decimals to round to; \code{NULL} prevents rounding }
  \item{...}{ further arguments to generic \code{GWLP}; not used in the methods}
}
\details{
   Function \code{GWLP} is much faster but also more inaccurate than the 
   function \code{\link{lengths}}, which calculates numbers of words 
   for lengths 2 to 5 only. Note, however, that function \code{\link{lengths}} 
   can be faster for designs with very many rows. \cr
   If a design factor contains only some of the intended levels, 
   \code{design} must be a data frame, and the factor must be an R
   factor with the complete set of levels specified, 
   in order to make function \code{GWLP} aware of the missing levels.
}
\value{
The \code{GWLP} methods output a named vector with the numbers of generalized 
words of lengths zero to \code{kmax}. If \code{attrib.out} is \code{TRUE}, 
this vector comes with the attributes \code{B} and \code{levels.info}, 
the latter documenting the level situation of the design, the former 
the distance distribution B (Xu and Wu 2001).
}
\references{ 
  Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for 
  asymmetrical fractional factorial designs. 
  \emph{Annals of Statistics} \bold{29}, 1066--1077.
}
\author{ Hongquan Xu, Ulrike Groemping }
\seealso{ See Also \code{\link{lengths}} }
\examples{
GWLP(L18)
GWLP(L18, attrib.out=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line