\name{utilities}
\alias{make.formulas}
\alias{make.generators}
\alias{des.recode}
\alias{Letters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Utility functions for DoE packages, not intended for direct use }
\description{
  Utility functions for DoE packages, not intended for direct use
}
\usage{
make.generators(name, liste)
make.formulas(orignames, factor.names)
des.recode(var, recodes, as.factor.result, char) 
Letters
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{ ... }
  \item{liste}{ ... }
  \item{orignames}{ ... }
  \item{factor.names}{ ... }
  \item{var}{ variable }
  \item{recodes}{ character string specifying recodes, so far only used in the principal form \dQuote{-1=level[1];1=level[2]} }
  \item{as.factor.result}{ logical that specifies whether or not results are to be coerced to factor. 
            The default is that factors are coerced to factor, while numeric variables with numeric recodes are kept numeric.}
  \item{char}{ logical: should replacement be treated as character string (TRUE) or evaluated? default is FALSE}
}
\details{
   The functions are not intended for direct use. 
   
   The constant \code{Letters} is the vector of all capital and lower case 
   letters of the Roman alphabet except \code{I} and \code{i}. 
   It is used for default factor names and has been obtained as \cr
       \code{Letters <- c(LETTERS[-9],letters[-9])}.
   

}
\value{The functions are not intended for direct use.
}
\references{ 
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link[FrF2]{FrF2-package}}, \code{\link[DoE.wrapper]{DoE.wrapper-package}} }
\examples{
## default factor names for most design generating functions 
## (some quantitative designs have other default factor names)
## for up to 50 factors
Letters
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
\keyword{internal}