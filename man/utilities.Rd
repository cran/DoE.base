\name{utilities}
\alias{make.formulas}
\alias{make.generators}
\alias{des.recode}
\alias{Letters}
\alias{printBy}
\alias{generators}
\alias{generators.default}
\alias{generators.design}
\alias{generators.catlg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Utility functions for DoE packages, not intended for direct use }
\description{
  Utility functions for DoE packages, not intended for direct use
}
\usage{
make.generators(name, liste)
make.formulas(orignames, factor.names)
des.recode(var, recodes, as.factor.result, char) 
printBy(data, byvars, ...)
Letters
generators(design, ...)
\method{generators}{design}(design, ...)
\method{generators}{catlg}(design, ...)
\method{generators}{character}(design, select.catlg = catlg, ...)
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
  \item{data}{ data frame to be printed in groups that are defined by the \code{byvars}}
  \item{byvars}{ character vector of variable names; 
        variables named here determine the groups; only adjacent observations are considered part of a group, i.e. 
        if data are not ordered by the \code{byvars}, several separate occurrences of the same group may occur}
  \item{design}{FrF2 design given as catalogue entry, design itself or character string; 
        function generators provides the generating contrasts for additional factors for these designs.
       }
  \item{select.catlg}{the catalogue in which to look for the design (should be of class \code{catlg})
       }
  \item{\dots}{ further arguments to function \code{print} or \code{generators} }
}
\details{
   Most functions are not intended for direct use. 
   
   The constant \code{Letters} is the vector of all capital and lower case 
   letters of the Roman alphabet except \code{I} and \code{i}. 
   It is used for default factor names and has been obtained as \cr
       \code{Letters <- c(LETTERS[-9],letters[-9])}.
       
   The generic function \code{generators} and its methods determine generating contrasts 
   for regular fractional factorial 2-level designs, which can be specified as \cr
   the design itself (created with function \code{\link[FrF2]{FrF2}}),\cr 
   a list of catalogue entries in package \pkg{FrF2} object \code{\link[FrF2]{catlg}},\cr 
   or a character string that names such a catalogue entry. \cr
   The method for the design itself cannot be used on designs of type 
   \code{FrF2.param...} or \code{FrF2.folded}. Also, it cannot
   be used on designs produced with versions of \pkg{FrF2} before \code{1.1} in case 
   of blocked or splitplot designs. It is generally recommended to update all packages of the 
   suite (\pkg{DoE.base}, \pkg{FrF2}, \pkg{DoE.wrapper}, \pkg{RcmdrPlugin.DoE}), whenever 
   one of them is updated.
   

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
\dontrun{
generators("7-2.2")
generators(catlg[2:8])
generators(FrF2(16,7))
generators(FrF2(16,5,blocks=4,alias.block.2fi=TRUE))
generators(FrF2(16,5,WPs=4,nfac.WP=2))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
\keyword{internal}