\name{utilities}
\alias{make.formulas}
\alias{make.generators}
\alias{des.recode}
\alias{Letters}
\alias{printBy}
\alias{gen.fun}
\alias{generators}
\alias{generators.default}
\alias{generators.design}
\alias{generators.catlg}
\alias{PFTs.from.variants}
\alias{matrix.fromPFTs}
\alias{rankPFT}
\alias{bestPFT}
\alias{rect_simplex}
\alias{HouseholderRotToOne}
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
gen.fun(obj, num = FALSE)
generators(design, ...)
\method{generators}{design}(design, ...)
\method{generators}{catlg}(design, ...)
\method{generators}{character}(design, select.catlg = catlg, ...)
PFTs.from.variants(array, variants, R=3, rela=TRUE)
matrix.fromPFTs(PFTs)
rankPFT(pfts)
bestPFT(pfts)
rect_simplex(r)
HouseholderRotToOne(from)
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
  \item{obj}{ an entry of a class \code{catlg} object}
  \item{num}{ logical; if \code{TRUE}, Yates matrix column numbers are output, otherwise human-friendly generators}
  \item{design}{FrF2 design given as catalogue entry, design itself or character string; 
        function generators provides the generating contrasts for additional factors for these designs.
       }
  \item{select.catlg}{the catalogue in which to look for the design (should be of class \code{catlg})
       }
  \item{\dots}{ further arguments to function \code{print} or \code{generators} }
  \item{array}{ orthogonal array (available in the workspace); must be matrix or data frame }
  \item{variants}{ matrix the rows of which contain column numbers for \code{array} }
  \item{R}{ resolution of \code{array} and also \code{array[variants,,drop=FALSE]} }
  \item{rela}{ logical; relative PFTs (\code{TRUE}) or absolute PFTs (\code{FALSE}) }
  \item{PFTs}{ list of (absolute or relative) projection frequency tables.\cr 
        The list could have been produced with function \code{PFTs.from.variants}. }
  \item{pfts}{ a matrix the columns of which contain the PFT entries for several designs, 
        with row names indicating the (relative) number of words of a triple/quadruple.\cr
        The matrix could have been obtained from function \code{matrix.fromPFTs}. }
  \item{r}{integer; dimension of the simplex}
  \item{from}{numeric vector to be rotated towards (1,0,...,0)}
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
   
   This paragraph describes internal functions: Function \code{PFTs.from.variants} 
   can be used to determine a list of (relative) 
   projection frequency tables (cf. Groemping 2011) for comparing several column 
   selections from the same array. Function \code{matrix.fromPFTs} brings 
   this list into matrix form, functions \code{rankPFT} and \code{bestPFT} 
   allow ranking or picking best selections.
}
\value{Most functions are not intended for direct use.

The \code{generators} methods output a list of human-readable character string 
vectors with the generators of catalogue entries or a design. 

The following functions have not yet been adapted to also handle the more 
attractive new metrics calculated with functions \code{P3.3}, \code{P4.4} and 
\code{GRind}.
Function \code{PFTs.from.variants} outputs a list of (relative) projection 
frequency tables (each entry is produced with function \code{P3.3} or \code{P4.4}). 
Function \code{matrix.fromPFTs} outputs a matrix (reshaped from such a list, filling 
missing entries with 0 counts for some designs, if required), with columns for 
the different selections and rows for different (relative) numbers of generalized words. 
Function \code{rankPFTs} outputs a ranking vector, with 1 for the best design 
(ties are kept in the original order). Function \code{bestPFTs} outputs a logical vector, 
with entry \code{TRUE} for all best designs (identical best (R)PFTs).

}
\references{ 
   Groemping, U. (2011). Relative projection frequency tables for orthogonal arrays. 
      Report 1/2011, \emph{Reports in Mathematics, Physics and Chemistry} \url{http://www1.beuth-hochschule.de/FB_II/reports/welcome.htm}, 
      Department II, Beuth University of Applied Sciences, Berlin.
      
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
## column selections from L18 with one 2-level and six 3-level factors
v <- rbind(1:7, c(1:6,8), c(1:5,7:8), c(1:4,6:8), c(1:3,5:8), c(1:2,4:8), c(1,3:8))
## RPFTs
RPFTs <- DoE.base:::PFTs.from.variants(L18, v, rela=TRUE)
rpfts <- DoE.base:::matrix.fromPFTs(RPFTs)
rpfts
DoE.base:::rankPFT(rpfts)
DoE.base:::bestPFT(rpfts)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
\keyword{internal}