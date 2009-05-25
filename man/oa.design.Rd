\name{oa.design}
\alias{oa.design}
\alias{origin}
\alias{comment}
\alias{oacat}
\alias{oa}
\alias{Letters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for accessing orthogonal arrays }
\description{
  Function for accessing orthogonal arrays
}
\usage{
oa.design(ID=NULL, nruns=NULL, nfactors=NULL, nlevels=NULL, 
      factor.names = if (!is.null(nfactors)) {
        if (nfactors <= 50) Letters[1:nfactors]
           else paste("F", 1:nfactors, sep = "")} 
        else NULL, columns=NULL, 
        replications=1, repeat.only=FALSE,
        randomize=TRUE, seed=NULL, min.residual.df=0)
origin(ID)
comment(ID)
oacat
oa
Letters
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{ name of the orthogonal array design to be used; 
        available names can be looked at via \code{oacat$name};
        users can also specify names of their own designs here (cf. details). 
        \code{ID} must be of class \code{oa}.
        If omitted, ID is automatically determined based on \code{nlevels} 
        or \code{factor.names}. }
  \item{nruns}{ number of runs, can be omitted if obvious from \code{ID} or if 
        the smallest possible array is to be found }
  \item{nfactors}{ number of factors; 
        only needed if \code{nlevels} is a single number and 
        \code{factor.names} is omitted;
        can otherwise determined from length of \code{factor.names}, 
        \code{nlevels} or \code{column} }
  \item{nlevels}{ number(s) of levels, vector with \code{nfactors} entries or single number; 
        can be omitted, if obvious from \code{factor.names} or if \code{ID} 
        and \code{columns} are given or if all columns of \code{ID} are to be used;
        can be a single number if \code{nfactors} is known directly or as length of 
        \code{factor.names} }
  \item{factor.names}{ like in \code{FrF2} }
  \item{columns}{ vector of column numbers referring to columns of design \code{ID};
        must not be specified, if \code{ID} is omitted; 
        the entries assign columns of the array to the factors}
  \item{replications}{ the number of replications of the array, 
        the setting of \code{repeat.only} determines, whether these are real replications
        or repeated measurements only. Note that replications are not considered for 
        accomodation of \code{min.residual.df} residual degrees of freedom, 
        unless a full factorial is used.}
  \item{repeat.only}{ default FALSE implies real replications, TRUE implies repeated measurements only }
  \item{randomize}{ logical indicating whether the run order is to be randomized ? }
  \item{seed}{ integer seed for the random number generator }
  \item{min.residual.df}{ minimum number of residual degrees of freedom }
}
\details{
   Function \code{oa.design} assigns factors to the columns of orthogonal arrays that are 
   available within package \pkg{DoE.wrapper}. The available arrays and their properties are listed in the 
   data frame \code{oacat}. The design names also indicate the number of runs and the numbers of factors 
   for each number of levels, e.g. L18.2.1.3.7 is an 18 run design with one factor with 
   2 levels and seven factors with 3 levels each. 
   
   \code{oa} is the S3 class used for orthogonal arrays. Objects of class \code{oa} should at least have 
   the attribute \dQuote{origin}, an attribute \dQuote{comment} should be used for additional information.
   
   Users can define their own orthogonal arrays and hand them to \code{oa.design} with parameter \code{ID}. 
   Requirements for the arrays: \cr
       Factor levels must be coded as a numbers from 1 to number of levels.\cr
       The array must be of classes \code{oa} and \code{matrix}.\cr
       The array should have an attribute \code{origin}.\cr
       The array can have an attribute \code{comment}; 
             this should be used for mentioning specific properties, e.g. 
             for the L18.2.1.3.7 that the interaction of the first two factors 
             can be estimated.
   
   Users are encouraged to send additional arrays to the package maintainer. 
   The requirements for these are the same as listed above, with attribute \code{origin} 
   being a MUST in this case.
   
   Currently, package \code{DoE.base} contains the orthogonal arrays from Warren Kuhfelds 
   collection of \dQuote{parent} arrays only. It is possible to combine these with each other, or 
   with Plackett-Burman, full or fractional factorial designs by nesting, as described 
   by Warren Kuhfeld. This is not currently implemented. 
   
   If no orthogonal array is found, 
   \code{oa.design} returns a full factorial, replicated for enough degrees of freedom, if necessary. 
   Note that replications specified with option \code{replications} are not counted in determining 
   residual degrees of freedom for \code{min.resid.df}. 
   
   Default factor names are the first elements of the character vector \code{Letters} (capital and small letters except for I and i), 
   or the factors position numbers preceded by capital F in case of more than 50 factors.
}
\value{\code{oa.design} returns a data frame of S3 class \code{\link{design}} 
    with attributes attached. Factors with numerical levels are numeric, factors with 
    character levels are factors. The content of the attributes in class \code{design} depends 
    on the specific type of design used.
    
    \code{origin} and \code{comment} return the respective attribute of the orthogonal array.
}
\references{ 
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
  
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ ~~See Also \code{FrF2}, \code{\link{oa.design}}, \code{pb} }
\examples{
  ## smallest available array for 6 factors with 3 levels each
  oa.design(nfactors=6,nlevels=3)
  ## level combination for which only a full factorial is (currently) found
  oa.design(nlevels=c(4,3,3,2))
  ## array requested via factor.names
  oa.design(factor.names=list(one=c("a","b","c"), two=c(125,275), three=c("old","new"), four=c(-1,1), five=c("min","medium","max")))
  ## array requested via character factor.names and nlevels (with a little German lesson for one two three four five)
  oa.design(factor.names=c("eins","zwei","drei","vier","fuenf"),nlevels=c(2,2,2,3,7))
  ## array requested via explicit name, Taguchi L18
  oa.design(ID=L18)
  ## array requested via explicit name, with column selection
  oa.design(ID=L18.3.6.6.1,columns=c(2,3,7))
  ## array requested with nruns, not very reasonable
  oa.design(nruns=12, nfactors=3, nlevels=2)
  ## array requested with min.residual.df
  oa.design(nfactors=3, nlevels=2, min.residual.df=12)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
