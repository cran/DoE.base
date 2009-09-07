\name{oa.design}
\alias{oa.design}
\alias{origin}
\alias{oacat}
\alias{oa}
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
oacat
oa
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{ orthogonal array to be used; must be given as the name without quotes 
        (e.g. \code{L12.2.2.6.1}); 
        available names can be looked at via \code{oacat$name};
        users can also specify names of their own designs here (cf. details). 
        \code{ID} must be of class \code{oa}.
        If omitted, \code{ID} is automatically determined based on \code{nlevels} 
        or \code{factor.names}. }
  \item{nruns}{ minimum number of runs to be used, 
        can be omitted if obvious from \code{ID} or if 
        the smallest possible array is to be found }
  \item{nfactors}{ number of factors; 
        only needed if \code{nlevels} is a single number and 
        \code{factor.names} is omitted;
        can otherwise determined from length of \code{factor.names}, 
        \code{nlevels} or \code{column} }
  \item{nlevels}{ number(s) of levels, vector with \code{nfactors} entries or single number; 
        can be omitted, if obvious from \code{factor.names} or if \code{ID} 
        and \code{columns} are given or if all columns of \code{ID} are to be used 
        with default factor names and levels;
        can be a single number if \code{nfactors} is known directly or as length of 
        \code{factor.names} }
  \item{factor.names}{ a character vector of \code{nfactors} factor names 
      or a list with \code{nfactors} elements;\cr 
      if the list is named, list names represent factor names, otherwise default factor names are used;\cr 
      the elements of the list are \cr
      EITHER vectors of appropriate length (corresponding to \code{nlevels}) 
      with factor levels for the respective factor\cr
      OR empty strings;\cr
       Default factor names are the first elements of the character vector \code{\link{Letters}}, 
       or the factors position numbers preceded by capital F in case of more than 50 factors.
       Default factor levels are the numbers from 1 to the number of levels for each factor.}
  \item{columns}{ vector of column numbers referring to columns of design \code{ID};
        must not be specified, if \code{ID} is omitted; 
        the entries assign columns of the array to the factors;\cr
        it may be possible to improve on the properties of the design by using 
        the columns option, since the default column choice is not always ideal!}
  \item{replications}{ the number of replications of the array, 
        the setting of \code{repeat.only} determines, whether these are real replications
        or repeated measurements only. Note that replications are not considered for 
        accomodation of \code{min.residual.df} residual degrees of freedom, 
        unless a full factorial is used.}
  \item{repeat.only}{ default \code{FALSE} implies real replications, 
        \code{TRUE} implies repeated measurements only }
  \item{randomize}{ logical indicating whether the run order is to be randomized ? }
  \item{seed}{ integer seed for the random number generator }
  \item{min.residual.df}{ minimum number of residual degrees of freedom;\cr
        Note: function \code{oa.design} does not count replications specified with option 
       \code{replications} in determining residual degrees of freedom for \code{min.resid.df}. }
}
\details{
   Function \code{oa.design} assigns factors to the columns of orthogonal arrays that are 
   available within package \pkg{DoE.base} or are provided by the user. 
   The available arrays and their properties are listed in the 
   data frame \code{oacat}. The design names also indicate the number of runs and the numbers of factors 
   for each number of levels, e.g. \code{L18.2.1.3.7} is an 18 run design with one factor with 
   2 levels and seven factors with 3 levels each. 
   
   \code{oa} is the S3 class used for orthogonal arrays. Objects of class \code{oa} should at least have 
   the attribute \code{origin}, an attribute \code{comment} should be used for additional information.
   
   Users can define their own orthogonal arrays and hand them to \code{oa.design} with parameter \code{ID}. 
   Requirements for the arrays: \cr
   \itemize{
       \item Factor levels must be coded as numbers from 1 to number of levels.
       \item The array must be of classes \code{oa} and \code{matrix}\cr 
       (If your array is a matrix named \code{foo}, you can simply assign it class \code{oa} by 
       the command \code{class(foo) <- c("oa","matrix")}, see also last example.)
       \item The array should have an attribute \code{origin}.
       \item The array can have an attribute \code{comment}; \cr
             this should be used for mentioning specific properties, e.g. 
             for the \code{L18.2.1.3.7} that the interaction of the first two factors 
             can be estimated.
             }
   
   Users are encouraged to send additional arrays to the package maintainer. 
   The requirements for these are the same as listed above, with attribute \code{origin} 
   being a MUST in this case. (See the last example for how to assign an attribute.)
   
   Currently, package \pkg{DoE.base} contains the orthogonal arrays from Warren Kuhfelds 
   collection of \dQuote{parent} arrays only, plus very few additional designs. 
   It is possible to combine these with each other, or 
   with Plackett-Burman, full or fractional factorial designs by expansive replacement, as described 
   by Warren Kuhfeld. The thus-obtainable child arrays have so far not been implemented; 
   as long as these are not available, function \code{oa.design} will 
   often suggest larger designs than necessary. The child arrays with up to 143 runs 
   listed in Warren Kuhfelds technical report will be implemented relatively soon. \cr
   For the interim, consider the last example for combining 
   designs manually. (A lot more than just the child arrays could be obtained from these arrays 
   by implementing a functionality similar to the market research macros available in SAS; presumably, 
   this topic will not be addressed soon, as it will involve a substantial amount of work.)
   
   If no specific orthogonal array is specified and function \code{oa.design} does not 
   find an orthogonal array that meets the specified requirements, 
   \code{oa.design} returns a full factorial, replicated for enough residual degrees of freedom, if necessary. 
   If \code{oa.design} has not found an array smaller than the full factorial, it is 
   absolutely possibly that a smaller array does exist nevertheless. It may be worth 
   while checking with oacat whether an appropriate smaller array can be found by 
   combining some of the parent arrays listed there (looking for a design with a few 
   factors in 5 runs, you may e.g. call \code{oacat[oacat$n5>0,]$name} in order to see the 
   names of more promising candidate arrays for combination, or you may also want to look 
   up arrays with \code{n25>0} subsequently.
   
   WARNING: Usually, it is recommended to investigate the properties of a design automatically 
   created by function \code{oa.design} before starting experimentation. 
   While all designs can estimate main effects \emph{in the absence of interactions}, 
   the presence of interactions may render some designs useless or even dangerous. 
   Deliberate choice of columns different from the default may improve a design 
   (an example for this will be added shortly)!

}
\value{\code{oa.design} returns a data frame of S3 class \code{\link{design}} 
    with attributes attached. 
    
   In the data frame itself, the experimental factors are all stored as R factors. \cr
   For factors with 2 levels, \code{\link{contr.FrF2}} contrasts (-1 / +1) are used. \cr
   For factors with more than 2 numerical levels, polynomial contrasts are used 
   (i.e. analyses will per default use orthogonal polynomials). \cr
   For factors with more than 2 categorical levels, the default contrasts are used.\cr
   Future versions will most likely allow more user control about the type of 
   contrasts to be used.

   The \code{desnum} and \code{run.order} attributes of class \code{\link{design}} are 
   as usual. In the \code{design.info} attribute, the following elements are specific for 
   this type of designs:
   \item{type}{is \code{oa} (unless no special orthogonal array is found, in which case 
      a full factorial is created instead, cf. \code{\link{fac.design}} for its \code{design.info} attribute),} 
   \item{nlevels}{vector containing the number of levels for each factor}
   \item{generating.oa}{contains information on the generating orthogonal array,}
   \item{selected.columns}{contains information, which column of the orthogonal array underlies which factor,}
   \item{origin}{contains the respective attribute of the orthogonal array,} 
   \item{comment}{contains the respective attribute of the orthogonal array,} 
   \item{residual.df}{contains the requested residual degrees of freedom for a main effects model.} 
   Other information is generic, like documented for class \code{\link{design}}. 
   
   Function \code{origin} returns the origin attribute of the orthogonal array \code{ID}, 
   functions \code{\link{comment}} and \code{"comment<-"} from package \pkg{base} 
   return and set the comment attribute.
}
\author{ Ulrike Groemping }
\references{ 
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
  
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
}
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link[FrF2]{FrF2}}, \code{\link{fac.design}}, \code{\link[FrF2]{pb}} }
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
  ## The last example:
  ## generate an orthogonal array equivalent to Taguchi's L54.2.1.3.25 array 
  ## (which is not currently included) up to swaps of levels within a factor 
  ## for some of the factors J to a (i.e. factors 9 to 26) 
  ## by combining L54.3.18.18.1 with L18.2.1.3.7
  ## look at catalogued designs for checking possibilities first
  ## assign class such that design is usable in function oa.design
  ## assign attributes for documentation purpose
  oacat[oacat$nruns==54,]$name   
                 ##L54.3.18.18.1 looks most promising
  oacat[oacat$nruns==18,]$name   
                 ##L18.2.1.3.7 will deliver one 2-level and 25 3-level factors
                 ##(it can itself be generated from L18.3.6.6.1 by nesting L6.2.1.3.1)
  parent.des <- oa.design(ID=L54.3.18.18.1, nlevels=c(rep(3,18),18),factor.names=c(Letters[9:26],"comb"))
  nest.des <- oa.design(ID=L18.2.1.3.7)
  L54.2.1.3.25 <- cbind(nest.des[parent.des$comb,], parent.des)[-27]
  rownames(L54.2.1.3.25) <- rownames(parent.des)
  L54.2.1.3.25[ord(L54.2.1.3.25),]  ## look at sorted array
    ## prepare for using it with function oa.design
    ## (ordering it is not necessary, just **tidy**)
  L54.2.1.3.25 <- as.matrix(L54.2.1.3.25[ord(L54.2.1.3.25),])
  class(L54.2.1.3.25) <- c("oa", "matrix")
  attr(L54.2.1.3.25, "origin") <- "L54.3.18.18.1, 18.1 -> L18.2.1.3.7"
  comment(L54.2.1.3.25) <- comment(L18.2.1.3.7)
     ## uses comment function from package base
  origin(L54.2.1.3.25)
  comment(L54.2.1.3.25)
    ## can now be used in oa.design, like the built-in arrays
  oa.design(ID=L54.2.1.3.25,nfactors=22,nlevels=3) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
