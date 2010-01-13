\name{show.oas}
\alias{show.oas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to display list of available orthogonal arrays
}
\description{
This function allows to inspect the list of available orthogonal arrays, 
specifying optionally specifying selection criteria.
}
\usage{
show.oas(name = "all", nruns = "all", nlevels = "all", factors = "all", show = 10)
}
\arguments{
  \item{name}{
    character string or vector of character strings giving name(s) of (an) orthogonal 
    array(s); results in an error if name does not contain any valid name; 
    warns if name contains any invalid name
}
  \item{nruns}{
    the requested number of runs or a 2-element vector 
    with a minimum and maximum for the number of runs 
}
  \item{nlevels}{
    a vector of requested numbers of levels for a set of factors in question, 
    must contain integers > 1 only;\cr
    nlevels cannot be specified together with factors
}
  \item{factors}{
    a list with the two elements \code{nlevels} and \code{number}, which are both integer 
    vectors of equal length;\cr 
    \code{nlevels} contains the number of levels and \code{number} the number of factors 
    for the corresponding number of levels
}
  \item{show}{
    an integer number specifying how many designs are to be listed (upper bound)
}
}
\details{
   The arrays are listed in the data frame \code{\link{oacat}}. 
   The design names also indicate the number of runs and the numbers of factors: 
   The first portion of each array name (starting with L) indicates number of runs, 
   each subsequent pair of numbers indicates a number of levels together with the frequency with which it occurs.
   For example, \code{L18.2.1.3.7} is an 18 run design with one factor with 
   2 levels and seven factors with 3 levels each. 
   
   Apart from \code{L18} and \code{L36} (Taguchi, but also in the collection under different names), 
   the source for the arrays is Warren Kuhfelds 
   collection of \dQuote{parent} arrays. 
   It is possible to combine these with each other, or 
   with Plackett-Burman, full or fractional factorial designs by nesting, as described 
   by Warren Kuhfeld. This is not currently automatically implemented. For manual implementation, 
   see an example in the help for function \code{\link{oa.design}}.
   (The two Taguchi arrays are derived arrays, not parent arrays, and are therefore 
   explicitly included.)
}
\value{
   The function displays informtion. No value is returned.
}
\author{
   Ulrike Groemping 
}
\note{
   Thanks to Peter Theodor Wilrich for proposing such a function.
}
\examples{
   ## the orthogonal arrays with 24 to 28 runs
   show.oas(nruns = c(24,28))
   ## the first 20 orthogonal arrays with 24 to 36 runs
   show.oas(nruns = c(24,36), show=20)
   ## the orthogonal arrays with 4 2-level factors, one 4-level factor and one 5-level factor
   show.oas(factors = list(nlevels=c(2,4,5),number=c(4,1,1)))
   ## the orthogonal arrays with 4 2-level factors, one 7-level factor and one 5-level factor
   show.oas(factors = list(nlevels=c(2,7,5),number=c(4,1,1)))
   ## the letter orthogonal arrays with the nlevels notation 
   ## (that can also be used in a call to oa.design subsequently)
   show.oas(nlevels = c(2,7,2,2,5,2))
   ## calling designs by name (currently not very useful)
   show.oas(name=c("L18","L12.2.11"))
}
\seealso{
    \code{\link{oacat}} for the catalog of orthogonal arrays,\cr
    \code{\link{oa.design}} for using this catalogue in design creation
}

\keyword{ array }
\keyword{ design }
