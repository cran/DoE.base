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
show.oas(name = "all", nruns = "all", nlevels = "all", factors = "all", 
    show = 10, parents.only = FALSE)
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
    an integer number specifying how many designs are to be listed (upper bound), 
    or the character string \code{"all"} for showing all designs, no matter how many. 
    The default is to show 10 designs. \code{show = 0} switches off the display of the result 
    and only returns a value.
}
  \item{parents.only}{
    logical specifying whether to show only parent arrays or child arrays as well; 
    the default is \code{FALSE} for inclusion of child arrays
}
}
\details{
   The function shows the arrays that are listed in the data frame \code{\link{oacat}}. 

   For child arrays that have to be generated with a lineage rule 
   (can be automatically done with function \code{\link{oa.design}}), the lineage is displayed 
   together with the design name. The option \code{parent.only = TRUE} 
   suppresses printing and output of child arrays. The structure of the lineage entry 
   is documented under \code{\link{oacat}}.
}
\value{
   A data frame with the three columns \code{name}, \code{nruns} and \code{lineage}, 
   containing the design name, the number of runs and - if applicable - the lineage for generating the design
   from other designs. The lineage entry is empty for parent designs that are either directly available 
   in the package and can be accessed by giving their name (e.g. \code{L18.3.6.6.1}) or are full factorials 
   (e.g. \code{L28.4.1.7.1}). 
   
   If no design has been found, the returned value is \code{NULL}.
}
\author{
   Ulrike Groemping 
}
\note{
   Thanks to Peter Theodor Wilrich for proposing such a function.
}
\references{ 
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
  
  Mee, R. (2009). \emph{A Comprehensive Guide to Factorial Two-Level Experimentation}. 
    New York: Springer.

}
\examples{
   ## the first 10 orthogonal arrays with 24 to 28 runs
   show.oas(nruns = c(24,28))
   ## the first 10 orthogonal arrays with 24 to 28 runs
   ## excluding child arrays
   show.oas(nruns = c(24,28), parents.only=TRUE)
   ## the orthogonal arrays with 4 2-level factors, one 4-level factor and one 5-level factor
   show.oas(factors = list(nlevels=c(2,4,5),number=c(4,1,1)))
   ## the orthogonal arrays with 4 2-level factors, one 7-level factor and one 5-level factor
   show.oas(factors = list(nlevels=c(2,7,5),number=c(4,1,1)))
   ## the latter orthogonal arrays with the nlevels notation 
   ## (that can also be used in a call to oa.design subsequently)
   show.oas(nlevels = c(2,7,2,2,5,2))
   ## calling designs by name 
   show.oas(name=c("L12.2.11", "L18.2.1.3.7"))
}
\seealso{
    \code{\link{oa.design}} for using the designs from \code{\link{oacat}} in design creation\cr
    \code{\link{oacat}} for the data frame underlying the function\cr
}

\keyword{ array }
\keyword{ design }
