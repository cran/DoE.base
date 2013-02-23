\name{Reshape designs with repeated measurements}
\alias{reptowide}
\alias{reptolong}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Reshape designs with repeated measurements}
\description{
  Convenience functions to reshape a design with repeated measurements 
  from long to wide or vice versa
}
\usage{
### generic function
reptowide(design, constant=NULL, ...)
reptolong(design)
}
\arguments{
  \item{design}{ 
      a data frame of S3 class \code{design}. \cr
      For function \code{reptowide}, the design must have repeated measurements 
      (\code{repeat.only=TRUE} in \code{design.info} attribute). \cr
      For \code{reptolong}, the design must be in the wide form produced 
      by function \code{reptowide}.} 
  \item{constant}{ \code{NULL} or character vector; 
      if \code{design} contains variables other than the experimental 
      factors and the block column (e.g. covariables) that do not 
      change over repeated measurements within the same experimental unit,
      \code{constant} must be a character vector with the respective variable names}
  \item{\dots}{ currently not used }
}

\details{
      Both functions leave the design unchanged (with a warning) 
      for all class \code{design} objects that are not of the required 
      repeated measurements form. 
      
      If \code{design} is not of class \code{design}, an error is thrown.
      
      The \code{reptowide} function makes use of the function \code{\link[stats]{reshape}} 
      in package \pkg{stats}, the \code{reptolong} function does not. 
      
}

\value{
    A data frame of class \code{design} with the required reshaping. 
    
    The \code{reptowide} function returns a design with one row containing 
    all the repeated measurements for the same experimental setup (therefore wide),
    the \code{reptolong} function reshapes a wide design back into the 
    long form with all repeated measurements directly underneath each other. 
    
    The attributes of the design are treated along with the data frame itself: 
    The \code{reptowide} function resets elements of the \code{design.info} 
    attribute (\code{response.names}, \code{repeat.only}) and adds the new elements 
    \code{format} with value \dQuote{repeatedMeasuresWide}, 
    \code{responselist} and, if there are variables that are neither experimental 
    factors nor responses, \code{restlist} for those of these that do change 
    with repeated measurements. The \code{reptolong} function reinstates 
    the original long version. 
    
    Note that the order of variables may change, if there are any variables 
    in addition to the factors and responses.
}
\author{ Ulrike Groemping }
\note{ The package is currently subject to intensive development; most key functionality is now included. 
Some changes to input and output structures may still occur.}
\seealso{ See Also \code{\link[FrF2]{FrF2}}, \code{\link[FrF2]{pb}}, 
      \code{\link{fac.design}}, \code{\link{oa.design}} }
\examples{
    ### design without response data
    ### response variable y is added per default
    plan <- oa.design(nlevels=c(2,6,2), replication=2, repeat.only=TRUE)
    pw <- reptowide(plan)  ## make wide
    pl <- reptolong(pw)  ## make long again
    
    ### design with response and further data
    y <- rexp(24)
    temp <- rep(sample(19:30),each=2)  ## constant covariable
    prot.id <- factor(Letters[1:24]) ## non-constant character covariable
    plan.2 <- add.response(plan, y)
    plan.2$temp <- temp     ## not response
    plan.2$prot.id <- prot.id   ##not response
    plan.2
    reptowide(plan.2, constant="temp")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line

