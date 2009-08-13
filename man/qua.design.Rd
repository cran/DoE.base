\name{qua.design}
\alias{qua.design}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to switch between qualitative and quantitative factors and 
different contrast settings}
\description{
  The function allows to switch between qualitative and quantitative factors and 
different contrast settings.
}
\usage{
qua.design(design, quantitative = NA, contrasts = character(0), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{ an experimental design, 
                data frame of class \code{\link{design}} }
  \item{quantitative}{ can be 
  
       EITHER
       
       one of the single entries \cr
       NA for setting all factors to the default coding for class design (cf. details),\cr 
       \dQuote{all} for making all factors quantitative (=numeric),\cr
       \dQuote{none} for making all factors qualitative (=factor)\cr
       
       OR
       
       an unnamed vector of length \code{nfactors} 
       with an entry TRUE, NA or FALSE for each factor, 
       where TRUE makes a factor into a numeric variable, 
       and FALSE makes it into a factor with treatment contrasts, and NA 
       reinstates the default factor settings;\cr 
       
       OR
       
       a named vector (names from the factor names of the design) with 
       an entry TRUE, NA or FALSE for each named factor 
       (implying no change for the omitted factors)
       }
  \item{contrasts}{ only takes effect for factors for which quantitative is \code{FALSE};\cr
       the default \code{character(0)} does not change any contrasts vs. the previous 
       or default contrasts.\cr
       For customizing, a named character vector can be given; the names must correspond to names of factors 
       to be modified, and entries must be names of contrast functions. The contrast functions 
       are then applied to the respective factors with the correct number of levels.\cr
       Possible contrast function names include (at least) 
       contr.FrF2 (for number of levels a power of 2 only), contr.helmert, contr.treatment, contr.SAS, contr.sum, contr.poly.
       CAUTION: Function \code{qua.design} checks whether the contrast names actually define a function, but it is not checked 
       whether this function is a valid contrast function.}
  \item{\dots}{ currently not used }
}
\details{
  An experimental factor for which quantitative is TRUE is recoded into a numeric variable. 
  An experimental factor for which quantitative is NA is recoded into an R-factor 
  with the default contrasts given below. An experimental factor for which quantitative is FALSE´is 
  recoded into an R-factor with treatment contrasts (default) or with custom contrasts as indicated by the 
  \code{contrasts} parameter.
  
  The default contrasts for factors in class \code{design} objects 
  (exception: purely quantitative design types like lhs or rsm designs) 
  depend on the number and content of levels: \cr
  2-level experimental factors are coded as R-factors with -1/1 contrasts, \cr
  experimental factors with more than two quantitative (=can be coerced to numeric) levels are 
  coded as R factors with polynomial contrasts (with scores the numerical levels of the factor), \cr
  and qualitatitve experimental factors with more than two levels are coded 
  as R factors with treatment contrasts. 
  
  Warning: It is possible to misuse this function especially for designs that have been 
  combined from several designs. For example, while setting factors in an lhs design to 
  qualitative is prevented, if the lhs design has been crossed with another design of a different 
  type, it would be possible to make such a nonsensical modification.
  
}
\value{
  A data frame of class \code{\link{design}}; the element \code{quantitative} of attribute \code{design.info}, 
  the data frame itself and the \code{desnum} attribute are modified as appropriate.
}
\references{ ~put references to the literature/web site here ~ }
\author{ Ulrike Groemping }
\note{ This function is still experimental.
}
\examples{
## usage with all factors treated alike
plan <- oa.design(nlevels=c(2,6,2))
plan
plan <- qua.design(plan, quantitative = "none")
plan
plan <- qua.design(plan, quantitative = "all")
plan
plan <- qua.design(plan)
plan
## usage with individual factors treated differently
plan <- oa.design(factor.names = list(liquid=c("type1","type2"), dose=c(0,10,50,100,200,500), temperature=c(10,15)))
str(undesign(plan))
\dontrun{
## would cause an error, since liquid is character and cannot be reasonably coerced to numeric
plan <- qua.design(plan, quantitative = "all")
}
plan <- qua.design(plan, quantitative = "none")
str(undesign(plan))

plan <- qua.design(plan, quantitative = c(dose=TRUE,temperature=TRUE))
str(undesign(plan))
## reset all factors to default
plan <- qua.design(plan, quantitative = NA)
str(undesign(plan))
desnum(plan)
## add a response
y <- rnorm(12)
plan <- add.response(plan,y)
## set dose to treatment contrasts
plan <- qua.design(plan, quantitative = c(dose=FALSE), contrasts=c(dose="contr.treatment"))
str(undesign(plan))
desnum(plan)


}
\keyword{ design }
\keyword{ array }% __ONLY ONE__ keyword per line
