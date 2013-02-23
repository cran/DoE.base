\name{qua.design}
\alias{qua.design}
\alias{change.contr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to switch between qualitative and quantitative factors and 
different contrast settings}
\description{
  The function allows to switch between qualitative and quantitative factors and 
different contrast settings.
}
\usage{
qua.design(design, quantitative = NA, contrasts = character(0), ...)
change.contr(design, contrasts=contr.treatment)
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
       with an entry \code{TRUE}, \code{NA} or \code{FALSE} for each factor, 
       where \code{TRUE} makes a factor into a numeric variable, 
       and \code{FALSE} makes it into a factor with treatment contrasts, and NA 
       reinstates the default factor settings;\cr 
       
       OR
       
       a named vector (names from the factor names of the design) with 
       an entry \code{TRUE}, \code{NA} or \code{FALSE} for each named factor 
       (implying no change for the omitted factors)
       }
  \item{contrasts}{ only takes effect for factors for which quantitative is \code{FALSE};\cr
       the default \code{character(0)} does not change any contrasts vs.~the previous 
       or default contrasts.\cr
       
       For customizing, a 
       
       character string \cr
       OR a character vector with a contrast name entry for each factor
       OR a named character vector of arbitrary length from 1 to number of factors\cr
       
       can be given; the names must correspond to names of factors 
       to be modified, and entries must be names of contrast functions. 
       The contrast functions are then applied to the respective factors 
       with the correct number of levels.\cr
       Possible contrast function names include (at least) 
       \code{\link{contr.FrF2}} (for number of levels a power of 2 only), 
       \code{\link[stats:contrast]{contr.helmert}}, 
       \code{\link[stats:contrast]{contr.treatment}}, 
       \code{\link[stats:contrast]{contr.SAS}}, 
       \code{\link[stats:contrast]{contr.sum}}, 
       \code{\link[stats:contrast]{contr.poly}}.
       CAUTION: Function \code{qua.design} checks whether the contrast names 
       actually define a function, but it is not checked 
       whether this function is a valid contrast function.
       }
  \item{\dots}{ currently not used }
}
\details{
  With function \code{qua.design}, option \code{quantitative} has the following implications:\cr
  An experimental factor for which quantitative is \code{TRUE} is recoded into a numeric variable.\cr 
  An experimental factor for which quantitative is \code{NA} is recoded into an R-factor 
  with the default contrasts given below.\cr 
  An experimental factor for which quantitative is FALSE is recoded into an R-factor 
  with treatment contrasts (default) or with custom contrasts as indicated by the 
  \code{contrasts} parameter. \cr
  If the intention is to change contrasts only, function \code{change.contr} 
  is a convenience interface to function \code{qua.design}.
  
  The default contrasts for factors in class \code{\link{design}} objects 
  (exception: purely quantitative design types like lhs or rsm designs) 
  depend on the number and content of levels: \cr
  2-level experimental factors are coded as R-factors with -1/1 contrasts, \cr
  experimental factors with more than two quantitative (=can be coerced to numeric) levels are 
  coded as R factors with polynomial contrasts (with scores the numerical levels of the factor), \cr
  and qualitatitve experimental factors with more than two levels are coded 
  as R factors with treatment contrasts. 
  
  Note that, for 2-level factors, the default contrasts from function \code{qua.design} 
  differ from the default contrasts with which the factors were generated in case of 
  functions \code{\link{fac.design}} or \code{\link{oa.design}}. Thus, for recreating 
  the original state, it may be necessary to explicity specify the desired contrasts.
  
  Function \code{change.contr} makes all factors qualitative. Per default, treatment 
  contrasts (cf. \code{\link[stats:contrast]{contr.treatment}}) 
  are assigned to all factors. The default contrasts can of course be modified.
  
  Warning: It is possible to misuse these functions especially for designs that have been 
  combined from several designs. For example, while setting factors in an lhs design 
  (cf. \code{\link[DoE.wrapper]{lhs.design}}) to 
  qualitative is prevented, if the lhs design has been crossed with another design of a different 
  type, it would be possible to make such a nonsensical modification.
  
}
\value{
  A data frame of class \code{\link{design}}; the element \code{quantitative} of attribute \code{design.info}, 
  the data frame itself and the \code{desnum} attribute are modified as appropriate.
}
\author{ Ulrike Groemping }
\examples{
## usage with all factors treated alike
y <- rnorm(12)
plan <- oa.design(nlevels=c(2,6,2))
lm(y~.,plan)
lm(y~., change.contr(plan))   ## with treatment contrasts instead
plan <- qua.design(plan, quantitative = "none")
lm(y~.,plan)
plan <- qua.design(plan, quantitative = "none", contrasts=c(B="contr.treatment"))
lm(y~.,plan)
plan <- qua.design(plan, quantitative = "none")
lm(y~.,plan)

plan <- qua.design(plan, quantitative = "all")
lm(y~.,plan)
plan <- qua.design(plan)  ## NA resets to default state
lm(y~.,plan)

## usage with individual factors treated differently
plan <- oa.design(factor.names = list(liquid=c("type1","type2"), 
     dose=c(0,10,50,100,200,500), temperature=c(10,15)))
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
