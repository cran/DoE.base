\name{formula.design}
\alias{formula.design}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to change the default formula for a data frame of 
class design to involve the correct factors with the desired effects 
and responses }
\description{
This function provides a reasonable default formula for linear model analyses 
of class design objects with response(s). Per default, the resulting formula 
refers to the first response in the design and is of design-type specific 
nature.
}
\usage{
\method{formula}{design}(x, ..., response=NULL, degree=NULL, FUN=NULL, 
      use.center=NULL, use.star=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{\link{design}}}
  \item{\dots}{ further arguments to function \code{\link[stats]{formula}}}
  \item{response}{ character string giving the name of the response variable 
          (must be among the numeric columns from \code{x}) \cr
          OR\cr
          integer number giving the position of the response in element 
          \code{response.names} of attribute \code{design.info}
          }
  \item{degree}{ degree of the model (1=main effects only, 2=with 2-factor 
           interactions and quadratic effects, 3=with 3-factor interactions and 
           up to cubic effects, ... }
  \item{FUN}{ function for the \code{\link{aggregate.design}} method; 
        this must be an unquoted function name or \code{NULL};\cr
        This option is relevant for repeated measurement designs 
        and parameter designs in long format only}
   \item{use.center}{ \code{NULL} or logical indicating whether center points are to be used +
        in the analysis; \cr
        if \code{NULL}, the default is FALSE for \code{pb} and \code{FrF2} designs with 
        center points and TRUE for \code{ccd} designs; the option is irrelevant 
        for all other design types.
        }
  \item{use.star}{ \code{NULL} or logical indicating whether the star portion of a CCD design 
        is to be used in the analysis (ignored for all other types of designs). 
        }
}
\details{
Function \code{\link[stats]{formula}} creates an appropriate formula for many kinds of 
objects, e.g. for data frames (try e.g. \code{formula(swiss)}). Function 
\code{\link[stats]{as.formula}} uses function formula, but cannot take any additional 
arguments. 

The method for class \code{design} objects modifies the way a data frame 
would normally be treated by the \code{formula} function. This also carries through 
to default linear models.

Without the additional arguments, the function creates the formula with the first 
response from the \code{response.names} element of the \code{design.info} attribute. 
The default degree depends on the type of design: it is 
\itemize{
\item{}{1 for \code{oa} and \code{pb}}
\item{}{2 for all other design types}}

\code{degree} does not have an effect for response surface designs 
  (types \code{bbd}, \code{bbd.blocked} and \code{ccd}) and latin hypercube designs (type \code{lhs}), 
  where the function always creates the formula for a full second order model including quadratic 
  effects.
 
Where \code{degree} does have an effect, it is the exponent of the sum of all experimental factors, 
i.e. it refers to the degree of interactions, not to powers of the variables themselves 
(e.g. \code{(A+B+C)^2} for degree 2).

For designs with a block variable (types \code{FrF2.blocked}, \code{bbd.blocked} and \code{ccd}) 
the block variable enters the formula as a main effect factor without any interactions. 

For 2-level designs with center points (types \code{FrF2.center} or \code{pb.center}), 
the formula contains an indicator variable \code{center} for the center points 
that can is used for checking whether quadratic effects are needed.

For designs with repeated measurements (\code{repeat.only} and parameter designs, 
the default is to analyse aggregated responses. For more detail, 
see the documentation of \code{\link{lm.design}}. 

For optimal designs (not implemented yet), the formula will be the model formula 
used in optimizing the design.
}

\value{
a formula
}
\author{ Ulrike Groemping }
\seealso{ See also \code{\link[stats]{formula}} and \code{\link{lm.design}} }
\examples{
  ## indirect usage via function lm.design is much more interesting
  ## cf help for lm design!

   my.L18 <- oa.design(ID=L18, 
       factor.names = c("one","two","three","four","five","six","seven"), 
       nlevels=c(3,3,3,2,3,3,3))
   y <- rnorm(18)
   my.L18 <- add.response(my.L18, y)
   formula(my.L18)
   lm(my.L18)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
