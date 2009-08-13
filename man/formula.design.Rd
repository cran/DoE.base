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
\method{formula}{design}(x, ..., response=NULL, degree=NULL)
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
}
\details{
Function \code{link[stats]{formula}} creates an appropriate formula for mny kinds of 
objects, e.g. for data frames (try e.g. \code{formula(swiss)}). Function 
\code{link[stats]{as.formula}} uses function formula, but cannot take any additional 
arguments. 

The method for class \code{design} objects modifies the way a data frame 
would normally be treated by the \code{formula} function. This also carries through 
to default linear models, as function \code{link[stats]{lm}} simply forces its first 
argument to be a formula via \code{as.formula}.

Without the additional arguments, the function creates the formula with the first 
response from the \code{response.names} element of the \code{design.info} attribute. 
The default degree depends on the type of design: it is 
\itemize{
\item{}{1 for \code{oa} and \code{pb}}
\item{}{2 for designs with types starting with \dQuote{FrF2} (blocked designs receive special treatment, 
  with blocks entering without any interactions)}
\item{}{will be 2 for response surface designs and lhs designs}
\item{}{will be the model formula used in optimizing the design for optimal designs}
}
}
\value{
a formula
}
\author{ Ulrike Groemping }
\seealso{ See also \code{\link[stats]{formula}} }
\examples{
   my.L18 <- oa.design(ID=L18, factor.names = c("one","two","three","four","five","six","seven"), nlevels=c(3,3,3,2,3,3,3))
   y <- rnorm(18)
   my.L18 <- add.response(my.L18, y)
   formula(my.L18)
   lm(my.L18)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
