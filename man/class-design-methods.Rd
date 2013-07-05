\name{Methods for class design objects}
\alias{[.design}
\alias{aggregate.design}
\alias{print.design}
\alias{summary.design}
\alias{class-design-methods}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for class design objects}
\description{
  Methods for subsetting, aggregating, printing and summarizing class 
  design objects. The formula, lm and plot methods are subject of a separate help page.
}
\usage{
\method{[}{design}(x, i, j, drop.attr = TRUE, drop = FALSE)
\method{print}{design}(x, show.order=NULL, group.print=TRUE, std.order=FALSE, ...)
\method{summary}{design}(object, brief = NULL, quote = FALSE, ...)
\method{aggregate}{design}(x, ..., 
    by = NULL, response = NULL, FUN = "mean", postfix = NULL, replace = TRUE)
}
\arguments{
  \item{x}{ data frame of S3 class \code{\link{design}} }
  \item{i}{ indices for subsetting rows }
  \item{j}{ indices for subsetting columns }
  \item{drop.attr}{ logical, controls whether or not attributes are dropped; 
       if \code{TRUE}, the result is no longer of class \code{design}, and all special design attributes are dropped;
       otherwise, the design attributes are adjusted to reflect the subsetting result}
  \item{drop}{ logical that controls dropping of dimensions in the Extract function for 
       data.frame objects, which is called by the method for class \code{design}}
  \item{show.order}{ \code{NULL} or logical; if \code{TRUE}, the design is printed with run order information; 
        default is \code{TRUE} for design types for which this information is helpful (see code for detail), \code{FALSE} otherwise }
  \item{group.print}{ logical, default \code{TRUE}; 
        if \code{TRUE}, structured designs (blocked and split-plot designs) 
        are printed with intermediate lines at structure breaks; 
        if \code{FALSE}, the designs are simply printed as data frames.}
  \item{std.order}{ logical, default \code{FALSE}; 
        if \code{TRUE}, the design is printed in standard order rather 
        than in the randomized order.}
  \item{\dots}{ further arguments to functions \code{\link{print}}, \code{\link{summary}}, \code{\link{aggregate}}, 
        \code{\link{contrasts}} }
  \item{object}{ data frame of S3 class \code{design}, like argument \code{design} }
  \item{brief}{ \code{NULL} or logical; \code{TRUE} requests a printout of the 
       design at the end of the \code{summary} output, \code{FALSE} suppresses such a printout. 
       If \code{brief = NULL} (the default), the \code{summary} method prints 
       the design \code{object} if it has up to 40 rows and up to 20 columns. }
  \item{quote}{ logical; \code{TRUE} requests quoting strings in print parts of the output, 
       \code{FALSE} suppresses quotes.}
  \item{by}{by variables for the \code{\link[stats:aggregate]{data frame method}} 
        of function \code{aggregate}, 
       needed if \code{x} is not a wide design for which the special 
       method for class \code{\link{design}} is intended}
  \item{response}{used for wide format designs only; \cr
       if \code{NULL}, all responses of the design are aggregated; 
       specify names of selected responses (column names of the \code{responselist} element 
       of the \code{design.info} attribute) for restricting the responses that are treated}
  \item{FUN}{a function to be used for aggregation, the default is \code{"mean"};\cr
       can be used like the \code{FUN} argument to \code{\link{apply}} }
  \item{postfix}{\code{NULL} implies postfixing the response name with (a character version of) 
       \code{FUN}; a character string can be given instead for a user-defined postfix}
  \item{replace}{logical that decides whether an existing variable of the given name 
       is to be replaced; the default is \code{TRUE} for convenience reasons. 
       WARNING: If custom variables other than aggregation variables are added to wide format designs, 
       it is recommended to use variables names that are not likely to be generated 
       by this function.}
  }
\details{
    Items of class \code{\link{design}} are data frames with attributes,
    that have been created for conducting experiments. Apart from the methods
    documented here, separate files document the methods \code{\link{formula.design}}
    and \code{\link{plot.design}}.
    
    The extractor method subsets the design, taking care of the attributes accordingly (cf. the value section). 
    Subsetting can also handle replication in a limited way, although this is not first 
    choice. Repeated measurements can be added to a design that has no proper replications, 
    and proper replications can be added to a design that has no repeated measurements. 
    
    The method for \code{print} displays the design. Per default, the design is 
    printed in the actual run order, and run order information is shown for designs 
    with special structure (blocked, replicated). Optionally, the design can be 
    printed in standard order, which may be useful for comparing to other designs 
    or for getting a clearer idea about the structure of smaller designs. 
    
    The method for \code{summary} provides design-specific information - 
    some further development may still be expected. If a standard data frame summary 
    is desired, explicitly use function \code{summary.data.frame} instead of \code{summary}.
    
    The method for \code{aggregate} provides aggregation utilities for wide format designs and 
    links back to the method for data frames for designs that are not of wide format. 
    If a wide format design is to be treated with the \code{aggregate} method for data frames, 
    \code{\link[stats:aggregate]{aggregate.data.frame}} must be used explicitly. 
    This method calculates a mean, standard deviation or SN ratio from the individual responses 
    (which can be repeated measurements or outer array runs from a Taguchi parameter design).  
}
\value{
    \item{extractor}{The extractor function returns a class design object with modified attributes 
        or a data frame without special attributes, depending on the situation. 
        
        If \code{j} is given, the function always returns a data frame without 
        special attributes, even if \code{drop.attr=FALSE} or \code{j=1:ncol(design)}.\cr
        If only \code{i} is given, 
        the default option \code{drop.attr=TRUE} also returns a 
        data frame without attributes.\cr
        Exception: Even for \code{drop.attr=TRUE}, if \code{i} is a permutation of 
        the row numbers or a logical vector with all elements \code{TRUE}, 
        the attributes are preserved, and attributes \code{run.order} 
        and \code{desnum} are reordered along with the design, if necessary.\cr
        If \code{drop.attr=FALSE} and \code{j} is empty, 
        the function returns an object of class design with rows of attributes \code{run.order} 
        and \code{desnum} selected in line with those of the design itself. 
        In this case, the new \code{design.info} attribute is a list with entries 
        \describe{
        \item{type}{resolving to \dQuote{subset of design}, }
        \item{subset.rows}{ a numeric or logical vector with the selected rows, and}
        \item{orig.design.info}{ which contains the original \code{design.info} attribute.}
        }}
    
    The \code{print} and \code{summary} methods are called for their side effects and return \code{NULL}.
    
    The method for \code{aggregate} returns the input wide format design with one or more 
    additional response columns and the \code{response.names} element of the 
    \code{design.info} attribute changed to only include the newly-added responses. 
}
\note{ The package is currently subject to intensive development; most key functionality is now included. 
Some changes to input and output structures may still occur.}

\author{ Ulrike Groemping }
\seealso{ See also the following functions known to produce objects of class 
    \code{design}: \code{FrF2}, \code{pb}, \code{\link{fac.design}}, \code{\link{oa.design}}.\cr
    See also the following further methods for class \code{design} objects: 
    \code{\link{formula.design}}, \code{\link{lm.design}}, \code{\link{plot.design}}.\cr
    Function \code{\link[graphics]{plot.design}} from package graphics works on 
    data frames with R factors as explanatory variables, if a numeric response is available; 
    this function is invoked by method \code{\link{plot.design}} from this package, 
    where appropriate.}
\examples{
oa12 <- oa.design(nlevels=c(2,2,6))
#### Examples for extractor function
  ## subsetting to half the runs drops all attributes per default
  oa12[1:6,]
  ## keep the attributes (usually not reasonable, but ...)
  oa12[1:6, drop.attr=FALSE]
  ## reshuffling a design
  ## (re-)randomize
  oa12[sample(12),]
  ## add repeated measurements
  oa12[rep(1:12,each=3),]
  ## add a proper replication 
  ## (does not work for blocked designs)
  oa12[c(sample(12),sample(12)),]
  ## subsetting and rbinding to loose also contrasts of factors
  str(rbind(oa12[1:2,],oa12[3:12]))
  ## keeping all non-design-related attributes like the contrasts
  str(undesign(oa12))

#### Examples print and summary
  ## rename factors and relabel levels of first two factors
  namen <- c(rep(list(c("current","new")),2),list(""))
  names(namen) <- c("First.Factor", "Second.Factor", "Third.Factor")
  factor.names(oa12) <- namen
  oa12   ### printed with the print method!

  ## add a few variables to oa12
  responses <- cbind(temp=sample(23:34),y1=rexp(12),y2=runif(12))
  oa12 <- add.response(oa12, responses)
  response.names(oa12)
  ## temp (for temperature) is not meant to be a response 
  ## --> drop it from responselist but not from data
  response.names(oa12) <- c("y1","y2")

## print design
  oa12
## look at design-specific summary
  summary(oa12)
## look at data frame style summary instead
  summary.data.frame(oa12)
  
## aggregation examples
  plan <- oa.design(nlevels=c(2,6,2), replications=2, repeat.only=TRUE)
  y <- rnorm(24)
  z <- rexp(24)
  plan <- add.response(plan, cbind(y=y,z=z))
  plan <- reptowide(plan)
  plan.mean <- aggregate(plan)
  plan.mean
  aggregate(plan, response="z")
  aggregate(plan, FUN=sd)
  aggregate(plan, FUN = function(obj) max(obj) - min(obj), postfix="range")
  ## several aggregates: add standard deviations to plan with means
  plan.mean.sd <- aggregate(plan.mean, FUN=sd)
  plan.mean.sd
  response.names(plan.mean.sd)
  ## change response.names element of design.info back to y.mean and z.mean
  ## may be needed for automatic analysis routines that have not been 
  ## created yet
  plan.mean.sd <- aggregate(plan.mean.sd, FUN=mean)
  plan.mean.sd
  response.names(plan.mean.sd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
