\name{Methods for class design objects}
\alias{[.design}
\alias{print.design}
\alias{summary.design}
\alias{plot.design}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Methods for class design objects}
\description{
  Methods for subsetting, printing, summarizing and plotting class 
  design objects
}
\usage{
\method{[}{design}(x, i, j, drop.attr = TRUE, drop = FALSE)
\method{print}{design}(x, show.order=NULL, ...)
\method{summary}{design}(object, ...)
\method{plot}{design}(x, y=NULL, ...)
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
  \item{\dots}{ further arguments to functions \code{\link{print}}, \code{\link{summary}}, or in case of plotting, 
        \code{\link{plot}}, \code{\link[vcd]{mosaic}}, or the function \code{\link[graphics]{plot.design}} from package graphics  }
  \item{object}{ data frame of S3 class \code{design}, like argument \code{design} }
  \item{y}{ a character vector of names of numeric variables in \code{x} to be plotted as responses, 
      or a numeric response vector, or a numeric matrix containing response columns, 
      or a data frame of numeric response variables (the latter would not work when directly 
      using function \code{\link[graphics]{plot.design}} from package graphics)}
}
\details{
    Items of class \code{\link{design}} are data frames with attributes, 
    that have been created for conducting experiments. 
    
    The extractor method subsets the design, taking care of the attributes accordingly (cf. the value section). 
    Subsetting can also handle replication in a limited way, although this is not first 
    choice. Repeated measurements can be added to a design that has no proper replications, 
    and proper replications can be added to a design that has no repeated measurements. 
    
    The methods for \code{print} or \code{summary} provide design-specific information - 
    especially the \code{summary} method will develop further. If a standard data frame summary 
    is desired, explicitly use function \code{summary.data.frame} instead of \code{summary}.
    
    The method for \code{plot} calls the method available in package graphics 
    (see \code{\link[graphics]{plot.design}}) wherever this makes sense (\code{x} not of class \code{design}, 
    \code{x} of class \code{design} but not following the class \code{\link{design}} structure 
    defined in package \pkg{DoE.base}, 
    and \code{x} a design with all factors being R-factors and at least one response available). 
    
    Function \code{\link[graphics]{plot.design}}) from package \pkg{graphics} is not 
    an adequate choice for designs without responses or designs with experimental factors 
    that are not R-factors. \cr
    For designs with all factors being R-factors and no response defined (e.g. a freshly-created 
    design from function \code{link{oa.design}}), function \code{plot.design} creates a mosaic plot of 
    the frequency table of the design, which may be quite useful to understand the structure 
    for designs with relatively few factors (cf. example below; function \code{plot.design} calls 
    function \code{\link[vcd]{mosaic}} for this purpose).\cr
    For designs with at least one experimental factor that is not an R-factor, function 
    \code{plot.design} calls function \code{\link[graphics]{plot.data.frame}} in order 
    to create a scatter plot matrix.\cr 
    Currently, there is no good method for plotting designs with mixed qualitative 
    and quantitative factors.
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
    The \code{print}, \code{summary} and \code{plot} methods are called for their side effects and return \code{NULL}.
}
\note{ The package is currently subject to intensive development; most key functionality is now included. 
Some changes to input and output structures may still occur.}

\author{ Ulrike Groemping }
\seealso{ See also the following functions known to produce objects of class 
    \code{design}: \code{FrF2}, \code{pb}, \code{\link{fac.design}}, \code{\link{oa.design}},
    and function \code{\link[graphics]{plot.design}} from package graphics }
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

#### Examples for plotting designs
  ## plotting a design without response (uses function mosaic from package vcd)
  plot(oa12)
  ## equivalent to mosaic(~A+B+C, oa12)
  ## alternative order:
  mosaic(~C+A+B, oa12)
  ## plotting a design with response
  y=rnorm(12)
  plot(oa12, y)
  ## plotting a numeric design (with or without response, 
  ##   does not make statistical sense here, for demo only)
  noa12 <- qua.design(oa12, quantitative="all")
  plot(noa12, y, main="Scatter Plot Matrix")

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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
