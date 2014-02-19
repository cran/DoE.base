\name{Plotting class design objects}
\alias{plot.design}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting class design objects}
\description{
  The plot method for class design objects; other methods are part of a
  separate help page.
}
\usage{
\method{plot}{design}(x, y=NULL, select=NULL, selprop=0.25, ask=NULL, ...)
}
\arguments{
  \item{x}{ data frame of S3 class \code{\link{design}} }
  \item{y}{ a character vector of names of numeric variables in \code{x} to be plotted as responses,
      or a numeric response vector, or a numeric matrix containing response columns,
      or a data frame of numeric response variables (the latter would not work when directly
      using function \code{\link[graphics]{plot.design}} from package graphics)}
  \item{select}{
      Specification of selected factors through option \code{select}
      has been introduced in order to obtain manageable plot sizes. For example,
      mosaic plots are most easily readible for up to three or at most four factors.
      Main effects plots with too many factors may also be hard to read because of overlapping
      labeling. \code{select} can also be used for bringing the factors into a desirable order. \cr
      \code{select} can be\cr
      a vector of integers with position numbers of experimental factors,\cr
      a character vector of factor letters, \cr
      or a character vector of factor names for factors to be selected for plotting; \cr
      The following choicees are of interest for creating mosaic plots of the design 
      factors; any response data will be ignored.\cr
      \code{select} can be\cr
      a list of numeric vectors (all of equal length) specifying the tuples to be plotted 
      (a length one list (insted of the numeric vector itself) allows to plot a 
      the design table as a mosaic plot instead of showing a main effects 
      plot, if the design has responses)\cr
      one of the special character strings \code{"all2"}, \code{"all3"} or \code{"all4"}
      for obtaining mosaic plots of all pairs, triples or quadruples of (a selection of) factors 
      (see Details section), \cr
      a list with a numeric vector with position numbers of experimental factors as the first
      and one of the above special character strings as the second element for requesting all 
      tuples of a subset of the factors,\cr
      or a list with a single factor position number as the first and one of the above special 
      character strings as the second element for requesting all tuples that include 
      the specified single factor,\cr
      or any of the character strings \code{"complete"}, \code{"worst"}, \code{"worst.rel"}, 
      \code{"worst.parft"} or \code{"worst.parftdf"}  
      for automatic selection of the projections with the worst confounding to be plotted 
      (see Details section)
      }
  \item{selprop}{ a number between 0 and 1 indicating which proportion of 
      worst cases to plot in case \code{select=worst} or \code{select=worst.rel} 
      is to be plotted (see Details section). The default is useful for small 
      designs only. For large designs, reduce this number !
      }  
  \item{ask}{ a logical that is relevant only if \code{select} requests plots of
      all pairs, triples or quadruples from a set of factors;
      for the default setting \code{NULL}, \code{ask} is \code{TRUE}
      if the current graphics device is interactive (or none is open but be the next to be opened 
      device is interactive) and \code{FALSE} otherwise}
  \item{\dots}{ further arguments to functions \code{\link{plot}},
      \code{\link[vcd]{mosaic}}, or the function \code{\link[graphics]{plot.design}} from package graphics;\cr
      For experts, option \code{sub} with the special settings \code{"GR"}, \code{"A"},\code{"rA"}, 
      \code{"sumPARFT"} or \code{"sumPARFTdf"} 
      can be used to create sub titles that display the generalized resolution, 
      absolute or relative word lengths (see \code{\link{generalized.word.length}}). 
      All other specifications for \code{sub} should work as expected.}
  }
\details{
    Items of class \code{\link{design}} are data frames with attributes,
    that have been created for conducting experiments. Apart from the plot method
    documented here, separate files document the methods \code{\link{formula.design}},
    \code{\link{lm.design}}, and \code{\link[DoE.base:class-design-methods]{further methods}}.

    The method for \code{plot} calls the method available in package graphics
    (see \code{\link[graphics]{plot.design}}) wherever this makes sense (\code{x} not of class \code{design},
    \code{x} of class \code{design} but not following the class \code{\link{design}} structure
    defined in package \pkg{DoE.base},
    and \code{x} a design with all factors being R-factors and at least one response available).

    Function \code{\link[graphics]{plot.design}} from package \pkg{graphics} is not
    an adequate choice for designs without responses or designs with experimental factors
    that are not R-factors. \cr
    For designs with all factors being R-factors and no response defined (e.g. a freshly-created
    design from function \code{link{oa.design}}), function \code{plot.design} creates a mosaic plot of
    the frequency table of the design, which may be quite useful to understand the structure
    for designs with relatively few factors (cf. example below; function \code{plot.design} calls
    function \code{\link[vcd]{mosaic}} for this purpose).\cr
    For designs with at least one experimental factor that is not an R-factor, function
    \code{plot.design} calls function \code{\link[graphics:plot.dataframe]{plot.data.frame}} in order
    to create a scatter plot matrix.\cr
    Currently, there is no good method for plotting designs with mixed qualitative
    and quantitative factors.

    If option \code{select} is set to \code{"all2"}, \code{"all3"} or \code{"all4"},
    or a list with a numeric vector as its first element and one of these as the second element, 
    or with \code{select} as any of \code{"complete"}, \code{"worst"}, \code{"worst.rel"},
    \code{"worst.parft"} or \code{"worst.parftdf"},
    response variables are ignored, and mosaic plots are created.\cr 
    These requests usually ask for several plots; note that the plots are
    created one after the other; with an interactive graphics device, the default is that they overwrite each other
    after a user confirmation for the next plot, which allows users to visually inspect them one at a time;
    under Windows, the plotting series can be aborted using the \code{Esc}-key. 
    With non-interactive graphics devices, 
    the default is \code{ask=FALSE} (e.g. for storing all the plots
    in a multi-page file, see examples).
    
    If option \code{select} is any of \code{"all2"}, \code{"all3"} or \code{"all4"},
    mosaic plots of all pairs, triples or quadruples of factors are created as specified. \cr
    Note that \code{"all2"}
    is interesting for non-orthogonal designs only, e.g. ones created by function \code{Dopt.design}.\cr
    
    If option \code{select} is set to \code{"complete"}, \code{"worst"} \code{"worst.rel"},
    \code{"worst.parft"} or \code{"worst.parftdf"},
    the worst case tuples to be displayed are selected by function \code{\link{tupleSel}}.    
}
\value{
    The \code{plot} method is called for its side effects and returns \code{NULL}.
}
\note{ The package is currently subject to intensive development; most key functionality is now included.
Some changes to input and output structures may still occur.}

\references{
Groemping, U (2014) Mosaic plots are useful for visualizing low order projections 
    of factorial designs. To appear in \emph{The American Statistician}
    \url{http://www.tandfonline.com/action/showAxaArticles?journalCode=utas20}.
}

\author{ Ulrike Groemping }
\seealso{ See also the following functions known to produce objects of class
    \code{design}: \code{FrF2}, \code{pb}, \code{\link{fac.design}}, \code{\link{oa.design}},
    and function \code{\link[graphics]{plot.design}} from package graphics;
    a method for function \code{\link[stats]{lm}} is described in the separate help file
    \code{\link{lm.design}}.}
\examples{
#### Examples for plotting designs
oa12 <- oa.design(nlevels=c(2,2,6))
  ## plotting a design without response (uses function mosaic from package vcd)
  plot(oa12)
  ## equivalent to mosaic(~A+B+C, oa12)
  ## alternative order: mosaic(~C+A+B, oa12)
  plot(oa12, select=c(3,1,2))
  ## using the select function: the plots show that the projection for factors
  ## C, D and E (columns 3, 14 and 15 of the array) is a full factorial,
  ## while A, D and E (columns 1, 14, and 15 of the array) do not occur in
  ## all combinations
  plan <- oa.design(L24.2.13.3.1.4.1,nlevels=c(2,2,2,3,4))
  plot(plan, select=c("E","D","A"))
  plot(plan, select=c("E","D","C"))
  \dontrun{
  plot(plan, select="all3")
  plot(plan, select=list(c(1,3,4,5), "all3"))
  ## use the specialist version of option sub
  plot(plan, select=list(c(1,3,4,5), "all3"), sub="A")
  ## create a file with mosaic plots of all 3-factor projections
  pdf(file="exampleplots.pdf")
  plot(plan, select="all3", main="Design from L24.2.13.3.1.4.1 in default column order)")
  plot(plan, select="worst", selprop=0.3, sub="A")
  dev.off()
  ## the file exampleplots.pdf is now available within the current working
  ## directory
  }

  ## plotting a design with response
  y=rnorm(12)
  plot(oa12, y)
  ## plot design with a response included
  oa12.r <- add.response(oa12,y)
  plot(oa12.r)
  ## plotting a numeric design (with or without response,
  ##   does not make statistical sense here, for demo only)
  noa12 <- qua.design(oa12, quantitative="all")
  plot(noa12, y, main="Scatter Plot Matrix")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
