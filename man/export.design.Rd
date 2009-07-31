\name{export.design}
\alias{export.design}
\alias{html}
\alias{html.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for exporting a design object }
\description{
  Function for exporting a design object
}
\usage{
export.design(design, response.names = NULL, 
       path = ".", filename = NULL, legend = NULL, type = "html", 
       OutDec = options("OutDec")$OutDec, replace = FALSE, ...)
html(object, ...)
\method{html}{data.frame}(object, file = paste(first.word(deparse(substitute(object))),
    "html", sep = "."), append = FALSE, link = NULL, linkCol = 1, bgs.col = NULL,
          OutDec=options("OutDec")$OutDec, linkType = c("href", "name"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{ A data frame of class design; 
        it must be stored in the global environment and referred to by its name, i.e. 
        it cannot be created \dQuote{on the fly}.  }
  \item{response.names}{ if not NULL (default), 
        this must be a character vector of response names; 
        the exported file contains a column for each entry;
        it is NOT necessary to include responses that are already present in 
        the design object!}
  \item{path}{ the path to the directory where the export files are to be stored;
        the default corresponds to the R working directory that can (on some systems) 
        be looked at using \code{getwd()}}
  \item{filename}{ character string that gives the file name (without extension) 
        for the files to be exported; if NULL, it is the name of the design object }
  \item{legend}{ data frame containing legend information; if NULL, 
        the legend is automatically generated from the 
        \code{factor.names} element of \code{design.info(design)}}
  \item{type}{ one of \code{"rda"}, \code{"html"}, \code{"csv"}, or \code{"all"}. 
        An R workspace with just the design object is always stored as an \code{"rda"} object. 
        If one of the other types is specified, the design is additionally exported to \code{"html"} 
        or \code{"csv"} or both. The \code{"csv"} file contains the design itself only, 
        with formatting depending on the \code{OutDec} option. The \code{"html"} file contains 
        some additional legend information and row color formatting.}
  \item{OutDec}{ decimal separator for the output file; one of \code{"."} or \code{","}; 
        the default is the option setting in the R options; this option also directs  
        whether \code{write.csv} or \code{write.csv2} is used and is very important for 
        usability of the exported files e.g. with Excel}
  \item{replace}{ logical indicating whether an existing file should be replaced; 
        if FALSE (default), the routine aborts without any action if one of the 
        files to be created exists; checking is not case-sensitive in order to protect 
        users on case-insensitive platforms from inadvertent replacing of files (i.e. 
        you cannot have TEST.html and test.html, even if it were allowed on your platform) }
  \item{object}{ object to be exported to html }
  \item{file}{ file to export the object to }
  \item{append}{ append data frame to existing file ? }
  \item{link}{ not used, unchanged from package \pkg{Hmisc} }
  \item{linkCol}{ not used, unchanged from package \pkg{Hmisc} }
  \item{bgs.col}{ background colors for data frame rows, default white and grey }
  \item{linkType}{ not used, unchanged from package \pkg{Hmisc} }
  \item{\dots}{ further arguments to function \code{html}, 
       usable e.g. for modifying row coloring }
}
\details{
   Function \code{export.design} always stores an R workspace that contains just 
   the design (with attached attributes, cf. class \code{\link{design}}). This file is stored 
   with ending \code{rda}.\cr 
   If requested by options \code{type="csv"}, \code{type="html"}, or \code{type="all"}, 
   \code{export.design} additionally creates an exported version of 
   the design that is usable outside of R. This is achieved via functions 
   \code{write.csv}, \code{write.csv2} or \code{html}. 
   The csv-file contains the data frame itself only, the html file contains the data frame 
   followed by the legend to the right of the data frame. The html file uses row coloring in 
   order to prevent mistakes in recording of experimental results by mix-ups of rows. 
   If the OutDec option is correct for the current computer, the csv and html files can 
   be opened in Excel, and decimal numbers are correctly interpreted. 
   
   Generation of the html-file is particularly important for Taguchi inner/outer array designs 
   in wide format, because it provides the legend to the suffix numbers of response columns in terms of 
   outer array experimental setups!
}
\value{The functions are used for their side effects and do not generate a result.
}
\references{ 
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
}
\author{ Ulrike Groemping; the html functions have been adapted from package Hmisc }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ ~~See Also \code{FrF2}, \code{\link{oa.design}}, \code{pb} }
\examples{
  ## six 2-level factors
  test <- oa.design(nlevels=c(2,3,3,3))
  ## export an html file with legend and two responses
  ## files test.rda and test.html will be written to the current working directory, 
  ##     if they do not exist yet
  export.design(test, response.names=c("pressure", "temperature"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
