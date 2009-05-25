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
export.design(design, response.names="y", 
       path=".", legend=NULL, type="html", OutDec=options("OutDec")$OutDec, ...)
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
  \item{response.names}{ a character vector of response names; the exported file contains a column for 
        each response; default: one response named \code{y}}
  \item{path}{ the path to the directory where the export files are to be stored;
        the default corresponds to the R working directory that can (on some systems) 
        be looked at using \code{getwd()}}
  \item{legend}{ data frame containing legend information; if NULL, 
        the legend is automatically generated from the 
        \code{factor.names} element of \code{design.info(design)}}
  \item{type}{ one of \code{"html"} or \code{"csv"} }
  \item{OutDec}{ decimal separator for the output file; one of \code{"."} or \code{","}; 
        the default is the option setting in the R options; this option also directs  
        whether \code{write.csv} or \code{write.csv2} is used and is very important for 
        usability of the exported files e.g. with Excel}
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
   Function \code{export} uses functions \code{write.csv}, \code{write.csv2} or \code{html} 
   for creating an exported version of the design that is usable outside of R. 
   If \code{type="csv"}, the design is exported as a csv-file, otherwise as an html-file. 
   The csv-file contains the data frame itself only, the html file contains the data frame 
   followed by the legend to the right of the data frame. The html file uses row coloring in 
   order to prevent mistakes by mixing up rows. If the OutDec option is correct for the 
   current computer, the html file can be opened in Excel. 
   
   In addition to the html- or csv file, an R workspace containing just the design object 
   is also output under the same name, with ending rda; if called from the R commander plugin, 
   this workspace additionally contains the menu settings of the design generating menu. 
   

}
\value{The functions are used for their side effects and do not generate a result.
}
\references{ 
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ ~~See Also \code{FrF2}, \code{\link{oa.design}}, \code{pb} }
\examples{
  ## six 2-level factors
  test <- oa.design(nlevels=c(2,3,3,3))
  ## export an html file with legend and two responses
  export.design(test, response.names=c("pressure", "temperature"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
