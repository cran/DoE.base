\name{expansive.replace}
\alias{expansive.replace}
\title{ Expansive replacement for two orthogonal arrays }
\description{
  Expansive replacement for two orthogonal arrays 
}
\usage{
   expansive.replace(array1, array2, fac1 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{array1}{ an orthogonal array, must be a matrix; \cr
      the levels of column \code{fac1} of this array are replaced by the 
      corresponding runs of \code{array2}; 
      they must be numbered with integers starting with 1}
  \item{array2}{ an orthogonal array, must be a matrix \cr
      this array is replaced for a column in \code{array1}  }
  \item{fac1}{if \code{NULL}, the first suitable column of \code{array1} 
      is replaced with \code{array2};\cr
      alternatively, a suitable column number for \code{array1} can be specified}
}
\details{
  This function mainly is meant for combining two orthogonal arrays
  via what Kuhfeld (2009) calls expansive replacement. 
  }
\value{The function returns an object of classes \code{oa} and \code{matrix}, 
which can be used in function \code{\link{oa.design}}. 
Whether or not the object is an orthogonal array depends on the choice of suitable input 
arrays by the user. The properties of the resulting array can e.g. be inspected with functions 
\code{\link{GWLP}} or \code{\link{GRind}}.
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link{oacat}} }
\references{
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
}
\examples{
myL24.2.14.6.1 <- expansive.replace(L24.2.12.12.1, L12.2.2.6.1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
