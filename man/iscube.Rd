\name{iscube}
\alias{iscube}
\alias{isstar}
\alias{pickcube}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to isolate cube points from 2-level fractional factorial design with center and / or star points
}
\description{
These functions identify the positions for cube points or star points 
and can reduce a central composite design to its cube portion (with 
center points).
}
\usage{
iscube(design, ...)
isstar(design, ...)
pickcube(design, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{
  a data frame of class design that contains a 2-level fractional factorial 
  (regular or non-regular) or a central composite design.
  }
  \item{\dots}{
   currently not used}
}
\details{
  Function \code{iscube} provides a logical vector that is TRUE for cube points 
  and FALSE for center points and star points. Its purpose is to enable use of simple functions 
  for \dQuote{clean} 2-level fractional factorials like \code{MEPlot} or \code{DanielPlot}. 
  
  Function \code{isstar} provides a logical vector that is TRUE for the star block 
  (including center points) of a central composite design.
  
  Function \code{pickcube} reduces a central composite design (type \code{ccd}) 
  to its cube block, including center points. This function is needed, if a CCD 
  has been created in one go, but analyses are already required after conducting 
  the cube portion of the design (and these perhaps even prevent the star portion 
  from being run at all).
}
\value{
  \code{iscube} and \code{isstar} each return a logical vector (cf. Details section).
  
  \code{pickcube} returns a data frame of class design with 
  \code{type} \code{FrF2.center} or \code{FrF2}.
  
}
\references{

    Montgomery, D.C. (2001). \emph{Design and Analysis of Experiments (5th ed.)}. Wiley, New York.

}
\author{
Ulrike Groemping
}
\note{
    The functions have not been tested for central composite designs for which 
    the cube portion itself is blocked.
}
\section{Warning}{
    For version 0.22-8 of package \pkg{DoE.base}, function \code{iscube} returned a wrong result 
    without warning, when applied to an old version CCD design 
    (before DoE.wrapper, version 0.8-6 of Nov 15 2011). \cr
    Since version 0.23 of package \pkg{DoE.base}, the function works on old designs, 
    except for blocked or replicated versions; for these, an error is thrown.)
}

\seealso{
See also as \code{\link[FrF2]{pb}}, \code{\link[FrF2]{FrF2}}, \code{\link[DoE.wrapper]{ccd.design}}
}
\examples{
  ## purely technical example, not run because FrF2 not loaded
  \dontrun{
  plan <- FrF2(16,5, factor.names=c("one","two","three","four","five"), ncenter=4)
  iscube(plan)
  plan2 <- ccd.augment(plan)
  iscube(plan2)
  isstar(plan2)
  pickcube(plan2)
  }
   }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
\keyword{ array }% __ONLY ONE__ keyword per line
