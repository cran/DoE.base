\name{contr.FrF2}
\alias{contr.FrF2}
\title{ Contrasts for orthogonal Fractional Factorial 2-level designs }
\description{
  Contrasts for orthogonal Fractional Factorial 2-level designs 
}
\usage{
   contr.FrF2(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ power of 2; number of levels of the factor for which contrasts are 
    to be generated}
}
\details{
  This function mainly supports \code{-1/+1} contrasts for 2-level factors.
  It does also work if the number of levels is a power of 2. 
  For more than four levels, the levels of the factor must be in an appropriate order 
  in order to guarantee that the columns of the model matrix for an FrF2-derived structure 
  are orthogonal.
  }
\value{The function returns orthogonal contrasts for factors with number of levels a power of 2. 
All contrast columns consist of \code{-1} and \code{+1} entries (half of each). 
If factors in orthogonal arrays 
with 2-level factors are assigned these contrasts, the columns of the model matrix 
for the main effects model are orthogonal to each other and to the column for the intercept.
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link[stats]{contrasts}}, \code{\link[FrF2]{FrF2}}, 
      \code{\link{fac.design}}, \code{\link{oa.design}}, \code{\link[FrF2]{pb}} }
\examples{
## assign contr.FrF2 contrasts to a factor
status <- as.factor(rep(c("current","new"),4))
contrasts(status) <- contr.FrF2(2)
contrasts(status)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
