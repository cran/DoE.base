\name{SN}
\alias{SN}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for the signal-to-noise ratio 10 * log10(mean^2/var) }
\description{
  Function for the signal-to-noise ratio 10 * log10(mean^2/var)
}
\usage{
SN(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a data vector to take the S/N ratio over }
}
\details{
   Taguchi proposes three different versions of S/N-ratio. 
   In line with Box, Hunter and Hunter (2005), only the one 
   for target-optimization is given here, as it is invariant 
   against linear transformation.

}
\value{a number (10 * log10(mean^2/var))
}
\references{ 
    Box G. E. P, Hunter, W. C. and Hunter, J. S. (2005) 
    \emph{Statistics for Experimenters, 2nd edition}.
    New York: Wiley.
    
}
\author{ Ulrike Groemping }
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See also \code{\link{aggregate.design}}; 
   function \code{SN} has been developed for use with aggregating parameter designs}
\examples{
x <- rexp(10)
SN(x)
10 * log10(mean(x)^2/var(x))
20 * log10(mean(x)/sd(x))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line