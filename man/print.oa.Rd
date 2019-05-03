\name{print.oa}
\alias{print.oa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Print oa Objects with a Lot of Added Info
}
\description{
The function suppresses printing of voluminous info attached as attributes to oa objects.
}
\usage{
\method{print}{oa}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
the oa object to be printed
}
  \item{\dots}{
further arguments for default print function
}
}
\details{
The function currently removes all attributes except \code{origin}, \code{class}, \code{dim}, \code{dimnames} before printing.\cr
If available, status information from the \code{MIPinfo} attribute is printed.\cr 
Additionally, the names of unusual attributes are printed. 
They can also be printed separately by running \code{names(attributes(x))}; to access an attribute, run \code{attr(x, "MIPinfo")}, for example.
}
\value{
The function is used for its side effects and does not return anything.
}
\author{
Ulrike Groemping
}

\seealso{
See also \code{\link{print.default}} and \code{\link{str}}
}
