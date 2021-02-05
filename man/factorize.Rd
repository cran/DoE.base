\name{factorize}
\alias{factorize.factor}
\alias{factorize.design}
\alias{factorize.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Factorize integer numbers and factors
}
\description{
Methods to factorize integer numbers into primes or factors 
into pseudo factors with integer numbers of levels
}
\usage{

\method{factorize}{factor}(x, name = deparse(substitute(x)), extension = letters,
    drop = FALSE, sep = "", ...)
\method{factorize}{design}(x, extension = letters, sep = ".", long=FALSE, ...)
\method{factorize}{data.frame}(x, extension = letters, sep = ".", long=FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{factor\cr
           OR data frame of class \code{design}
           OR data frame }
  \item{name}{name to use for prefixing the pseudo factors}
  \item{extension}{extensions to use for postfixing the pseudo factors}
  \item{drop}{TRUE: have a vector only in case of just one pseudo factor }
  \item{sep}{separation between name and postfix for pseudo factors}
  \item{long}{TRUE: create a complete matrix of pseudofactors; 
         FALSE: only create the named numbers of levels }
  \item{\dots}{ currently not used}
}

\details{
  These functions are used for blocking full factorials. 
  The method for class \code{factors} is a modification of the analogous method 
  from package \pkg{conf.design}, the other two are convenience versions for designs 
  and data frames.
  
  }
\value{
   All three methods return a matrix of pseudo factors (in case \code{long=TRUE}) 
   or a named numeric vector of numbers of levels of the pseudo factors 
   (for the default \code{long=FALSE}). 
}
\author{
Ulrike Groemping; Bill Venables authored the original of factorize.factor.
}
\note{
There may be conflicts with functions from packages \pkg{conf.design} or \pkg{sfsmisc}.
}

\seealso{
The function \code{\link[conf.design]{factorize}} from package \pkg{conf.design}, \cr
the function \code{factorize} from package \pkg{sfsmisc} (no link provided, 
in order to avoid having to include \pkg{sfsmisc} in Suggests).
}
\examples{
factorize(12)
factorize(c(2,2,3,3,6))
factorize(fac.design(nlevels=c(2,2,3,3,6)))
unlist(factorize(c(2,2,3,3,6)))
factorize(undesign(fac.design(nlevels=c(2,2,3,3,6))))
}

\keyword{ array }
\keyword{ design }
