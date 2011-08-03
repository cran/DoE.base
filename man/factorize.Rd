\name{factorize}
\alias{factorize}
\alias{factorize.default}
\alias{factorize.factor}
\alias{factorize.design}
\alias{factorize.data.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Factorize integer numbers and factors
}
\description{
Generic and methods to factorize integer numbers into primes or factors 
into pseudo factors with integer numbers of levels
}
\usage{

factorize(x, ...)
\method{factorize}{default}(x, ...)
\method{factorize}{factor}(x, name = deparse(substitute(x)), extension = letters,
    drop = FALSE, sep = "", ...)
\method{factorize}{design}(x, extension = letters, sep = ".", long=FALSE, ...)
\method{factorize}{data.frame}(x, extension = letters, sep = ".", long=FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{integer number or vector of numbers to be factorized into primes \cr
           OR factor\cr
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
  They are unchanged (default method) or modified (factor method) versions of 
  functions from package \pkg{conf.design}, or convenience versions for designs 
  and data frames.
  
  If in doubt whether the correct methods are used, use {DoE.base::factorize}.
  }
\value{
   The default function returns a vector or a list of vectors. 
   The other methods return a data frame of pseudo factors, 
   or a numeric vector. 
}
\author{
Bill Venables authored the originals, modifications and extensions by Ulrike Groemping
}
\note{
There may be conflicts with functions from packages \pkg{conf.design} or \pkg{sfsmisc}.
}

\seealso{
The function from package \pkg{conf.design}: \code{\link[conf.design]{factorize}}\cr
The function from package \pkg{sfsmisc}: \code{\link[sfsmisc]{factorize}}
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
