\name{GWLP}
\alias{GWLP}
\alias{GWLP.design}
\alias{GWLP.default}
\alias{levels.no}
\alias{ham}
\alias{Kraw}
\alias{Choose}
\alias{distDistmix}
\alias{levelmix}
\alias{Bprime}
\alias{dualDistmix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions for fast calculation of GWLP }
\description{
  Functions needed for fast calculation of GWLP using the formulae 
  from Xu and Wu (2001)
}
\usage{
GWLP(design, ...)
\method{GWLP}{design}(design, kmax=design.info(design)$nfactors, 
     attrib.out=FALSE, with.blocks = FALSE, digits = NULL, ...)
\method{GWLP}{default}(design, kmax=ncol(design), attrib.out=FALSE, digits = NULL, ...)
Choose(n, k)
Kraw(k,x,n,q)
ham(c1, c2)
levels.no(xx)
levelmix(xx)
distDistmix(code, levm)
Bprime(dists, nmax=5)
dualDistmix(Bprime, nmax=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{ a design, not necessarily of class \code{\link{design}};
      class design properties are exploited by using only factor columns 
      (or factor and block columns, if \code{with.blocks} is \code{TRUE}) }
  \item{kmax}{ the maximum word length requested }
  \item{attrib.out}{ the detail added to the output (see Value section)}
  \item{with.blocks}{ if \code{TRUE}, the block column contributes to 
       the GWLP, otherwise it does not }
  \item{digits}{ the number of decimals to round to; \code{NULL} prevents rounding }
  \item{n}{an integer number}
  \item{k}{an integer number}
  \item{x}{an integer number}
  \item{q}{an integer number}
  \item{c1}{a design row}
  \item{c2}{a design row}
  \item{xx}{ a design, not necessarily of class \code{\link{design}} }
  \item{code}{ a design, not necessarily of class \code{\link{design}} }
  \item{levm}{ an object returned by function \code{levelmix} }
  \item{dists}{ the BSep element of an outcome object from \code{distDistmix}, 
      analogous to the B_j1_j2 of p.1072 of Xu and Wu 2001}
  \item{nmax}{set to \code{kmax} in calls by other functions}
  \item{Bprime}{an outcome object from \code{Bprime}, the MacWilliams transform 
     of the distance distribution}
}
\details{
   Function \code{GWLP} is intended for direct use, the others are not. \cr
   Function \code{GWLP} is much faster but also more inaccurate than the 
   function \code{\link{lengths}}, which calculates numbers of words 
   for lengths 2 to 5 only. Note, however, that function \code{\link{lengths}} 
   can be faster for designs with very many rows. 
   
   Function \code{ham} calculates the Hamming distance, function \code{Kraw} 
   the Krawtchouk polynomials, function \code{Choose} differs from the base 
   function \code{\link[base]{choose}} by treatment of negative values \code{n}, 
   functions \code{levels.no} and \code{levelmix} are utilities providing the 
   level information on the design \code{xx}. 
   The functions \code{distDistmix}, \code{Bprime} and \code{dualDistmix} 
   implement formulae from Xu and Wu (2001) for the distance distribution, 
   its MacWilliams transform and the calculation of GWLP from the latter.
}
\value{Only function \code{GWLP} is intended for direct use.

The \code{GWLP} methods output a named vector with the numbers of generalized 
words of lengths zero to \code{kmax}. If \code{attrib.out} is \code{TRUE}, 
this vector comes with the attributes \code{B} and \code{levels.info}, 
the latter documenting the level situation of the design, the former 
the distance distribution B (Xu and Wu 2001).
}
\references{ 
  Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for 
  asymmetrical fractional factorial designs. 
  \emph{Annals of Statistics} \bold{29}, 1066--1077.
}
\author{ Hongquan Xu, Ulrike Groemping }
\note{ This package is currently under intensive development. 
  Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link{lengths}} }
\examples{
GWLP(L18)
GWLP(L18, attrib.out=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
\keyword{internal}