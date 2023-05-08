\name{GWLP_internals}
\alias{levels.no}
\alias{ham}
\alias{Kraw}
\alias{Choose}
\alias{distDistmix}
\alias{levelmix}
\alias{Bprime}
\alias{dualDistmix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Internal functions in support of function GWLP }
\description{
  Internal functions needed for function GWLP
}
\usage{
Choose(n, k)
Kraw(k,x,n,q)
ham(c1, c2)
# levels.no(xx)
levelmix(xx)
distDistmix(code, levm)
Bprime(dists, nmax=5)
dualDistmix(Bprime, nmax=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
   The functions are used in function \code{\link{GWLP}}.
   Function \code{ham} calculates the Hamming distance, function \code{Kraw} 
   the Krawtchouk polynomials, function \code{Choose} differs from the base 
   function \code{\link[base]{choose}} by treatment of negative values \code{n}, 
   functions \code{levels.no} and \code{levelmix} are utilities providing the 
   level information on the design \code{xx} (\code{levels.no(xx)} is commented out 
   in the usage section, because it can be seen as an S3 method for the generic 
   function \code{levels} for objects of a conceivable class \code{no}; 
   therefore its documentation without \code{#} would cause complaints in CRAN checks.) 
   The functions \code{distDistmix}, \code{Bprime} and \code{dualDistmix} 
   implement formulae from Xu and Wu (2001) for the distance distribution, 
   its MacWilliams transform and the calculation of GWLP from the latter.
}
\value{
not documented for these internal functions
}
\references{ 
  Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for 
  asymmetrical fractional factorial designs. 
  \emph{Annals of Statistics} \bold{29}, 1066--1077.
}
\author{ Hongquan Xu, Ulrike Groemping }
\seealso{ See Also \code{\link{GWLP}} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
\keyword{internal}