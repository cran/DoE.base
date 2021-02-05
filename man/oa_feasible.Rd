\name{oa_feasible}
\alias{oa_feasible}
\title{Function to Check Whether an Array of Specified Strength Might Exist
}
\description{
The function checks necessary conditions for the existence of an array of specified strength
}
\usage{
oa_feasible(nruns, nlevels, strength = 2, verbose=TRUE)
}
\arguments{
  \item{nruns}{positive integer, number of rows}
  \item{nlevels}{vector of positive integers: its length determines the number of columns, the elements determine the numbers of levels for each column}
  \item{strength}{positive integer (default 2), not larger than the length of \code{nlevels} requested strength of array; 1+strength is the resolution}
  \item{verbose}{logical; if TRUE, reason for outcome is printed}
}
\details{
The function uses several known bounds and necessary divisibility requirements on \code{nruns} for checking \emph{potential} feasibility of an array of the requested strength. It is checked that \code{nruns} is a multiple of the LCM of the run sizes of unreplicated full factorials of all sets of \code{strength} factors and that Rao's bound is fulfilled (the simplest one for strength 2 arrays being that \code{nruns} is larger than the sum of the main effect degrees of freedom; formulae available in Hedayat et al. 1999 Theorem 2.1 for pure levels and Diestelkamp 2004 Theorem 3.1 for mixed levels). For pure level designs, the Bush bounds and Bose/Bush bounds are implemented (Hedayat et al., Theorems 2.8, 2.11 and 2.19).
Furthermore, Bierbrauer's bound (Diestelkamp 2004 Theorems 2.1 and 2.2) is implemented for pure and mixed level designs; note that the mixed level formula has been applied for large \code{strength} values only, because the proof of Diestelkamp is valid only for these (contrary to what is claimed in the paper). For pure 2-level-designs, the bound from Bierbrauer et al. (1999) is also implemented. All these are necessary but not a sufficient conditions for the existence of an orthogonal array of the requested strength.

The implemented bounds have been verified against selected scenarii from Tables 12.1 to 12.3 of Hedayat, Sloane and Stufken 1999. These tables detect further infeasibilities, since they incorporate detailed research results for specific scenarii, contrary to this function which only checks straightforward explicit bounds.
}
\value{
A logical; if FALSE, an OA is infeasible; if TRUE, an OA \emph{might} be feasible.
}
\references{
Bierbrauer, J., Gopalakrishnan, K. and Stinson, D.R. (1999). 
   Orthogonal Arrays, Resilient Functions, Error Correcting Codes and Linear Programming Bounds. 
   Working paper (expanded and revised version of a published extended abstract of the same authors). 
   \url{https://pages.mtu.edu/~jbierbra/}.

Diestelkamp, W. (2004). Parameter inequalities for orthogonal arrays with mixed levels.
   \emph{Designs, Codes and Cryptography} \bold{33}, 187-197.

Hedayat, S., Sloane, N.J.A. and Stufken, J. (1999). Orthogonal Arrays. Springer, New York.
}
\author{
Ulrike Groemping
}

\seealso{
See also function \code{\link[DoE.base]{show.oas}} of package \pkg{DoE.base} for orthogonal arrays catalogued in that package.
}
\examples{
## strength 2 equal to resolution 3 is the default
## pure level examples (function checks criteria in the order listed here)
oa_feasible(51, rep(5,7))
   ## nruns not divisible by 5^2
oa_feasible(1024, rep(2,14), strength=7)
   ## violates Bierbrauer et al.s bound for 2-level
oa_feasible(6561, rep(3,11), strength=8)
   ## violates Bierbrauer's bound for pure level
oa_feasible(25, rep(5,7))
   ## violates Rao's bound for pure level
oa_feasible(256,rep(4,7), 4)
   ## violates Bush bound (checked for pure level only)
oa_feasible(54, rep(3,26))
   ## violates Bose/Bush bound (checked for pure level only)
oa_feasible(25, rep(5, 12), strength = 1)
   ## feasible; but do not try to optimize (5^12 integer variables!!!)
oa_feasible(243, rep(3,11), strength = 4)
   ## strength 4 design that strictly attains the Rao bound for pure level

## mixed level examples (function checks criteria in the order listed here)
oa_feasible(25, c(rep(5,6),4))
   ## too few df for main effects (special case of Rao's bound)
oa_feasible(100, c(rep(5,6),4), 5)
   ## violates Diestelkamps mixed level version of Bierbrauer's bound
               ## (also violates Rao's bound, but this is checked earlier)
oa_feasible(100, c(rep(5,7),4), 3)
   ## violates Rao's bound for mixed level, strength 3
oa_feasible(100, c(rep(5,7),4), 4)
   ## violates Rao's bound for mixed level, even strength
oa_feasible(100, c(rep(5,7),4), 5)
   ## violates Rao's bound for mixed level, general odd strength
oa_feasible(50, c(2,rep(5,12)))
   ## does not violate any bound, although the pure level portion
   ## violates the Bose/Bush bound
   ## for almost pure level: also check pure level portions!

oa_feasible(24, c(2,4,3,4))
   ## violates divisibility by the LCM of all products of pairs
oa_feasible(48, c(2,4,3,4,2))
   ## TRUE and indeed feasible
}
\keyword{ array }% use one of  RShowDoc("KEYWORDS")
\keyword{ design }% use one of  RShowDoc("KEYWORDS")
