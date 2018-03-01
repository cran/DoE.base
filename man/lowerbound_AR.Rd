\name{lowerbound_AR}
\alias{lowerbound_AR}
\alias{lowerbounds}
\alias{lowerbound_chi2}
\title{
Function to Calculate a Lower Bound for A_R and Internal Auxiliary Functions
}
\description{
The functions serve the calculation of lower bounds for the worst case confounding. lowerbound_AR is intended for direct use, lowerbounds and lowerbound_chi2 are internal functions.
}
\usage{
lowerbound_AR(nruns, nlevels, R, crit = "total")
lowerbounds(nruns, nlevels, R)
lowerbound_chi2(nruns, nlevels)
}
\arguments{
  \item{nruns}{
positive integer, the number of runs
}
  \item{nlevels}{
vector of positive integers, the numbers of levels for the factors
}
  \item{R}{
positive integer, the resolution of the design
}
  \item{crit}{
\code{"total"} or \code{"worst"}; if \code{"total"},
a bound for the overall A_R (sum of the results from \code{lowerbounds}) is calculated;
otherwise, a bound for the largest individual contribution from an R factor set is calculated}
}
\details{
Function \code{lowerbounds} provides (integral) bounds on \eqn{n^2 A_R} (with \eqn{n}=\code{nruns}) according to Groemping and Xu (2014) Theorem 5 for all R factor sets. If the number of runs permits a design with resolution larger than R, the value(s) will be 0. For resolution at least III, the result of function \code{lowerbound_AR} is the sum (\code{crit="total"}) or maximum (\code{crit="worst"}) of these individual bounds, divided by the square of the number of runs.

For resolution II and \code{crit="total"}, function \code{lowerbound_chi2} implements the lower bound \emph{B} on chi^2 which was provided in Lemma 2 of Liu and Lin (2009). For supersaturated resolution II designs, this bound is is usually sharper than the one obtained on the basis of Grömping and Xu (2014). Due to the relation between \eqn{A_2} and \eqn{\chi^2}{chi^2} that is stated in Groemping (2017) (summands of \eqn{A_2} are an \emph{n}th of a \eqn{\chi^2}{chi^2}, with \eqn{n}=\code{nruns}), this bound can be easily transformed into a bound for \eqn{A_2}; this relation is also used to slightly sharpen the bound \emph{B} itself: \eqn{n^2 \cdot A_2}{n^2 A_2} must be integral, which implies that \emph{B} can be replaced by \code{ceiling(nruns*B)/nruns}, which is applied in function \code{lowerbound_chi2}. Function \code{lowerbound_AR} increases the lower bound on \eqn{A_2} accordingly, if \code{lowerbound_chi2} provides a sharper bound than the sum of the elements returned by functioni \code{lowerbounds}.
}
\value{
\code{lowerbound_AR} returns a lower bound for the number of words of length \code{R} (either total or worst case), \cr
\code{lowerbounds} returns a vector of lower bounds for individual \code{R} factor sets on a different scale (division by \code{nruns^2} needed for transforming this into the contributions to words of length R),\cr
and function \code{lowerbound_chi2} returns a lower bound on the \eqn{\chi^2}{chi^2} value which can be used as a quality criterion for supersaturated designs.
}
\references{
Groemping, U. and Xu, H. (2014). Generalized resolution for orthogonal arrays. \emph{The Annals of Statistics} \bold{42}, 918-939.

Groemping, U. (2017). Frequency tables for the coding-invariant quality assessment of factorial designs. \emph{IISE Transactions} \bold{49}, 505-517.

Liu, M.Q. and Lin, D.K.J. (2009). Construction of Optimal Mixed-Level Supersaturated Designs. \emph{Statistica Sinica} \bold{19}, 197-211.

}
\author{
Ulrike Groemping
}

\seealso{
See also \code{\link[DoE.base]{oa_feasible}}.
}
\examples{
lowerbound_AR(24, c(2,3,4,6),2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }% use one of  RShowDoc("KEYWORDS")
\keyword{ array }% __ONLY ONE__ keyword per line
