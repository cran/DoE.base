\name{halfnormal}
\alias{halfnormal}
\alias{halfnormal.default}
\alias{halfnormal.lm}
\alias{halfnormal.design}
\alias{null.check}
\alias{orth.check}
\alias{ME.Lenth}
\alias{CME.LW98}
\alias{CME.EM08}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creation of half normal effects plots and numeric methods for
significance assessment
}
\description{
Generic function and methods for creating half normal effects plots
}
\usage{
halfnormal(x, ...)
\method{halfnormal}{default}(x, labs=names(x), codes = NULL, pch = 1, cex.text = 1,
   alpha = 0.05, xlab = "absolute effects", large.omit = 0, plot=TRUE,
   crit=NULL, ...)
\method{halfnormal}{lm}(x, labs = NULL, code = FALSE, pch = NULL, cex.text = 1,
   alpha = 0.05, xlab = "absolute coefficients", large.omit = 0, plot=TRUE,
   keep.colons = !code, ME.partial = FALSE,
   external.pe = NULL, external.center = FALSE, contr.center = "contr.poly",
   pch.set = c(1, 16, 8), scl = NULL, method="Lenth",
   legend=code, err.points=TRUE, err.line=TRUE, linecol="darkgray", linelwd=2,
   ...)
\method{halfnormal}{design}(x, response = NULL, labs = NULL, code = FALSE, pch = NULL,
   cex.text = 1,
   alpha = 0.05, xlab = "absolute coefficients", large.omit = 0, plot=TRUE,
   keep.colons = !code, ME.partial = FALSE,
   external.pe = NULL, external.center = FALSE, contr.center = "contr.poly",
   pch.set = c(1, 16, 8), scl = NULL, method="Lenth",
   legend=code, err.points=TRUE, err.line=TRUE, linecol="darkgray", linelwd=2,
   ...)

ME.Lenth(b, simulated=TRUE, alpha=NULL)
CME.LW98(b, sterr, dfe, simulated=TRUE, alpha=NULL)
CME.EM08(b, sterr, dfe, simulated=TRUE, weight0=5, alpha=NULL)

}
\arguments{
  \item{x}{a numeric vector of effects, a linear model from experimental data,
     or an experimental design of class \code{design}}
  \item{labs}{effect labels;\cr
     default labels: for the default method, names of the vector x, or \code{b1}, \code{b2}, ... for
     unnamed vectors; for classes \code{design} or \code{lm} taken from the linear model}
  \item{codes}{a vector with a code for each effect; the default \code{NULL}
     uses the \code{labs} values}
  \item{code}{a logical; \code{TRUE} implies that factor letters
     are used instead of factor codes, and that the default for default for
     \code{keep.colons} is changed to \code{FALSE}}
  \item{pch}{plot symbol; \code{NULL}, a number or a vector of plot symbol numbers
    or the same length as the effects in x;\cr
    in the default method, a single number (default 1) implies that the given
    plotting symbol is used for for all points;\cr
    for the other methods, the default \code{NULL} or a single number
    implies that pch.set is employed for lack of fit or pure error contrast points;\cr
		for the non-default methods, a vector-valued \code{pch} will only rarely be useful
		(see Details section)}
  \item{cex.text}{factor to hand to \code{cex} argument for point labeling with
    function \code{text} and margin annotations with function \code{mtext};
    for \code{mtext}, it is multiplied with \code{par("cex")}, in order to
    obtain the same size for point labels and the margin annotations.
    }
  \item{alpha}{number between 0 and 1: the significance level for labelling effects;\cr
    for functions \code{ME.Lenth}, \code{CME.LW98} and \code{CME.EM08},
    \code{alpha} can also be \code{NULL} or a numeric vector;
    for using the simulated critical bounds, all elements of alpha must be in
    0.01,0.02,...,0.25}
  \item{xlab}{character string: the x axis label}
  \item{plot}{logical; if \code{FALSE}, plotting is suppressed}
  \item{large.omit}{integer number of largest effects to be omitted from plot
     and calculations in order to concentrate on the smaller effects;
     (note that the significance is also re-assessed; if that is undesirable,
     an explicit \code{crit} value can be specified from all coefficients,
     or \code{alpha} can be adjusted to reflect the same significant effects
     as with all coefficients)}
  \item{crit}{default \code{NULL}; not meant for the end user; allows the \code{method}
     option for linear models and experimental designs to choose alternatives
     to Lenth's method}
  \item{keep.colons}{if \code{TRUE}, the automatic effect labels contain colons for interactions}
  \item{ME.partial}{if \code{TRUE}, partial aliasing among main effects is
     permitted and will be orthogonalized away}
  \item{external.pe}{numeric vector with values from outside the experimental data
     for use in estimating the error variance}
  \item{external.center}{if \code{TRUE}, external values from \code{external.pe}
    are taken as center point values, and a nonlinearity check contrast is estimated
    from them}
  \item{contr.center}{contrasts used for external center points;
    \code{contr.poly} or \code{contr.XuWu}}
  \item{pch.set}{plot symbols used for experimental effects, automatically
    determined lack of fit contrasts or pure error effects}
  \item{scl}{squared column length to which the model matrix is normalized;
    default: number of experimental runs}
  \item{method}{the default \code{"Lenth"} applies Lenth's method to
    the combined set of effects including error contrasts (if any);
    the alternatives \code{"LW98"} or \code{"EM08"}
    apply the methods proposed by Larntz and Whitcomb (1998) or
    Edwards and Mee (2008) with weight0=5; if there is no pure error,
    method \code{"Lenth"} is always used, with a warning}
  \item{legend}{squared column length to which the model matrix is normalized;
    default: number of experimental runs}
  \item{err.points}{logical, default \code{TRUE}; determines, whether pure error
    points are added to the plot (lack-of-fit points are always added)}
  \item{err.line}{logical, default \code{TRUE}; determines, whether null line
    is added to the plot in case pure error points are available}
  \item{linecol}{specifies the color for the null line, if applicable}
  \item{linelwd}{specifies the width of the null line, if applicable}
  \item{response}{response for which the plot is to be created}
  \item{\dots}{further options to be handed to the \code{plot} function;\cr
      among these,
      if options \code{col} and/or \code{cex} have an element for each effect,
      these are used in the expected order (first color refers to first element
      of x and so forth); this change was introduced in version 0.26-2 and
      causes an appropriate reordering in the actual plot function.}
  \item{b}{vector of coefficients}
  \item{simulated}{logical; if \code{FALSE}, the original critical values from
   Lenth 1989 are used, otherwise the methods use stored simulated values from a million
   simulation runs for significance levels of 0.01, 0.02, ..., to 0.25}
  \item{sterr}{a standard error for \code{b}, obtained from (a few, \code{dfe})
    pure error degrees of freedom; the methods by Larntz and Whitcomb (1998) and
    Edwards and Mee (2008) combine this with Lenth's method}
  \item{dfe}{the number of pure error degrees of freedom on which
    \code{sterr} was based}
  \item{weight0}{a tuning parameter for the method by Edwards and Mee 2008;
    Edwards and Mee recommend to set this to 5}
}
\details{
Function \code{halfnormal} creates half normal effects plots with automatic
effect labelling according to significance. It also prints the significant
effects and creates an output object that contains only the vector if signifcant
effects (for the default method) or in addition several further components (see
section "Value"). Note: The methods for linear models and experimental designs plot
absolute coefficients from a linear model (i.e. in case of 2-level factors with
the usual -1/+1 coding, half of the absolute effects).

The methods for linear models and experimental designs allow to automatically
create lack of fit and pure error contrasts to also be included in the plot,
following an orthogonalization strategy similar to Section 5 in Langsrud (2001).
Furthermore, they handle factors with more than two levels, and they handle partially
aliased effects by orthogonalizing out previous effects from later effects in
the model order (similar to what Langsrud 2001 proposed for multiple response
variables); thus, the plots are order dependent in case of partial aliasing.
The more severe the partial aliasing, the more drastic the difference between the
different effect orders. Per default, main effects are required to be
orthogonal; this can be changed via option \code{ME.partial}. This is discussed in
detail in Groemping (2015).

The functions \code{ME.Lenth}, \code{CME.LW98} and \code{CME.EM08} yield standard
error estimates and critical values. For alpha in 0.01, 0.02, ..., 0.25,
function \code{ME.Lenth} uses simulated critical values from a large number of
simulations (1000000), if the number of effects is in 7 to 143.
Functions \code{CME.LW98} and \code{CME.EM08} currently simulate critical values
from 10000 simulation runs on the fly.
If no simulated values are available or simulation has been switched off,
the half-normal plotting routines will use the conservative t-values proposed by
Lenth (1989) (\code{ME.Lenth}) or Larntz and Whitcomb (\code{CME.LW98} and \code{CME.EM08}).

Vector valued entries for \code{pch}, \code{col} and \code{cex} are handled
very specifically for the class \code{lm} and class \code{design} methods:
They make the most sense if the model is already saturated:
If no pure error effects have been automatically calculated, effects whose \code{pch}
is identical to the third element of \code{pch.set} will be treated as pure error effects;
this allows to manually code these effects.\cr
Generally, vector-valued \code{pch} (and \code{col} and \code{cex}) must have as
many elements as the final coefficients vector after augmenting the coefficients;
the coefficient vector carries first the experimental coefficients, then the automatically
calculated lack-of-fit coefficients, then the automatically calculated pure error
coefficients, then lack-of fit coefficients from external replications,
and finally the pure error coefficients from external replications. Even for
\code{err.points=FALSE}, entries for all these elements are needed. The value for
\code{pch} determines, which coefficients are considered pure error.
}
\value{
The default method for \code{halfnormal} visibly returns a character vector of significant
effects only. The methods for linear models and experimental designs invisibly return lists
of nine elements:

\tabular{ll}{
\code{coef} \tab contains the estimated coefficients\cr
\code{mm}   \tab contains the model matrix \cr
            \tab after adjustment to equally scaled independent effects\cr
\code{mod.effs} \tab the effects that are part of the model \cr
\code{res}    \tab list that indicates the effects (named vector of position numbers)\cr
              \tab that were projected out from any particular model effect (element name) \cr
\code{LCs}    \tab contains the coefficients of the linear combinations\cr
              \tab taken from the residuals after projecting out the effects\cr
              \tab listed in \code{res} from the original model matrix columns.\cr
              \tab Where \code{LCs} elements are \code{NULL}, \cr
              \tab the original effect completely disappeared \cr
              \tab because of complete confounding with previous effects.\cr
\code{alpha}  \tab contains the significance level\cr
\code{method} \tab contains the method of significance assessment\cr
\code{signif} \tab is a character vector of significant effects\cr
\code{pchs}   \tab is a numeric vector of plot character identifiers\cr
}

The functions \code{ME.Lenth}, \code{CME.LW98} and \code{CME.EM08} each
return lists of length 4 with an estimate for s0, PSE, ME and SME for Lenth's
method or their respective modifications for the other two methods (called
s0, CPSE, CME and CSME for \code{CME.LW98} and Cs0, CPSE, CME and CSME for
\code{CME.EM08}). The length of the (C)ME and (C)SME components depends on
the length of alpha (default: 25 critical values for alphas from 0.25 to 0.01).
}
\references{
    Daniel, C. (1959)
    Use of Half Normal Plots in Interpreting Two Level Experiments.
    \emph{Technometrics} \bold{1}, 311--340.

    Daniel, C. (1976)
    \emph{Application of Statistics to Industrial Experimentation.}
    New York: Wiley.

    Edwards, D. and Mee, R. (2008) Empirically Determined p-Values for Lenth t Statistics.
    \emph{Journal of Quality Technology} \bold{40}, 368--380.

    Groemping, U. (2015) Augmented Half Normal Effects Plots in the Presence of a Few Error Degrees of Freedom. \emph{Quality and Reliability Engineering International} \bold{31}, 1185--1196. \url{https://prof.bht-berlin.de/fileadmin/prof/groemp/downloads/Groemping2015_QREI_AcceptedManuscript.pdf}

    Langsrud, O. (2001) Identifying Significant Effects in Fractional Factorial
    Multiresponse Experiments. \emph{Technometrics} \bold{43}, 415--424.

    Larntz, K. and Whitcomb, P. (1998)
    Use of replication in almost unreplicated factorials.
    Manuscript of a presentation given at the 42nd ASQ Fall Technical
    conference in Corning, New York.
    Downloaded 4/26/2013 at \url{https://cdnm.statease.com/pubs/use-of-rep.pdf}.

    Lenth, R.V. (1989) Quick and easy analysis of unreplicated factorials.
    \emph{Technometrics} \bold{31}, 469--473.

}
\author{
Ulrike Groemping, Berliner Hochschule fuer Technik}

\note{If someone worked out how to modify symbol colors (option \code{col})
      and/or sizes (option \code{cex}) for a version before 0.26-2,
      version 0.26-2 will mess up the order of the symbol colors and/or sizes.
      The benefit: colors and symbol sizes can now be specified in the natural
      order, see description of the \dots argument.}

\seealso{See also \code{\link[FrF2]{DanielPlot}} for (half) normal plots
    of 2-level fractional factorial designs without partial aliasing
    and ignoring any residual degrees of freedom}

\examples{
### critical values
b <- rnorm(12)
ME.Lenth(b)
ME.Lenth(b)$ME
ME.Lenth(b, alpha=0.22)
ME.Lenth(b, alpha=0.123)
ME.Lenth(b, alpha=0.12)
ME.Lenth(rnorm(144), alpha=0.1)
(mel <- ME.Lenth(b, alpha=0.1))
## assuming an external effect standard error based on 3df
\dontrun{CME.EM08(b, 0.1, 3, alpha=0.1)}
         ## does not run for saving CRAN check time
         ## much smaller than Lenth, if external
         ## standard error much smaller than s0 (see mel)

### Half normal plots
## the default method
halfnormal(rnorm(15), labs=paste("b",1:15,sep=""))
b <- c(250, 8,7,6, rnorm(11))
halfnormal(b, labs=paste("b",1:15,sep=""))
halfnormal(b, labs=paste("b",1:15,sep=""), large.omit=1)

## the design method, saturated main effects design
plan <- oa.design(L12.2.11)
halfnormal(add.response(plan,rnorm(12)))

## the design method, saturated main effects design,
## partial aliasing due to a missing value
y <- c(NA, rnorm(11))
## the following line would yield an error, because there is even
## complete aliasing among main effects:
\dontrun{halfnormal(lm(y~., add.response(plan, y)), ME.partial=TRUE)}
## this can only be helped by omitting a main effect from the model;
## afterwards, there is still partial aliasing,
## which must be explicitly permitted by the ME.partial option:
halfnormal(lm(y~.-D, add.response(plan, y)), ME.partial=TRUE)

## the linear model method
yc <- rnorm(12)
## partial aliasing only
halfnormal(lm(yc~A+B+C+D+E+F+G+H+J+A:B, plan))
## both partial (A:B) and complete (E:F) aliasing are present
halfnormal(lm(yc~A+B+C+D+E+F+G+H+J+A:B+E:F, plan))
## complete aliasing only because of the missing value in the response
halfnormal(lm(y~A+B+C+D+E+F+G+H+J+A:B+E:F, plan),ME.partial=TRUE)
## omit a large dominating effect

halfnormal(lm(y~A+B+C+D+E+F+G+H+J+A:B+E:F, plan),ME.partial=TRUE)


## a regular fractional factorial design with center points
y20 <- rnorm(20)
\dontrun{halfnormal(lm(y20~.^2, FrF2(16,7,ncenter=4)))}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
\keyword{ array }
