\name{GRind}
\alias{GR}
\alias{GRind}
\alias{print.GRind}
\alias{SCFTs}
\title{
Functions for calculating generalized resolution, average R-squared values and 
squared canonical correlations, and for checking design regularity
}
\description{
Function GR calculates generalized resolution, function GRind calculates
more detailed generalized resolution values, squared canonical correlations
and average R-squared values, the print method for class GRind appropriately prints the
detailed GRind values. Function SCFTs calculates squared canonical correlations for 
factorial designs. SCFTs includes more projections than GRind (all full resolution 
projections or even all projections) and decides on regularity of the design, 
based on a conjecture.
}
\usage{
GR(ID, digits=2)
GRind(design, digits=3, arft=TRUE, scft=TRUE, cancors=FALSE, with.blocks=FALSE)
SCFTs(design, digits = 3, all = TRUE, resk.only = TRUE, kmin = NULL, kmax = ncol(design), 
   regcheck = FALSE, arft = TRUE, cancors = FALSE, with.blocks = FALSE)
\method{print}{GRind}(x, quote=FALSE,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{an orthogonal array, either a matrix or a data frame; need not be of class \code{oa};
      can also be a character string containing the name of an array listed in data frame \code{\link{oacat}}}
  \item{digits}{number of decimal points to which to round the result}
  \item{design}{a factorial design. This can either be a matrix or a data frame
      in which all columns are experimental factors, or a special data frame
      of class \code{\link[DoE.base:class-design]{design}}, which may also include response data.\cr
      In any case, the design should be a factorial design;
      the functions are not useful for quantitative designs (like e.g. latin hypercube samples).
      }
  \item{arft}{logical indicating whether or not the average $R^2$ frequency table
       (ARFT, see Grömping 2013) is to be returned
       }
  \item{scft}{logical indicating whether the squared canonical correlation frequency
       table (SCFT, see Grömping 2013) is to be returned
       }
  \item{cancors}{logical indicating whether individual canonical correlations
       are to be returned (see Groemping 2013). These will not be needed
       for normal use of the package.
       }
  \item{with.blocks}{a logical, indicating whether or not an existing block factor
      is to be included into word counting. This option is ignored if \code{design}
      is not of class \code{design}.\cr
      Per default, an existing block factor is ignored.\cr
      For designs without a block factor, the option does not have an effect.\cr
      If the design is blocked, and \code{with.blocks}
      is \code{TRUE}, the block factor is treated like any other factor in terms of word counting.}
  \item{all}{ logical; decides whether or not to consider projections of more than R~factors, where R denotes the design resolution}
  \item{resk.only}{logical; if \code{all} is \code{TRUE}, should only full resolution projections be considered? 
              Choosing \code{FALSE} may cause very long run times.}
  \item{kmin}{integer; purpose is to continue an earlier run with additional larger projections}
  \item{kmax}{integer; limit on projection sizes to consider}
  \item{regcheck}{logical; is the purpose a regularity check? If \code{TRUE}, the function stops after the first projection size 
              that included squared canonical correlation different from 0 or 1.}
  \item{x}{a list of class \code{GRind}, as created by function \code{GRind}}
  \item{quote}{a logical indicating whether character values are quoted}
  \item{\dots}{further arguments to function \code{print}}
}
\details{
   Functions \code{GR}, \code{GRind}, and \code{SCFTs} work for factors only and are not intended
   for quantitative variables. Nevertheless it is possible to apply them to class \code{\link{design}} plans with quantitative
   variables in them in some situations.

   Function \code{GR} calculates the generalized resolution according to Deng and Tang (1999)
   for 2-level designs or a generalization thereof according to Groemping (2011) and
   Groemping and Xu (2014) for general
   orthogonal arrays. It returns a value between 3 and 5, where the numeric value 5 stands for
   \dQuote{at least 5}. Roughly, generalized resolution measures the closeness of a design
   to the next higher resolution (worst-case based, e.g. one completely aliased triple of
   factors implies resolution 3).

   Function \code{GRind} (newer than \code{GR}, and recommended) calculates the generalized
   resolution, together with factor wise generalized resolution values, squared canonical correlations
   and average R-squared values, as introduced in Groemping and Xu (2014) and detailed in Grömping (2013).
   The print method for class \code{Grind} objects prints the individual factor components of GRind.i such that they
   do not mislead:
   Because of the shortest word approach for GR, SCFT and ARFT, a GRind.i component
   can be at most one larger than the resolution. For example, if GR is 3.5 so that the
   resolution is 3, the largest possible numeric value of a GRind.i component is 4, but it means ">=4".

   Function \code{SCFTs} does more extensive SCFT and ARFT calculations than function \code{GRind}: 
   in particular, the function allows to do such calculations for more projection sizes, 
   either restricting attention to full resolution projections or going for ALL projections.
   These capabilities have been introduced in relation to regularity checking based on SCFTs 
   (a paper on this topic is under preparation): 
   Defining a factorial design as regular if all main effects are orthogonal in some sense 
   to effects including other factors of any order, it is conjectured that a regularity check on full resolution 
   projections only will suffice for identifying non-regularity (work in progress). 
   However, this is a conjecture only; as long as it is not proven, a definite check for this type of regularity requires checking ALL projections, 
   i.e. setting \code{resk.only} to \code{FALSE}. With this setting, the function may run for a very long time!
   
}
\value{

   Function \code{GR} returns a list with elements \code{GR} (the generalized resolution of the array, a not necessarily integer
   number between 3 and 5) and \code{RPFT} (the relative projection frequency table).
   \code{GR} values smaller than 5 are exact, while the number five
   stands for \dQuote{at least 5}. The resolution itself is the integer portion of \code{GR}.
   The \code{RPFT} element is the relative projection frequency table for 4-factor projections
   for \code{GR=5}. For unconfounded three- and four-column designs, \code{GR} takes the
   value \code{Inf} (used to be 5 for package versions up to 0.23-4).

   Function \code{GRind} works on designs with resolution at least 3 and
   returns a list with elements \cr
   \code{GRs} (the two versions of
   generalized resolution described in Groemping and Xu 2014), \cr
   the matrix \code{GR.i} with rows GRtot.i and GRind.i for the
   factor wise generalized resolutions (also in Groemping and Xu 2014), \cr
   and optionally \cr
   the ARFT (Groemping 2013), \cr
   the SCFT (Groemping 2013), \cr
   and/or the canonical correlations.\cr
   The latter are held in an \cr
   nfac x choose(nfac-1, R-1) x max(nlev)-1 array \cr
   and are supplemented with 0es,
   if there are fewer of them than the respective dfi.

   The factor wise generalized resolutions are in the closed interval between
   resolution and resolution + 1. In the latter case, their meaning is "at least
   resolution + 1". (The print method ensures that they are printed accordingly,
   but the list elements themselves are just the numbers.)

   Function \code{SCFTs} returns a list of lists with a component for 
   each projection size considered. Each such component contains the following entries: 
     \item{SCFT}{Squared canonical correlation table for the projection size}
     \item{ARFT }{Average R^2 frequency table for the projection size (if requested)}
     \item{cancors }{canonical correlations (if requested)}
}
\references{
   Groemping, U. (2011). Relative projection frequency tables for orthogonal arrays.
      Report 1/2011, \emph{Reports in Mathematics, Physics and Chemistry}
      \url{http://www1.beuth-hochschule.de/FB_II/reports/welcome.htm},
      Department II, Beuth University of Applied Sciences, Berlin.

   Groemping, U. (2013). Frequency tables for the coding invariant ranking of
      orthogonal arrays. Report 2/2013, \emph{Reports in Mathematics, Physics and Chemistry}
      \url{http://www1.beuth-hochschule.de/FB_II/reports/welcome.htm},
      Department II, Beuth University of Applied Sciences, Berlin.

   Groemping, U. and Xu, H. (2014). Generalized resolution for orthogonal arrays.
      \emph{The Annals of Statistics} \bold{42}, 918--939.
      \url{http://prof.beuth-hochschule.de/fileadmin/user/groemping/downloads/AOS1205.pdf}
   }
\author{ Ulrike Groemping }
\seealso{ See also \code{\link{GWLP}} and \code{\link{generalized.word.length}}}

\section{Warning }{The functions have been checked on the types of designs for which
   they are intended (especially orthogonal arrays produced with oa.design) and
   on 2-level fractional factorial designs produced with package \pkg{FrF2}.
   They may produce meaningless results for some other types of designs.
   }

\examples{
   oa24.bad <- oa.design(L24.2.13.3.1.4.1, columns=c(1,2,14,15))
   oa24.good <- oa.design(L24.2.13.3.1.4.1, columns=c(3,10,14,15))
   ## generalized resolution differs (resolution is III in both cases)
   GR(oa24.bad)
   GR(oa24.good)

   ## and analogously also GRind and ARFT and SCFT
   GRind(oa24.bad)
   GRind(oa24.good)

   ## SCFTs
   plan <- L24.2.12.12.1[,c(1:5,13)]
   GRind(plan)  ## looks regular (0/1 SCFT only)
   SCFTs(plan)
   SCFTs(plan, resk.only=FALSE)

}
\keyword{ design }
\keyword{ array }
