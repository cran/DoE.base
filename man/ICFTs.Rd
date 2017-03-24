\name{ICFTs}
\alias{ICFTs}
\alias{ICFT}
\title{
Function for calculating interaction contribution frequency tables
}
\description{
Function ICFTs calculates interaction contribution frequency tables, 
function ICFT does the same for an entire (usually small) design with more detail.
}
\usage{
ICFTs(design, digits = 3, resk.only = TRUE, kmin = NULL, kmax = ncol(design), 
   detail = FALSE, with.blocks = FALSE, conc = TRUE)
ICFT(design, digits = 3, with.blocks = FALSE, conc = TRUE, recode=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{a factorial design. This can either be a matrix or a data frame
      in which all columns are experimental factors, or a special data frame
      of class \code{\link[DoE.base:class-design]{design}}, which may also include response data.\cr
      In any case, the design should be a factorial design;
      the functions are not useful for quantitative designs (like e.g. latin hypercube samples).
      }
  \item{digits}{integer; number of digits to round to}
  \item{resk.only}{logical; if \code{all} is \code{TRUE}, should only full resolution projections be considered? 
              Choosing \code{FALSE} may cause very long run times.}
  \item{kmin}{integer; purpose is to continue an earlier run with additional larger projections}
  \item{kmax}{integer; limit on projection sizes to consider}
  \item{detail}{logical indicating whether calculation details 
       are to be returned (see Groemping 2016). These will not be needed
       for normal use of the outcome, but may be interesting for special situations.
       }
  \item{with.blocks}{a logical, indicating whether or not an existing block factor
      is to be included into word counting. This option is ignored if \code{design}
      is not of class \code{design}.\cr
      Per default, an existing block factor is ignored.\cr
      For designs without a block factor, the option does not have an effect.\cr
      If the design is blocked, and \code{with.blocks}
      is \code{TRUE}, the block factor is treated like any other factor.}
  \item{conc}{logical indicating whether ambiguities should be resolved 
      concentrating the contribution on as few individual values as possible (default) 
      or distributing it as evenly as possible (if \code{FALSE})}
  \item{recode}{
     logical indicating whether or not to recode each column into normalized orthogonal 
     coding with function \code{\link{contr.XuWu}};\cr
     if set to FALSE, it is the users responsibility to provide a design in a 
     \emph{normalized orthogonal} coding of choice
     }
}
\details{
   The functions work for factors only and are not intended
   for quantitative variables. 

   Function \code{ICFTs} decomposes the projected $a_k$ values (most often: projected $a_3$ values) 
   into single degree of freedom contributions from the respective $k$ factor interaction.   
   
   Function \code{ICFT} decomposes the all-factor interaction of the design given to it; 
   it is intended for deep-dive investigations.
   
   The ICFT itself is independent of the choice of normalized orthogonal coding, as are the 
   singular values and the matrix of left singular vectors; in case of several identical 
   singular values, the left singular vectors are not uniquely determined but are subject to 
   arbitrary rotation. The right singular vectors depend on the choice of normalized orthogonal 
   coding. They represent the directions of coefficient vectors for which the interaction contributions 
   indicate the bias potential for the intercept (see Groemping 2016 for the maths behind this).
}
\value{
   Function \code{ICFTs} returns a list of lists with a component for 
   each projection size considered. Each such component contains the following entries: 
     \item{ICFT}{interaction contribution frequency table for the projection size}
     \item{ICs }{individual interaction contributions (if requested by option \code{detail})}
     \item{sv2s }{squared singular values (if requested by option \code{detail})}
     \item{mean.u2s }{squared column means of left-singular vectors (if requested by option \code{detail})}
     
   Function \code{ICFT} returns a list with the following components: 
     \item{ICFT}{interaction contribution frequency table for the projection size}
     \item{ICs }{Average R^2 frequency table for the projection size}
     \item{sv2s }{squared singular values of the model matrix}
     \item{mean.u2s }{squared column means of left-singular vectors in the rotated version (concentrated or even)}
     \item{mm}{model matrix of the interaction}
     \item{u}{(left singular vectors corresponding to the rotated version of ICFT (concentrated or even); 
        these do not depend on the coding underlying the model matrix}   
     \item{v}{(right singular vectors corresponding to the rotated version of ICFT (concentrated or even); 
        these depend on the coding underlying the model matrix}   
     \item{c.worst}{(\code{v\%*\%c.worst} is the worst case parameter vector for the model matrix \code{mm} 
        in terms of bias of the average response for estimation of the intercept 
        caused by the interaction under consideration}   
}
\references{
   Groemping, U. (2017). An Interaction-Based Decomposition of Generalized Word Counts Suited to Assessing 
       Combinatorial Equivalence of Factorial Designs. \emph{Reports in Mathematics, Physics and Chemistry}, 
       Report 1/2017. \url{http://www1.beuth-hochschule.de/FB_II/reports/Report-2017-001.pdf},
      Department II, Beuth University of Applied Sciences, Berlin.
   }
\author{ Ulrike Groemping }
\seealso{ See also \code{\link{GWLP}} and \code{\link{generalized.word.length}}}

\section{Warning }{The functions have been checked on the types of designs for which
   they are intended (especially orthogonal arrays produced with oa.design).
   They may produce meaningless results for some other types of designs.
   }

\examples{
   oa24.bad <- oa.design(L24.2.13.3.1.4.1, columns=c(1,2,14,15))
   oa24.good <- oa.design(L24.2.13.3.1.4.1, columns=c(3,10,14,15))
   ## resolution is III in both cases, but the bad one has more words of length 3
   GWLP(oa24.bad)[4:5]
   ICFTs(oa24.bad)
   ICFTs(oa24.bad, conc=FALSE)
   GWLP(oa24.good)[4:5]
   ICFTs(oa24.good)
   ICFTs(oa24.good, conc=FALSE)
   ICFTs(oa24.good, resk.only=FALSE)
   
   ICFT(L18[,c(1,4,6)])
   ICFT(L18[,c(1,4,6)], conc=FALSE)
}
\keyword{ design }
\keyword{ array }
