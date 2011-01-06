\name{generalized.word.length}
\alias{generalized.word.length}
\alias{length2}
\alias{length3}
\alias{length4}
\alias{length5}
\alias{lengths}
\alias{contr.XuWu}
\alias{P3.3}
\alias{P4.4}
\alias{GR}
\alias{oa.min3}
\alias{oa.min34}
\alias{oa.max3}
\alias{oa.max4}
\alias{oa.maxGR}
\alias{oa.maxGR.min34}
\alias{nchoosek}
\title{
Functions for calculating the generalized word length pattern,
projection frequencies or optimizing column selection within an array
}
\description{
Functions length2, length3, length4 and length5 calculate the numbers of generalized words 
of lengths 2, 3, 4, and 5 respectively, lengths calculates them all. Functions P3.3 and P4.4 
calculate projection frequencies, functions oa.min3, oa.min34, oa.maxGR, oa.maxGR.min34, 
oa.max3 and oa.max4 determine column allocations with minimum or maximum aliasing. 
Function nchoosek is an auxiliary function 
for calculating all subsets without replacement.
}
\usage{
length2(design, with.blocks = FALSE, J = FALSE)
length3(design, with.blocks = FALSE, J = FALSE, rela = FALSE)
length4(design, with.blocks = FALSE, separate = FALSE, J = FALSE, rela = FALSE)
length5(design, with.blocks = FALSE, J = FALSE, rela = FALSE)
lengths(design, with.blocks = FALSE, J = FALSE)
contr.XuWu(n, contrasts = TRUE, sparse = FALSE)
oa.min3(ID, nlevels, all, rela = FALSE)
oa.min34(ID, nlevels, min3=NULL, all, rela = FALSE)
oa.max3(ID, nlevels, rela = FALSE)
oa.max4(ID, nlevels, rela = FALSE)
oa.maxGR(ID, nlevels)
oa.maxGR.min34(ID, nlevels, maxGR = NULL)
P3.3(ID, digits = 4, rela=FALSE)
P4.4(ID, digits = 4, rela=FALSE)
GR(ID, digits=2)
nchoosek(n, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{an experimental design. This can either be a matrix or a data frame 
      in which all columns are experimental factors, or a special data frame 
      of class \code{\link[DoE.base:class-design]{design}}, which may also include response data.\cr
      In any case, the design should be a factorial design; 
      the functions are not useful for quantitative designs (like e.g. latin hypercube samples).
      }
  \item{with.blocks}{a logical, indicating whether or not an existing block factor 
      is to be included into word counting. This option is ignored if \code{design} 
      is not of class \code{design}.\cr 
      Per default, an existing block factor is ignored.\cr
      For designs without a block factor, the option does not have an effect.\cr
      If the design is blocked, and \code{with.blocks} 
      is \code{TRUE}, the block factor is treated like any other factor in terms of word counting.}
  \item{J}{a logical, indicating whether or not a vector of contributions from 
      individual degrees of freedom is produced. If \code{TRUE}, the entries of the vector 
      are absolute normalized J-characteristics from all 3- or 4-factor products respectively, 
      based on normalized Helmert contrasts (cf. Ai and Zhang 2004). \cr
      This is not expected to be useful for practical purposes.\cr
      \code{J} cannot be \code{TRUE} simultaneously with \code{separate} or \code{rela}.}
  \item{rela}{logical indicating whether the word lengths are to be calculated in 
       absolute terms (as usual) or relative to the maximum possible word length in case of 
       complete aliasing; if \code{TRUE}, each word length is divided by the worst 
       case word length (that corresponds to a completely aliased set of factors) 
       derived in Groemping (2011).\cr
       \code{rela=TRUE} is only permitted for the shortest possible word length, 
       i.e. \code{length3} or \code{P3.3} for resolution III designs, 
       \code{length4} or \code{P4.4} for resolution IV designs, or \code{length5} 
       for resolution V designs.\cr
       \code{rela} cannot be \code{TRUE} simultaneously with \code{J} or \code{separate}.
       }
  \item{separate}{a logical, indicating whether or not separate (and overlapping) 
      sums are requested for each two-factor interaction; 
      the idea is to be able to identify clear two-factor interactions; 
      this may be useful for a design for which \code{length3} returns zero, 
      in analogy to clear 2fis for regular fractional factorials, 
      implemented in function \code{\link[FrF2]{FrF2}}; this 
      is currently experimental and may be removed again if it does not prove useful.\cr
      \code{separate} cannot be \code{TRUE} simultaneously with \code{J}.}
  \item{n}{integer; \cr
      for function \code{contr.XuWu}: number of levels of the factor for which to determine contrasts\cr
      for function \code{nchoosek}: number of elements to choose from}
  \item{contrasts}{output a contrast matrix ?}
  \item{sparse}{return a sparse contrast matrix ?}
  \item{ID}{an orthogonal array, either a matrix or a data frame; need not be of class \code{oa}; 
      can also be a character string containing the name of an array listed in data frame \code{\link{oacat}}}
  \item{nlevels}{a vector of requested level informations (vector with an entry for each factor)}
  \item{all}{logical; if \code{FALSE}, the search stops whenever a design with 0 generalized words 
       of highest requested length is found; otherwise, the function always determines all best designs}
  \item{min3}{the outcome of a call to \code{oa.min3}, which is to be used for a call to \code{oa.min34}}
  \item{maxGR}{the outcome of a call to \code{oa.maxGR}, which is to be used for a call to \code{oa.maxGR.min34}}
  \item{digits}{number of decimal points to which to round the result}
  \item{k}{number of elements to be chosen, integer from 0 to n}
}
\details{
   These functions work for factors only and are not intended for quantitative variables. 
   Nevertheless it is possible to apply them to class \code{\link{design}} plans with quantitative 
   variables in them in some situations. 
   
   The generalized word length pattern as introduced in Xu and Wu (2001) is the basis 
   for the functions described here. Consult their article or Groemping (2011) 
   for rigorous mathematical detail of this concept. A brief explanation is also given here, 
   before explaining the details for the functions: Assume a design with qualitative 
   factors, for which all factors are coded with specially normalized Helmert \code{\link{contrasts}} 
   (which orthogonalizes the model matrix columns to the intercept column). 
   Function \code{contr.XuWu} provides such contrasts, normalized according to the 
   prescription by Xu and Wu (2001) which implies that all model matrix columns 
   have Euclidean norm \code{sqrt(n)}, provided that each 
   individual factor is balanced.\cr 
   Then, the number of generalized words of length 3 is determined by taking the sum 
   of squares of the column sums of all three-factor interaction columns 
   (from a model matrix with all three-factor interactions included), divided by the 
   squared number of runs. \cr 
   Likewise, the number of generalized words of length 4 is determined by taking the sum 
   of squares of the column sums of all four-factor interaction columns 
   (from a model matrix with all four-factor interactions included), divided by the 
   squared number of runs, and so on.\cr
   A certain plausibility can be found in these numbers by noting that they provide the 
   more well-known word length pattern for regular fractional factorial 2-level designs, 
   implying that they are exactly zero for resolution IV or resolution V fractional 
   factorial 2-level designs, respectively.

   Function \code{lengths} 
   calculates the generalized word length 
   pattern (numbers of generalized words of lengths 2, 3, 4 and 5 respectively), 
   functions \code{length2}, \code{length3}, \code{length4} and \code{length5} calculate 
   each length separately. 
   The most important ones are \code{length3} and \code{length4}; \code{length2}
   should yield zero for all orthogonal arrays, and \code{length5} will in most 
   cases not be of interest either. The number of shortest possible words, e.g. 
   length 4 for resolution IV designs, can be calculated in relative terms, if 
   interest is in the extent of complete aliasing (cf. Groemping 2011).\cr 
   The length functions are fast for small numbers of factors but can take a 
   long time if the number of factors is large. Note that an orthogonal array 
   based design is called resolution III if the result of 
   function \code{length3} is non-zero, resolution IV, 
   if the result of function \code{length3} is zero and the 
   result of function \code{length4} is non-zero, 
   and resolution V+ (at least V), if the result of both functions \code{length3} 
   and \code{length4} are zero. 
   
   Functions \code{P3.3} and \code{P4.4} 
   calculate the pattern of generalized words of length 3 for all three-factor 
   projections of an array and of generalized words of length 3 or 4 for all four-factor 
   projections of an array. Calculation of such projection frequencies has been proposed 
   by Xu, Cheng and Wu (2004). The relative version for \code{P3.3} and \code{P4.4} has been 
   introduced by Groemping (2011) for better assessment of the projective properties of a design. 
   It divides each absolute number of words by the maximum possible number in case one 
   factor is completely determined by the combinations of the other two factors. 
   For \code{P4.4}, the relative version is valid only for resolution IV designs. 
   
   The functions can be used in selecting among different possibilities to accomodate factors 
   within a given orthogonal array (cf. examples). For general purposes, it is recommended 
   to use designs with as small an outcome of \code{length3} as possible, and within the same result 
   for \code{length3} (particularly 0), with as small a result for \code{length4} as possible. This 
   corresponds to (a step towards) generalized minimum aberration. It can also be useful 
   to consider the patterns, particularly \code{P3.3}.
   
   Function \code{GR} calculates the generalized resolution according to Deng and Tang (1999) 
   for 2-level designs or a generalization thereof according to Groemping (2011) for general 
   orthogonal arrays. It returns a value between 3 and 5, where the numeric value 5 stands for 
   \dQuote{at least 5}.
   
   Functions \code{oa.min3}, \code{oa.min34}, \code{oa.maxGR} and \code{oa.maxGR.min34} 
   optimize column allocation for a given array 
   for which a certain factor combination must be accomodated: They return designs that allocate 
   columns such that the number of generalized words of length 3 is minimized (\code{oa.min3}), or 
   the number of generalized words of length 4 is minimized within all designs for which the number 
   of generalized words of length 3 is minimal (\code{oa.min34}). Option \code{rela} allows to switch 
   from the default consideration of absolute numbers of words to relative numbers of words 
   according to Groemping (2011). Function \code{oa.maxGR} maximizes generalized resolution 
   according to Deng and Tang (1999) as generalized by Groemping (2011), 
   function \code{oa.maxGR.min34} minimizes relative words of lengths 3 or 4 (as appropriate) 
   among the designs with maximum generalized resolution.\cr
   Functions \code{oa.max3} and \code{oa.max4} 
   do the opposite: they search for the worst design in terms of the number of 
   generalized words of lengths 3 or 4. Such a design can e.g. be used 
   for demonstrating the benefit of optimizing the number of words, 
   or for exemplifying theoretical properties. 
   Occasionally, it may also be useful, 
   if there are severe restrictions on possible combinations. 
   (\code{oa.max4} should only be used for resolution IV designs.)
}
\value{
   The functions \code{length3} and \code{length4} (currently) per default 
   return the number of words. \cr
   If option \code{J=TRUE} is set, their value 
   is a named vector of normalized absolute J-characteristics (cf. Ai and Zhang 2004) 
   for the respective length, based on normalized Helmert contrasts, 
   with names indicating factor indices. 
   (For blocked designs with the \code{with.blocks=TRUE} option, 
   the block factor has index 1.)
   
   Functions \code{P3.3} and \code{P4.4} return a matrix 
   with the numbers of generalized words of length 3 (4) that do occur 
   for 3 (4) factor projections (column \code{length3} or \code{length4} resp.) 
   and their frequencies. If option \code{rela=TRUE} is set, the numbers of generalized 
   words are normalized by dividing them by the number of words that corresponds 
   to perfect aliasing among the factors for each projection. For \code{P4.4}, the 
   relative version is only reasonable for resolution IV designs.
   
   The functions \code{oa.min3}, \code{oa.min34}, \code{oa.max3} and \code{oa.max4} 
   (currently) return a list with elements 
   \code{GWP} (the number(s) of generalized words of length 3 (lengths 3 and 4)) 
   \code{column.variants} (the columns to be used for design creation, ordered with 
   ascending nlevels) and \code{complete} (logical indicating whether or not the list is 
   guaranteed to be complete). 
   The function \code{oa.maxGR} returns a list with elements \code{GR}, \code{column.variants}, 
   \code{RPFTs} and \code{complete}. Whenever element \code{GR} is less than 5, 
   element \code{RPFTs} is a list of the relative projection frequency tables corresponding 
   to the rows of the matrix \code{column.variants}. 
   
   Function \code{GR} returns a list with elements \code{GR} (the generalized resolution of the array, a not necessarily integer 
   number between 3 and 5) and \code{RPFT} (the relative projection frequency table). 
   \code{GR} values smaller than 5 are exact, while the number five 
   stands for \dQuote{at least 5}. The resolution itself is the integer portion of \code{GR}.
   The \code{RPFT} element is \code{NULL} for \code{GR=5}. 

   The function \code{nchoosek} returns a matrix with k rows and \code{choose(n, k)} columns, 
   each of which contains a different subset of k elements.
}
\references{
   Ai, M.-Y. and Zhang, R.-C. (2004). Projection justification of generalized minimum
      aberration for asymmetrical fractional factorial designs. 
      \emph{Metrika} \bold{60}, 279--285.
      
   Groemping, U. (2011). Relative projection frequency tables for orthogonal arrays. 
      Report 1/2011, \emph{Reports in Mathematics, Physics and Chemistry} \url{http://www1.beuth-hochschule.de/FB_II/reports/welcome.htm}, 
      Department II, Beuth University of Applied Sciences, Berlin.
      
   Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for asymmetrical
      fractional factorial designs. \emph{Annals of Statistics} \bold{29}, 1066--1077.
      
   Xu, H., Cheng, S., and Wu, C.F.J. (2004). Optimal projective three-level
      designs for factor screening and interaction detection. \emph{Technometrics} \bold{46}, 
      280--292.
   }
\author{ Ulrike Groemping }
\note{Function \code{nchoosek} has been taken from \pkg{Bioconductor} package \pkg{vsn}.}

\section{Warning }{The functions have been checked on the types of designs for which 
   they are intended (especially orthogonal arrays produced with oa.design) and 
   on 2-level fractional factorial designs produced with package \pkg{FrF2}. 
   They may produce meaningless results for some other types of designs.
   
   Programming of some of the functions with \code{rela=TRUE} is not optimized for 
   efficiency; these may take a particularly long time.
   
   Furthermore, all optimizing functions work for relatively small problems only 
   and will break down for larger problems because of storage space requirements 
   (size depends on the number of possible selections among columns; for example, selecting 
   9 out of 31 columns is not doable on my computer because of storage space issues, 
   while selecting 29 out of 31 columns is doable within the available storage space). 
   Programming of a less storage-intensive algorithm is underway.
   }

\examples{
   ## check a small design 
   oa12 <- oa.design(nlevels=c(2,2,6))
   length3(oa12)
   ## length4 is of course 0, because there are only 3 factors
   P3.3(oa12)

   ## the results need not be an integer
   oa12 <- oa.design(L12.2.11,columns=1:6)
   length3(oa12)
   length4(oa12)
   P3.3(oa12)  ## all projections have the same pattern
             ## which is known to be true for the complete L12.2.11 as well
   P3.3(L18)   ## this is the pattern of the Taguchi L18
             ## also published by Schoen 2009
   P3.3(L18[,-2])  ## without the 2nd column (= the 1st 3-level column)
   P3.3(L18[,-2], rela=TRUE)  ## relative pattern, divided by theoretical upper 
                              ## bound for each 3-factor projection
   
   ## choosing among different assignment possibilities
   ## for two 2-level factors and one 3- and 4-level factor each
   show.oas(nlevels=c(2,2,3,4))
   ## default allocation: first two columns for the 2-level factors
   oa24.bad <- oa.design(L24.2.13.3.1.4.1, columns=c(1,2,14,15))
   length3(oa24.bad)
   ## much better: columns 3 and 10
   oa24.good <- oa.design(L24.2.13.3.1.4.1, columns=c(3,10,14,15))
   length3(oa24.good)
   length4(oa24.good)  ## there are several variants, 
                       ## which produce the same pattern for lengths 3 and 4
                       
   ## the difference matters
   plot(oa24.bad, select=c(2,3,4))
   plot(oa24.good, select=c(2,3,4))
   
   ## generalized resolution differs as well (resolution is III in both cases)
   GR(oa24.bad)
   GR(oa24.good)

   ## choices for columns can be explored with functions oa.min3, oa.min34 or oa.max3
   oa.min3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   oa.min34(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   ## columns for designs with maximum generalized resolution 
   ##    (can take very long, if all designs have worst-case aliasing) 
      ## then optimize these for overall relative number of words of length 3
      ##     and in addition absolute number of words of length 4 
   mGR <- oa.maxGR(L18, c(2,3,3,3,3,3,3))
   oa.maxGR.min34(L18, c(2,3,3,3,3,3,3), maxGR=mGR)
   
   oa.max3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))    ## this is not for finding 
                                                    ## a good design!!!
                                                    
   \dontrun{
   ## play with selection of optimum design
   ## somewhat experimental at present
   oa.min3(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4))
   best3 <- oa.min3(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), rela=TRUE)
   oa.min34(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4))
   oa.min34(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), min3=best3, rela=TRUE)
   }

   ## select among column variants with projection frequencies 
   ## here, all variants have identical projection frequencies
   ## for larger problems, this may sometimes be relevant
   variants <- oa.min34(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   for (i in 1:nrow(variants$column.variants)){
      cat("variant ", i, "\n")
      print(P3.3(oa.design(L24.2.13.3.1.4.1, columns=variants$column.variants[i,])))
      }
   
   ## automatic optimization is possible, but can be time-consuming
   ## (cf. help for oa.design)
   plan <- oa.design(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4), columns="min3")
   length3(plan)
   length4(plan)
   plan <- oa.design(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4), columns="min34")
   length3(plan)
   length4(plan)

   \dontrun{
   ## blocked design from FrF2
   ## the design is of resolution IV
   ## there is one (generalized) 4-letter word that does not involve the block factor
   ## there are four more 4-letter words involving the block factor
   ## all this and more can also be learnt from design.info(plan)
   require(FrF2)
   plan <- FrF2(32,6,blocks=4)
   length3(plan)
   length3(plan, with.blocks=TRUE)
   length4(plan)
   length4(plan, with.blocks=TRUE)
   design.info(plan)
   }

}
\keyword{ design }
\keyword{ array }
