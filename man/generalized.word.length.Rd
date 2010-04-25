\name{generalized.word.length}
\alias{generalized.word.length}
\alias{length3}
\alias{length4}
\alias{P3.3}
\alias{P4.3}
\alias{P4.4}
\alias{oa.min3}
\alias{oa.min34}
\alias{oa.max3}
\alias{nchoosek}
\title{
Functions for calculating the generalized word length pattern,
projection frequencies or optimizing column selection within an array
}
\description{
Functions length3 and length4 calculate the numbers of generalized words 
of lengths 3 and 4, respectively. Functions P3.3, P4.3 and P4.4 
calculate projection frequencies, functions oa.min3, oa.min34 and oa.max3 
determine column allocations with minimum or maximum aliasing. 
Function nchoosek is an auxiliary function 
for calculating all subsets without replacement.
}
\usage{
length3(design, with.blocks = FALSE)
length4(design, with.blocks = FALSE, separate = FALSE)
oa.min3(ID, nlevels, all)
oa.min34(ID, nlevels, min3=NULL, all)
oa.max3(ID, nlevels)
P3.3(ID, digits = 4)
P4.3(ID, digits = 4)
P4.4(ID, digits = 4)
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
  \item{separate}{a logical, indicating whether or not separate (and overlapping) sums are requested 
      for each two-factor interaction; the idea is to be able to identify clear two-factor interactions; 
      this may be useful for a design for which \code{length3} returns zero, in analogy to clear 
      2fis for regular fractional factorials, implemented in function \code{\link[FrF2]{FrF2}}; this 
      is currently experimental and may be removed again if it does not prove useful.}
  \item{ID}{an orthogonal array, either a matrix or a data frame; need not be of class \code{oa}; 
      can also be a character string containing the name of an array listed in data frame \code{\link{oacat}}}
  \item{nlevels}{a vector of requested level informations (vector with an entry for each factor)}
  \item{all}{logical; if \code{FALSE}, the search stops whenever a design with 0 generalized words 
       of highest requested length is found; otherwise, the function always determines all best designs}
  \item{min3}{the outcome of a call to \code{oa.min3}, which is to be used for a call to \code{oa.min34}}
  \item{digits}{number of decimal points to which to round the result}
  \item{n}{number of elements to choose from, integer}
  \item{k}{number of elements to be chosen, integer from 0 to n}
}
\details{
   These functions work for factors only and are not intended for quantitative variables. 
   Nevertheless it is possible to apply them to class \code{\link{design}} plans with quantitative 
   variables in them in some situations. 
   
   The generalized word length pattern as introduced in Xu and Wu (2001) is the basis 
   for the functions described here. Consult their article for rigorous mathematical detail 
   of this concept. A brief explanation is also given here, 
   before explaining the details for the functions: Assume a design with qualitative 
   factors, for which all factors are coded with Helmert (\code{\link{contrasts}} 
   (which orthogonalizes the model matrix columns to the intercept column). 
   Furthermore, normalize all model matrix columns to Euclidean norm \code{sqrt(n)}.\cr 
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

   Functions \code{length3} and \code{length4} calculate the generalized word length 
   pattern (numbers of generalized words of lengths 3 and 4 respectively). 
   They are fast for small numbers of factors but can take a 
   long time if the number of factors is large. Note that an orthogonal array 
   based design is called (generalized) resolution III if the result of 
   function \code{length3} is non-zero, (generalized) resolution IV, 
   if the result of function \code{length3} is zero and the 
   result of function \code{length4} is non-zero, 
   and (generalized) resolution V+ (at least V), 
   if the result of both functions \code{length3} 
   and \code{length4} are zero. 
   
   Functions \code{P3.3}, \code{P4.3} and \code{P4.4} 
   calculate the pattern of generalized words of length 3 for all three-factor 
   projections of an array and of generalized words of length 3 or 4 for all four-factor 
   projections of an array. Calculation of such projection frequencies has been proposed 
   by Xu, Cheng and Wu (2004).
   
   The functions can be used in selecting among different possibilities to accomodate factors 
   within a given orthogonal array (cf. examples). For general purposes, it is recommended 
   to use designs with as small an outcome of \code{length3} as possible, and within the same result 
   for \code{length3} (particularly 0), with as small a result for \code{length4} as possible. This 
   corresponds to (a step towards) generalized minimum aberration. It can also be useful 
   to consider the patterns, particularly \code{P3.3}.
   
   Functions \code{oa.min3} and \code{oa.min34} optimize column allocation for a given array 
   for which a certain factor combination must be accomodated: They return designs that allocate 
   columns such that the number of generalized words of length 3 is minimized (\code{oa.min3}) or 
   the number of generalized words of length 4 is minimized within all designs for which the number 
   of generalized words of length 3 is minimal. Function \code{oa.max3} does the opposite: it searches 
   for the worst design in terms of the number of generalized words of length 3. Such a design can be used 
   for demonstrating the benefit of optimizing the number of words. Occasionally, it may also be useful, 
   if there are severe restrictions on possible combinations.
}
\value{
   The functions \code{length3} and \code{length4} (currently) return the number of 
   words only, functions \code{P3.3}, \code{P4.3} and \code{P4.4} return a matrix 
   with the numbers of generalized words of length 3 (4) that do occur 
   for 3 (4) factor projections (column \code{length3} or \code{length4} resp.) 
   and their frequencies.
   
   The functions \code{oa.min3} and \code{oa.min34} (currently) return a list with elements 
   \code{GWP} (the number(s) of generalized words of length 3 (lengths 3 and 4)) 
   \code{column.variants} (the columns to be used for design creation, ordered with 
   ascending nlevels) and \code{complete} (logical indicating whether or not the list is 
   guaranteed to be complete). 

   The function \code{nchoosek} returns a matrix with k rows and \code{choose(n, k)} columns, 
   each of which contains a different subset of k elements.
}
\references{
   Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for asymmetrical
      fractional factorial designs. \emph{Annals Statistics} \bold{29}, 1066--1077.
      
   Xu, H., Cheng, S., and Wu, C.F.J. (2004). Optimal projective three-level
      designs for factor screening and interaction detection. \emph{Technometrics} \bold{46}, 
      280--292.
   }
\author{ Ulrike Groemping }
\note{Function \code{nchoosek} has been taken from \pkg{Bioconductor} package \pkg{vsn}.}

\section{Warning }{The functions have been checked on the types of designs for which 
   they are intended (especially orthogonal arrays produced with oa.design) and 
   on 2-level fractional factorial designs produced with package \pkg{FrF2}. 
   They may produce meaningless results for some other types of designs.}

\examples{
   ## check a small design 
   oa12 <- oa.design(nlevels=c(2,2,6))
   length3(oa12)
   ## length4 is of course 0, because there are only 3 factors
   P3.3(oa12)
   P4.3(oa12)   ## returns NULL because there are no projections onto 4 factors

   ## the results need not be an integer
   oa12 <- oa.design(L12.2.11,columns=1:6)
   length3(oa12)
   length4(oa12)
   P3.3(oa12)  ## all projections have the same pattern
             ## which is known to be true for the complete L12.2.11 as well
   P3.3(L18)   ## this is the pattern of the Taguchi L18
             ## also published by Schoen 2009
   
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
   
   ## choices for columns can be explored with functions oa.min3, oa.min34 or oa.max3
   oa.min3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   oa.min34(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   oa.max3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))    ## this is not for finding 
                                                    ## a good design!!!
                                                    
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
