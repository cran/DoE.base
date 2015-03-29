\name{generalized.word.length}
\alias{generalized.word.length}
\alias{length2}
\alias{length3}
\alias{length4}
\alias{length5}
\alias{lengths}
\alias{lengths.default}
\alias{lengths.design}
\alias{lengths.matrix}
\alias{contr.XuWu}
\alias{contr.XuWuPoly}
\alias{P3.3}
\alias{P4.4}
\alias{GR}
\alias{GRind}
\alias{oa.min3}
\alias{oa.min34}
\alias{oa.max3}
\alias{oa.max4}
\alias{oa.maxGR}
\alias{oa.minRelProjAberr}
\alias{nchoosek}
\alias{tupleSel}
\alias{tupleSel.design}
\alias{tupleSel.default}
\title{
Functions for calculating the generalized word length pattern,
projection frequencies or optimizing column selection within an array
}
\description{
Functions length2, length3, length4 and length5 calculate the numbers of generalized words 
of lengths 2, 3, 4, and 5 respectively, lengths calculates them all. Functions P3.3 and P4.4 
calculate projection frequencies, functions oa.min3, oa.min34, oa.maxGR, oa.minRelProjAberr, 
oa.max3 and oa.max4 determine column allocations with minimum or maximum aliasing. 
Function nchoosek is an auxiliary function 
for calculating all subsets without replacement.
}
\usage{
length2(design, with.blocks = FALSE, J = FALSE)
length3(design, with.blocks = FALSE, J = FALSE, rela = FALSE)
length4(design, with.blocks = FALSE, separate = FALSE, J = FALSE, rela = FALSE)
length5(design, with.blocks = FALSE, J = FALSE, rela = FALSE)
lengths(design, ...)
\method{lengths}{default}(design, ...)
\method{lengths}{design}(design, ...)
\method{lengths}{matrix}(design, ...)
contr.XuWu(n, contrasts=TRUE)
contr.XuWuPoly(n, contrasts=TRUE)
oa.min3(ID, nlevels, all, rela = FALSE, variants = NULL, crit = "total")
oa.min34(ID, nlevels, variants = NULL, min3=NULL, all = FALSE, rela = FALSE)
oa.max3(ID, nlevels, rela = FALSE)
oa.max4(ID, nlevels, rela = FALSE)
oa.maxGR(ID, nlevels, variants = NULL)
oa.minRelProjAberr(ID, nlevels, maxGR = NULL)
P3.3(ID, digits = 4, rela=FALSE, parft=FALSE, parftdf=FALSE, detailed=FALSE)
P4.4(ID, digits = 4, rela=FALSE, parft=FALSE, parftdf=FALSE, detailed=FALSE)
GR(ID, digits=2)
GRind(design, digits=3, arft=TRUE, scft=TRUE, cancors=FALSE, with.blocks=FALSE)
nchoosek(n, k)
tupleSel(design, type="complete", selprop=0.25, ...)
\method{tupleSel}{design}(design, type="complete", selprop=0.25, ...)
\method{tupleSel}{default}(design, type="complete", selprop=0.25, ...)

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
       \code{rela} cannot be \code{TRUE} simultaneously with \code{parft}, \code{parftdf}, 
       \code{J} or \code{separate}.
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
      for functions \code{contr.XuWu} and \code{contr.XuWuPoly}: 
      number of levels of the factor for which to determine contrasts\cr
      for function \code{nchoosek}: number of elements to choose from}
  \item{contrasts}{must always be \code{TRUE}; option needed for 
      function \code{model.matrix} to work properly}
  \item{ID}{an orthogonal array, either a matrix or a data frame; need not be of class \code{oa}; 
      can also be a character string containing the name of an array listed in data frame \code{\link{oacat}}}
  \item{nlevels}{a vector of requested level informations (vector with an entry for each factor)}
  \item{all}{logical; if \code{FALSE}, the search stops whenever a design with 0 generalized words 
       of highest requested length is found; otherwise, the function always determines all best designs}
  \item{variants}{matrix of integer column number entries; each row gives the column numbers 
       for one variant to be compared; the matrix columns must correspond to the entries of the \code{nlevels} 
       option}
  \item{crit}{character string that requests \code{"total"} or \code{"worst"} triple optimization; 
       \code{"total"} corresponds to the previous version that optimizes the overall number of length 3 words;
       \code{"worst"} minimizes the aliasing of the worst triple.}
  \item{min3}{the outcome of a call to \code{oa.min3} with \code{crit="total"}, 
       which is to be used for a call to \code{oa.min34}}
  \item{maxGR}{the outcome of a call to \code{oa.min3} with \code{crit="worst"} and \code{rela=TRUE} 
       (or the outcome of a call to \code{oa.maxGR}), which is to be used for a call to \code{oa.minRelProjAberr}}
  \item{digits}{number of decimal points to which to round the result}
  \item{parft}{logical indicating whether to tabulate projection averaged $R^2$ 
       values (see Groemping 2013) instead of word lengths;
       if \code{TRUE}, the table shows projection averaged $R^2$ values as 
       detailed in Groemping (2013).\cr
       \code{parft=TRUE} is only permitted for the shortest possible word length, 
       i.e. \code{length3} or \code{P3.3} for resolution III designs, 
       \code{length4} or \code{P4.4} for resolution IV designs.\cr
       \code{parft} cannot be \code{TRUE} simultaneously with \code{rela} or \code{parftdf}.
       }
  \item{parftdf}{logical indicating whether to tabulate averaged $R^2$ 
       values, where averaging is over individual degrees of freedom; this variant 
       is not explicitly described in Groemping (2013) and usually yields very similar 
       results as \code{parft}, except for some situations where there are factors 
       with very unequal numbers of levels (e.g. 2-level and 8-level factors).\cr
       \code{parftdf=TRUE} is only permitted for the shortest possible word length, 
       i.e. \code{length3} or \code{P3.3} for resolution III designs, 
       \code{length4} or \code{P4.4} for resolution IV designs.\cr
       \code{parftdf} cannot be \code{TRUE} simultaneously with \code{rela} or \code{parft}.
       }
  \item{detailed}{logical indicating whether the vector of all (relative) tuple word lengths 
       is to become an attribute of the output object (attribute \code{detail}); \cr
       intended for supporting other functions (can be a very long vector!)
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
  \item{k}{number of elements to be chosen, integer from 0 to n}
  \item{type}{character string with type of worst case to consider;\cr 
       \code{"complete"}, \code{"worst"} and \code{"worst.rel"} are available.\cr
       For a resolution R design, tuples of R factors are considered (works for R=3 and R=4 only).
       \code{"complete"} selects all tuples with complete aliasing of at least one factor, 
       \code{"worst"} selects all tuples whose number of words is larger than the \code{1-selprop} 
       quantile of the word length distribution of R-tuples, \code{"worst.rel"} does the same 
       with relative words (i.e. increases the weight for tuples whose minimum number of levels 
       is small), \code{"worst.parft"} and \code{"worst.parftdf"} do the same with the 
       different versions of projection average $R^2$ values.}
  \item{selprop}{(approximate) proportion of worst case tuples to be selected (see \code{type})}
  \item{\dots}{further arguments; for the \code{design} method of function \code{lengths}, 
        the defaults \code{with.blocks = FALSE, J = FALSE} can be changed here; 
        for function \code{tupleSel}, \dots is currently not used}
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
   Functions \code{contr.XuWu} and \code{contr.XuWuPoly} provide such contrasts 
   based on Helmert contrasts or orthogonal polynomial contrasts, 
   normalized according to the prescription by Xu and Wu (2001) 
   which implies that all model matrix columns 
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

   Function \code{lengths} calculates the generalized word length 
   pattern (numbers of generalized words of lengths 2, 3, 4 and 5 respectively), 
   functions \code{length2}, \code{length3}, \code{length4} and \code{length5} calculate 
   each length separately. For designs with few rows and many columns, the newer 
   function \code{\link{GWLP}} is much faster; therefore it will be a better choice 
   than \code{lengths} for most applications. On the other hand, for designs with 
   many rows, \code{lengths} can be much faster. Furthermore, \code{lengths} and 
   the compoment functions \code{length2} to \code{length5} can calculate additional 
   detail not available from \code{\link{GWLP}}.
   
   The most important component length functions are 
   \code{length3} and \code{length4}; \code{length2}
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
   to use designs with as small an outcome of \code{length3} as possible (either absolute or relative,
   either total or worst case), and within the same result 
   for \code{length3} (particularly 0), with as small a result for \code{length4} as possible. This 
   corresponds to (a step towards) generalized minimum aberration. It can also be useful 
   to consider the patterns, particularly \code{P3.3}.
   
   Function \code{GR} calculates the generalized resolution according to Deng and Tang (1999) 
   for 2-level designs or a generalization thereof according to Groemping (2011) for general 
   orthogonal arrays. It returns a value between 3 and 5, where the numeric value 5 stands for 
   \dQuote{at least 5}. Roughly, generalized resolution measures the closeness of a design 
   to the next higher resolution (worst-case based, e.g. one completely aliased triple of 
   factors implies resolution 3).
   
   Functions \code{oa.min3}, \code{oa.min34} 
   optimize column allocation for a given array 
   for which a certain factor combination must be accomodated: They return designs that allocate 
   columns such that the number of generalized words of length 3 is minimized (\code{oa.min3}; 
   with a choice between minimizing the total number or minimizing the number for the worst-case triple 
   of factors), or 
   the number of generalized words of length 4 is minimized within all designs for which the number 
   of generalized words of length 3 is minimal (\code{oa.min34}, total number only). 
   Option \code{rela} allows to switch 
   from the default consideration of absolute numbers of words to relative numbers of words 
   according to Groemping (2011). 
   
   Function \code{oa.maxGR} maximizes generalized resolution 
   according to Deng and Tang (1999) as generalized by Groemping (2011), 
   function \code{oa.minRelProjAberr} conducts minimum relative projection aberration 
   according to Groemping (2011), with the four steps (a) maximize GR, (b) minimize rA3 or 
   rA4 (depending on resolution), (c) optimize RPFT (as obtained by \code{P3.3} 
   or \code{P4.4}) and (d) minimize absolute words of lengths 4 etc. (only carried through 
   to length 4 by the function. Note that function \code{oa.maxGR} 
   can be replaced by the much faster function \code{oa.min3} with options 
   \code{crit="worst"} and \code{rela=TRUE}, whenever GR<=4. Only for designs with 
   GR > 4, the extra effort with function \code{oa.maxGR} is useful.
   
   Functions \code{oa.max3} and \code{oa.max4} 
   do the opposite: they search for the worst design in terms of the number of 
   generalized words of lengths 3 or 4. Such a design can e.g. be used 
   for demonstrating the benefit of optimizing the number of words, 
   or for exemplifying theoretical properties. 
   Occasionally, it may also be useful, 
   if there are severe restrictions on possible combinations. 
   (\code{oa.max4} should only be used for resolution IV designs.)
   
   Function \code{tupleSel} selects worst case tuples of R factors for resolution R designs. 
   Depending on the type requested, all completely aliased tuples are selected, 
   or the worst case tuples that exceed the \code{1-selprop} quantile of the numbers 
   of absolute or relative words are selected. 
   
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
   The matrix of projection frequencies has the overall number of generalized words 
   of the respective length as an attribute; in the case \code{rela=TRUE} it also 
   has the generalized resolution and the overall absolute number of generalized words          
   of the respective length as an attribute.
   
   The functions \code{oa.min3}, \code{oa.min34}, \code{oa.max3} and \code{oa.max4} 
   (currently) return a list with elements 
   \code{GWP} (the number(s) of generalized words of length 3 (lengths 3 and 4)) 
   \code{column.variants} (the columns to be used for design creation, ordered with 
   ascending nlevels) and \code{complete} (logical indicating whether or not the list is 
   guaranteed to be complete). 
   \code{oa.min3}, the name of the first element is either \code{GWP3} (\code{crit="total"}), 
   \code{worst.a3} (\code{rela=FALSE, crit="worst"}) or \code{GR} (\code{rela=FALSE, crit="worst"}).
   The function \code{oa.maxGR} returns a list with elements \code{GR}, \code{column.variants} 
   and \code{complete}, the function \code{oa.minRelProjAberr} returns a list with elements 
   \code{GR}, \code{GWP}, \code{column.variants} and \code{complete}.
   
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

   The function \code{nchoosek} returns a matrix with k rows and \code{choose(n, k)} columns, 
   each of which contains a different subset of k elements.
   
   The function \code{tupleSel} returns a sorted list of worst case tuples, 
   beginning with the worst case. In case of types \code{"worst"} or \code{"worst.rel"}, 
   attributes provide the (relative) projection frequency tables and the sorted 
   vector of the worst case projection values corresponding to the listed tuples.
}
\references{
   Ai, M.-Y. and Zhang, R.-C. (2004). Projection justification of generalized minimum
      aberration for asymmetrical fractional factorial designs. 
      \emph{Metrika} \bold{60}, 279--285.
      
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

   Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for asymmetrical
      fractional factorial designs. \emph{The Annals of Statistics} \bold{29}, 1066--1077.
      
   Xu, H., Cheng, S., and Wu, C.F.J. (2004). Optimal projective three-level
      designs for factor screening and interaction detection. \emph{Technometrics} \bold{46}, 
      280--292.
   }
\author{ Ulrike Groemping }
\note{Function \code{nchoosek} has been taken from \pkg{Bioconductor} package \pkg{vsn}.\cr
Function \code{GWLP} is much faster (but also more inaccurate) than function \code{lengths} 
and may be a better choice for designs with many factors.}
\seealso{ See also \code{\link{GWLP}} for a version of lengths that is much 
    faster for designs with not so many runs.}

\section{Warning }{The functions have been checked on the types of designs for which 
   they are intended (especially orthogonal arrays produced with oa.design) and 
   on 2-level fractional factorial designs produced with package \pkg{FrF2}. 
   They may produce meaningless results for some other types of designs.
   
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
   
   ## and analogously also GRind and ARFT and SCFT
   GRind(oa24.bad)
   GRind(oa24.good)

   ## GR and GRind can be different   
   GRind(L18[, c(1:4,6:8)], arft=FALSE, scft=FALSE)

   ## choices for columns can be explored with functions oa.min3, oa.min34 or oa.max3
   oa.min3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   oa.min34(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))
   ## columns for designs with maximum generalized resolution 
   ##    (can take very long, if all designs have worst-case aliasing) 
      ## then optimize these for overall relative number of words of length 3
      ##     and in addition absolute number of words of length 4 
   mGR <- oa.maxGR(L18, c(2,3,3,3,3,3,3))
   oa.minRelProjAberr(L18, c(2,3,3,3,3,3,3), maxGR=mGR)
   
   oa.max3(L24.2.13.3.1.4.1, nlevels=c(2,2,3,4))    ## this is not for finding 
                                                    ## a good design!!!
                                                    
   \dontrun{
   ## play with selection of optimum design
   ## somewhat experimental at present
   oa.min3(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4))
   best3 <- oa.min3(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), rela=TRUE)
   oa.min34(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4))
   oa.min34(L32.2.10.4.7, nlevels=c(2,2,2,4,4,4,4,4), min3=best3)
   
   ## generalized resolution according to Groemping 2011, manually
   best3GR <- oa.min3(L36.2.11.3.12, c(rep(2,3),rep(3,3)), rela=TRUE, crit="worst")
      ## optimum GR is 3.59
   ## subsequent optimization w.r.t. rA3
   best3reltot.GR <- oa.min3(L36.2.11.3.12, c(rep(2,3),rep(3,3)), rela=TRUE, 
           variants=best3GR$column.variants)
      ## optimum rA3 is 0.5069
   ## (note: different from first optimizing rA3 (0.3611) and then GR (3.5))
   ## remaining nine designs: optimize RPFTs
   L36 <- oa.design(L36.2.11.3.12, randomize=FALSE)
   lapply(1:9, function(obj) P3.3(L36[,best3reltot.GR$column.variants[obj,]]))
      ## all identical
   oa.min34(L36, nlevels=c(rep(2,3),rep(3,3)), min3=best3reltot.GR)
      ## still all identical
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
