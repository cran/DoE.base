\name{oa.design}
\alias{oa.design}
\alias{origin}
\alias{oa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for accessing orthogonal arrays }
\description{
  Function for accessing orthogonal arrays, 
  allowing limited optimal allocation of columns 
}
\usage{
oa.design(ID=NULL, nruns=NULL, nfactors=NULL, nlevels=NULL, 
      factor.names = if (!is.null(nfactors)) {
        if (nfactors <= 50) Letters[1:nfactors]
           else paste("F", 1:nfactors, sep = "")} 
        else NULL, 
        columns="order", 
        replications=1, repeat.only=FALSE,
        randomize=TRUE, seed=NULL, min.residual.df=0)
origin(ID)
oa
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ID}{ orthogonal array to be used; must be given as the name without quotes 
        (e.g. \code{L12.2.2.6.1}); 
        available names can be looked at using function \code{\link{show.oas}} 
        with option \code{parents.only = TRUE} (later it will also be possible 
        to directly access child designs); furthermore, \code{L18}, \code{L36} 
        and \code{L54} for the respective Taguchi arrays can be used.
        Users can also specify names of their own designs here (cf. details). 
        \code{ID} must be of class \code{oa}.
        If omitted, \code{ID} is automatically determined based on \code{nlevels} 
        or \code{factor.names}. }
  \item{nruns}{ minimum number of runs to be used, 
        can be omitted if obvious from \code{ID} or if 
        the smallest possible array is to be found }
  \item{nfactors}{ number of factors; 
        only needed if \code{nlevels} is a single number and 
        \code{factor.names} is omitted;
        can otherwise determined from length of \code{factor.names}, 
        \code{nlevels} or \code{column} }
  \item{nlevels}{ number(s) of levels, vector with \code{nfactors} entries or single number; 
        can be omitted, if obvious from \code{factor.names} or if \code{ID} 
        and \code{columns} are given or if all columns of \code{ID} are to be used 
        with default factor names and levels;
        can be a single number if \code{nfactors} is known directly or as length of 
        \code{factor.names} }
  \item{factor.names}{ a character vector of \code{nfactors} factor names 
      or a list with \code{nfactors} elements;\cr 
      if the list is named, list names represent factor names, otherwise default factor names are used;\cr 
      the elements of the list are \cr
      EITHER vectors of appropriate length (corresponding to \code{nlevels}) 
      with factor levels for the respective factor\cr
      OR empty strings;\cr
       Default factor names are the first elements of the character vector \code{\link{Letters}}, 
       or the factors position numbers preceded by capital F in case of more than 50 factors.
       Default factor levels are the numbers from 1 to the number of levels for each factor.}
  \item{columns}{ 
        EITHER\cr
        a vector of column numbers referring to columns of design \code{ID}, 
        assigning a specific column of the array to each factor;\cr
        this can only be specified, if \code{ID} is also given;\cr 
        OR\cr
        a string that defines the degree of optimization requested 
        in terms of column allocation (cf. section \dQuote{Details}): 
        choices are \code{"order"}, \code{"min3"}, \code{"min34"}, 
        \code{"min3.rela"}, \code{"min34.rela"}, or \code{"minRelProjAberr"}.\cr
        For resource reasons, the default is \code{"order"}, but smaller designs 
        can sometimes be substantially improved with other choices.
        Cf. the \dQuote{Details} section for the 
        meaning of the character string specifications for \code{columns.}
        Column optimization can be computationally intensive. If it cannot 
        be accomplished with the given ressources, a warning is issued, and 
        an unoptimized design is returned.\cr
        Some of the optimization methods have just been proposed, and there is 
        little experience with them. It is strongly recommended to always check 
        the properties of the design w.r.t. suitability for the planned experiment 
        BEFORE starting expensive investments.}
  \item{replications}{ the number of replications of the array, 
        the setting of \code{repeat.only} determines, whether these are real replications
        or repeated measurements only. Note that replications are not considered for 
        accomodation of \code{min.residual.df} residual degrees of freedom, 
        unless a full factorial is used.}
  \item{repeat.only}{ default \code{FALSE} implies real replications, 
        \code{TRUE} implies repeated measurements only }
  \item{randomize}{ logical indicating whether the run order is to be randomized ? }
  \item{seed}{ integer seed for the random number generator }
  \item{min.residual.df}{ minimum number of residual degrees of freedom;\cr
        Note: function \code{oa.design} does not count replications specified with option 
       \code{replications} in determining residual degrees of freedom for \code{min.resid.df}. }
}
\details{
   Function \code{oa.design} assigns factors to the columns of orthogonal arrays that are 
   available within package \pkg{DoE.base} or are provided by the user. 
   The available arrays and their properties are listed in the 
   data frame \code{oacat} and can be systematically searched for using function \code{\link{show.oas}}. 
   The design names also indicate the number of runs and the numbers of factors 
   for each number of levels, e.g. \code{L18.3.6.6.1} is an 18 run design with six factors in 3 levels 
   (\code{3.6}) and one factor in 6 levels (\code{6.1}). 
   
   \code{oa} is the S3 class used for orthogonal arrays. Objects of class \code{oa} should at least have 
   the attribute \code{origin}, an attribute \code{comment} should be used for additional information.
   
   Users can define their own orthogonal arrays and hand them to \code{oa.design} with parameter \code{ID}. 
   Requirements for the arrays: \cr
   \itemize{
       \item Factor levels must be coded as numbers from 1 to number of levels.
       \item The array must be of classes \code{oa} and \code{matrix}\cr 
       (If your array is a matrix named \code{foo}, you can simply assign it class \code{oa} by 
       the command \code{class(foo) <- c("oa","matrix")}, see also last example.)
       \item The array should have an attribute \code{origin}.
       \item The array can have an attribute \code{comment}; \cr
             this should be used for mentioning specific properties, e.g. 
             for the \code{L18.2.1.3.7} that the interaction of the first two factors 
             can be estimated.
             }
   
   Users are encouraged to send additional arrays to the package maintainer. 
   The requirements for these are the same as listed above, with attribute \code{origin} 
   being a MUST in this case. (See the last example for how to assign an attribute.)
   
   Currently, package \pkg{DoE.base} contains the orthogonal arrays from Warren Kuhfelds 
   collection of \dQuote{parent} arrays only, plus very few additional designs, 
   and can automatically create the child arrays from Kuhfelds collection, using 
   the replacement instructions provided in the data frame \code{\link{oacat}} 
   through the variable \code{lineage}. The last example below indicates how 
   a child array can be created manually, and compares this to the automatically created array.\cr
   (A lot more than just the child arrays could be obtained from these arrays 
   by implementing a functionality similar to the market research macros available in SAS; presumably, 
   this topic will not be addressed soon, as it will involve a substantial amount of work.)
   
   If no specific orthogonal array is specified and function \code{oa.design} does not 
   find an orthogonal array that meets the specified requirements, 
   \code{oa.design} returns a full factorial, replicated for enough residual degrees of freedom, if necessary. 
   If \code{oa.design} has not found an array smaller than the full factorial, it is 
   absolutely possibly that a smaller array does exist nevertheless. It may be worth 
   while checking with oacat whether an appropriate smaller array can be found by 
   combining some of the parent arrays listed there (looking for a design with a few 
   factors in 5 runs, you may e.g. call \code{oacat[oacat$n5>0,]$name} in order to see the 
   names of more promising candidate arrays for combination, or you may also want to look 
   up arrays with \code{n25>0} subsequently.
   
   With version 0.9-18 of the package, 
   the possibility for an automatic allocation of columns for improved design 
   performance has been implemented. This is not switched on per default because of performance reasons. 
   However, the package always issues a warning to remind users that an automatic unoptimized design 
   can be quite far from ideal. 
   If optimization is activated, the first step is selection of an array, 
   either explicitly by the user (option \code{ID}) or automatically (unoptimized) 
   according to the required combination of factors. 
   If option \code{columns="min3"} is chosen, 
   the function tries to allocate the experimental 
   factors to columns of the selected array such that aliasing between main effects and 2-factor 
   interactions is kept to a minimal degree, 
   minimizing the number of generalized words of length 3 according to Xu and Wu (2001). 
   If option \code{columns="min3.rela"} is chosen, the same approach is taken, but with 
   \emph{relative} number of generalized words according to Groemping (2011).
   The default \code{columns="order"} 
   allocates factors from left to right (option \code{columns="order"}), which 
   is what most software does (but what is not necessarily good). 
   If only very few columns of an array are used, it may be useful to 
   specify the most demanding optimization (which may sometimes take very long) 
   with option \code{columns="min34"}, 
   which requests that the number of words of generalized length 4 is also minimized (e.g. useful 
   if there are several designs without any generalized words of length 3).
   Again, if option \code{columns="min34.rela"} is chosen, the same approach is taken, but with 
   \emph{relative} number of generalized words according to Groemping (2011).
   Finally, if option \code{columns="minRelProjAberr"} is chosen, minimum relative 
   projection aberration according to Groemping (2011) is applied ((a): maximize generalized 
   resolution, (b): minimize total relative number of shortest words, (c) rank designs 
   according to relative projection frequency table (obtainable with P3.3 or P4.4, depending on resolution) 
   and (d) resolve ties by looking at absolute number of length 4 words in case of resolution III).

   
   WARNING: Usually, it is recommended to investigate the properties of a design automatically 
   created by function \code{oa.design} before starting experimentation. 
   While all designs can estimate main effects \emph{in the absence of interactions}, 
   the presence of interactions may render some designs useless or even dangerous. 
   Deliberate choice of columns different from the default may improve a design 
   (an example for this will be added shortly)!
   
   Mathematical comment on the expansion example: 
   There are 720 different ways to expand the unique \code{L18.3.6.6.1} 
   into an \code{L18.2.1.3.7}, depending on which row of the replacement design 
   \code{nest.des} is assigned to which level of the 6 level factor. 
   According to Eric Schoen (personcal communication), all the resulting children 
   are isomorphic to each other and are also isomorphic to the Taguchi L18. 
   (This statement holds for qualitatitve factors only; there are 
   more different possibilities for arrays for quantitative 
   3 level factors, since arbitrary 
   relabelling of the levels is no longer isomorphic). 
   To see isomorphism of two designs is not easy; in the example, \code{nest.des} 
   has been prepared such that it is easy to see isomorphism of the resulting 
   child to the Taguchi \code{L18}: \code{L18} is reproduced by 
   assigning the first row of \code{nest.des} to level 1 etc., 
   except for a swap of columns \code{G} and \code{H}. 
   
   Schoen (2009) identified three different isomorphism classes of 
   orthogonal arrays for one 2 level factor and seven 3 level factors. 
   Note that Schoen classifies the isomorphism class of the Taguchi \code{L18} as worst among the 
   three classes, based on the fact that it has more subsets of three factors with bad 
   number of generalized three letter words than the other classes (\code{\link{P3.3}} pattern).
   On the good side, the Taguchi array class also has the most orthogonal subsets of three factors. 
   In most cases, the difference will not be practically relevant.
}
\value{\code{oa.design} returns a data frame of S3 class \code{\link{design}} 
    with attributes attached. 
    
   In the data frame itself, the experimental factors are all stored as R factors. \cr
   For factors with 2 levels, \code{\link{contr.FrF2}} contrasts (-1 / +1) are used. \cr
   For factors with more than 2 numerical levels, polynomial contrasts are used 
   (i.e. analyses will per default use orthogonal polynomials). \cr
   For factors with more than 2 categorical levels, the default contrasts are used.\cr
   Future versions will most likely allow more user control about the type of 
   contrasts to be used.

   The \code{desnum} and \code{run.order} attributes of class \code{\link{design}} are 
   as usual. In the \code{design.info} attribute, the following elements are specific for 
   this type of designs:
   \item{type}{is \code{oa} (unless no special orthogonal array is found, in which case 
      a full factorial is created instead, cf. \code{\link{fac.design}} for its \code{design.info} attribute),} 
   \item{nlevels}{vector containing the number of levels for each factor}
   \item{generating.oa}{contains information on the generating orthogonal array,}
   \item{selected.columns}{contains information, which column of the orthogonal array underlies which factor,}
   \item{origin}{contains the respective attribute of the orthogonal array,} 
   \item{comment}{contains the respective attribute of the orthogonal array,} 
   \item{residual.df}{contains the requested residual degrees of freedom for a main effects model.} 
   Other information is generic, like documented for class \code{\link{design}}. 
   
   Function \code{origin} returns the origin attribute of the orthogonal array \code{ID}, 
   functions \code{\link{comment}} and \code{"comment<-"} from package \pkg{base} 
   return and set the comment attribute.
}
\author{ Ulrike Groemping }
\references{ 
   Groemping, U. (2011). Relative projection frequency tables for orthogonal arrays. 
      Report 1/2011, \emph{Reports in Mathematics, Physics and Chemistry} \url{http://www1.beuth-hochschule.de/FB_II/reports/welcome.htm}, 
      Department II, Beuth University of Applied Sciences, Berlin. 
      
  Hedayat, A.S., Sloane, N.J.A. and Stufken, J. (1999) \emph{Orthogonal Arrays: Theory and Applications}, Springer, New York. 
  
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
  
  Schoen, E. (2009). All orthogonal arrays with 18 runs. \emph{Quality and Reliability Engineering International} \bold{25}, 467--480. 

  Xu, H.-Q. and Wu, C.F.J. (2001). Generalized minimum aberration for asymmetrical
      fractional factorial designs. \emph{Annals of Statistics} \bold{29}, 1066--1077.
      
}
\note{ This package is currently under intensive development. Substantial changes are to be expected in the near future.}
\seealso{ See Also \code{\link[FrF2]{FrF2}}, \code{\link{fac.design}}, \code{\link[FrF2]{pb}} }
\examples{
  ## smallest available array for 6 factors with 3 levels each
  oa.design(nfactors=6,nlevels=3)
  ## level combination for which only a full factorial is (currently) found
  oa.design(nlevels=c(4,3,3,2))
  ## array requested via factor.names
  oa.design(factor.names=list(one=c("a","b","c"), two=c(125,275), three=c("old","new"), four=c(-1,1), five=c("min","medium","max")))
  ## array requested via character factor.names and nlevels (with a little German lesson for one two three four five)
  oa.design(factor.names=c("eins","zwei","drei","vier","fuenf"),nlevels=c(2,2,2,3,7))
  ## array requested via explicit name, Taguchi L18
  oa.design(ID=L18)
  ## array requested via explicit name, with column selection
  oa.design(ID=L18.3.6.6.1,columns=c(2,3,7))
  ## array requested with nruns, not very reasonable
  oa.design(nruns=12, nfactors=3, nlevels=2)
  ## array requested with min.residual.df
  oa.design(nfactors=3, nlevels=2, min.residual.df=12)
  
  ## examples showing alias structures and their improvment with option columns
  plan <- oa.design(nfactors=6,nlevels=3)
  plan
     ## generalized word length pattern
     length3(plan)
     ## length3 (first element of GWP) can be slightly improved by columns="min3"
     plan <- oa.design(nfactors=6,nlevels=3,columns="min3")
     summary(plan)  ## the first 3-level column of the array is not used
     length3(plan)
  plan <- oa.design(nlevels=c(2,2,2,6))
     length3(plan)
  plan.opt <- oa.design(nlevels=c(2,2,2,6),columns="min3") ## substantial improvement
     length3(plan.opt)   
     length4(plan.opt)   
  ## visualize practical relevance of improvement:
     ## for optimal plan, all 3-dimensional projections are full factorials
  plot(plan, select=1:3)
  plot(plan, select=c(1,2,4))
  plot(plan, select=c(1,3,4))
  plot(plan, select=2:4)
  plot(plan.opt, select=1:3)
  plot(plan.opt, select=c(1,2,4))
  plot(plan.opt, select=c(1,3,4))
  plot(plan.opt, select=2:4)
  

  ## The last example:
  ## generate an orthogonal array equivalent to Taguchi's L18
  ## by combining L18.3.6.6.1 with a full factorial in 2 and 3 levels
  show.oas(nruns=18, parents.only=FALSE)
       ## lineage entry leads the way:
           ## start from L18.3.6.6.1 
           ## insert L6.2.1.3.1 for the 6 level factor
  ## prepare the parent 
   parent.des <- L18.3.6.6.1
   colnames(parent.des) <- c(Letters[3:8], "comb")   
       ## column comb will create the first two columns of the target design
  ## 6-level design can be created by fac.design or expand.grid
   nest.des <- as.matrix(expand.grid(1:3,1:2))[c(1:3,5,6,4),c(2,1)]  
        ## want first column to change most slowly
        ## want resulting design to be easily transformable into Taguchi L18
        ## see mathematical comments in section Details
   colnames(nest.des) <- c("A","B")
  ## do the expansion (see mathematical comments in section Details)
  L18.2.1.3.7.manual <- cbind(nest.des[parent.des[,"comb"],], parent.des)[,-9]
  L18.2.1.3.7.manual <- L18.2.1.3.7.manual[ord(L18.2.1.3.7.manual),]  ## sort array
      rownames(L18.2.1.3.7.manual) <- 1:18
        ## (ordering is not necessary, just **tidy**)
  ## prepare for using it with function oa.design
  attr(L18.2.1.3.7.manual, "origin") <- 
      c(show.oas(name="L18.2.1.3.7", parents.only=FALSE,show=0)$lineage, 
        "unconventional order")
  class(L18.2.1.3.7.manual) <- c("oa", "matrix")
  comment(L18.2.1.3.7.manual) <- "Interaction of first two factors estimable"
     ## indicates that first two factors are full factorial from 6-level factor
  origin(L18.2.1.3.7.manual)
  comment(L18.2.1.3.7.manual)
  L18  ## Taguchi array
  L18.2.1.3.7.manual  ## manually expanded array
  oa.design(L18.2.1.3.7, randomize=FALSE)
        ## automatically expanded array
  P3.3(L18.2.1.3.7.manual)  ## length 3 pattern of 3 factor projections
                  ## this also identifies the array as isomorphic to L18
                  ## according to Schoen 2009
  ## the array can now be used in oa.design, like the built-in arrays
  oa.design(ID=L18.2.1.3.7.manual,nfactors=7,nlevels=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
