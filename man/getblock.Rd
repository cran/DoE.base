\name{getblock}
\alias{getblock}
\alias{rerandomize.design}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to extract a block factor from a class design object 
or to rerandomize a class design object
}
\description{
Function getblock creates block factors for designs with replications, repeated 
measurements or split plot designs. Function rerandomize.design rerandomizes 
an experimental design.
}
\usage{
getblock(design, combine=FALSE, ...)
rerandomize.design(design, seed=NULL, ...)
}
\arguments{
  \item{design}{
    an object of class \code{\link{design}}, which is a design with replications 
    or repeated measurements or a split plot design
}
  \item{combine}{
    logical with default \code{FALSE}. It has an effect for replicated blocked 
    and splitplot designs only: If \code{TRUE}, all blocking information 
    is combined into a single factor. Otherwise, a 
    data frame with separate identifiers is returned.
}
  \item{seed}{
    integer number for initialization of the random number generator 
    (needed for repeatable rerandomization)
}
  \item{\dots}{
    currently not used
}
}
\details{
   The purpose of function \code{getblock} is to support users in doing their own analyses 
   accomodating randomization restrictions like blocking and split plotting 
   with R modeling functions. 
   
   The reason for including designs with proper replications is that these 
   are randomized in blocks by packages \pkg{DoE.base} and \pkg{FrF2} and partly 
   by \pkg{DoE.wrapper}. 
   While the package author does not consider it generally necessary to 
   analyze these with a block factor, 
   function getblock makes it easy for users with a different opinion 
   (or for situations for which time turns out to be important in spite of 
   not having explicitly blocked for time) to run an analysis with a block factor
   for the replication. 
   
   For unreplicated split plot designs, a whole plot identifier is returned; 
   the design itself contains the plot information via the settings of the 
   whole plot factors only. Thus, it may be useful to be able to create the plot 
   identifier.
   
   For replicated block or split plot designs, there is a randomization hierarchy 
   that will depend on how the experiment was actually conducted. Therefore, 
   a dataframe is generated the columns of which can be used in the appropriate way 
   by a statistically literate user. 
   
   Function \code{rerandomize.design} rerandomizes a design. This can be useful if 
   the user wants to obtain unblocked replications (packages \code{DoE.base} and \pkg{FrF2} 
   usually randomize in blocks on time) or wants to freely randomize the center point 
   position over the whole range of the experiment (or a block, respectively), 
   or if the user wants to also randomize the 
   blocks (rather than randomizing the block units to the experimental blocks 
   outside of the design).
}
\value{
   Function \code{getblock} returns \cr
   a single factor with block information (for split plot designs without replication 
   or replicated designs without randomization restrictions) \cr
   or a data frame with several blocking factors (for designs with randomization 
   restrictions and replication).
   
   Function \code{rerandomize.design} returns a class \code{design} object; 
   note that it will not be possible to add center points after re-randomization, 
   i.e. if required, center points have to be added before using function 
   the function.
}
\author{
   Ulrike Groemping 
}
\examples{
   ## a blocked full factorial design
   ff <- fac.design(nlevels=c(2,2,2,3,3,3), blocks=6, bbrep=2, wbrep=2, repeat.only=FALSE)
   getblock(ff)
   getblock(ff, combine=TRUE)
   rerandomize.design(ff)
   ff <- fac.design(nlevels=c(2,2,2,3,3,3), replications=2, repeat.only=FALSE)
   getblock(ff)  
   ff <- fac.design(nlevels=c(2,2,2,3,3,3), replications=2, repeat.only=FALSE)
   try(getblock(ff))
}

\keyword{ array }
\keyword{ design }
