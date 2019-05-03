\name{cross.design}
\alias{cross.design}
\title{Function to cross several designs}
\description{
 This function generates cartesian products of two or more experimental designs.
}
\usage{
cross.design(design1, design2, ..., randomize = TRUE, seed=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design1}{ a data frame of class \code{design} that restricted by certain 
       criteria (cf. details)
       
       if \code{design1} is not of class \code{design}, crossing will nevertheless work, 
       but the output object will be a data frame only without any design information; 
       there is no guaranteed support for this usage}
  \item{design2}{ a data frame of class \code{design} with the same restrictions 
       for design type as for \code{design1}; \cr
       can also be a vector if \code{...} is not used; \cr 
       cf. details for what is allowed regarding replications }
  \item{\dots}{ optional further data frames that are to be crossed; 
       they must be of class \code{design} with the 
       above-mentioned restrictions for design types; 
       the last element can also be a vector}
  \item{randomize}{ logical indicating whether randomization should take place 
      after crossing the designs }
  \item{seed}{ integer seed for the random number generator \cr
        In R version 3.6.0 and later, the default behavior of function \code{\link{sample}}
        has changed. If you work in a new (i.e., >= 3.6.-0) R version and want to reproduce 
        a randomized design from an earlier R version (before 3.6.0), 
        you have to change the RNGkind setting by\cr
        \code{RNGkind(sample.kind="Rounding")}\cr
        before running function \code{cross.design}.\cr
        It is recommended to change the setting back to the new recommended way afterwards:\cr
        \code{RNGkind(sample.kind="default")}\cr
        For an example, see the documentation of the example data set \code{\link{VSGFS}}.
        }
}
\details{
  Crossing is carried out recursively, following the \code{direct.sum} approach 
  from package \pkg{conf.design}. All but the last designs must fulfill various 
  criteria (cf. below). The last design to be crossed can also be a vector. 
  
  Designs to be crossed must not be a blocked, nor splitplot, nor crossed, folded 
  or Taguchi parameter design, nor designs in wide format. Furthermore, designs must 
  not contain responses (checked via the response.names element of \code{design.info}).

  If replications are desired, it is recommended to accomodate them in the last 
  design. Only the last design may have \code{repeat.only} replications. If the 
  last design has \code{repeat.only} replications and there are also proper replications 
  in earlier designs, a warning is thrown, but the \code{repeat.only} replications are 
  nevertheless accomodated; this is experimental and may not yield the expected 
  results under all circumstances.
}
\value{
  Function \code{cross.design} returns a simple data frame without design 
  information, if \code{design1} is not of class \code{design}.

  Otherwise, the value is a data frame of class \code{\link{design}} 
  with type \dQuote{crossed} and the following extraordinary elements: 
  \item{cross.nruns}{vector of run numbers of individual designs}
  \item{cross.nfactors}{vector of numbers of factors of individual designs}
  \item{cross.types}{vector of types of individual designs}
  \item{cross.randomize}{vector of logicals (randomized or not) of individual desigs}
  \item{cross.seed}{vector of seeds of individual designs}
  \item{cross.replications}{vector of numbers of replications of individual designs}
  \item{cross.repeat.only}{vector of logicals (repeat.only or not) of individual designs}
  \item{cross.map}{list with the map vectors for component designs of type \code{FrF2.estimable} }
  \item{cross.selected.columns}{\code{NULL} (if no \code{oa} type design) or list of column vectors for each design}
  \item{cross.nlevels}{list with the \code{nlevels} vectors for those component designs that have them}

  The standard elements are as usual, with \code{randomize} and \code{seed} referring to 
  the randomization within function \code{cross.design} itself (previous randomizations are shown 
  under \code{cross.randomize} and \code{cross.seed}). 
  
  The \code{nlevels} element of \code{design.info} is available only if it is 
  available for all designs that have been crossed (otherwise refer to the element \code{cross.nlevels}.

  The \code{creator} element of the \code{design.info} attribute consists is a 2-element list 
  containing\cr
  the list \code{original} of all the original creators and \cr
  the element \code{modify} that contains the call to \code{cross.design}. 
  
  If present, the \code{clear}, \code{ncube}, \code{ncenter}, \code{residual.df}, 
  \code{origin}, \code{comment}, \code{generating.oa} elements of \code{design.info} are vector-valued. 
  
  If present, the \code{generators} element of \code{design.info} is a list of character vectors. 
  
  If present, the \code{aliased} and \code{catlg.entry} elements of \code{design.info} are lists of lists. 
  
    }
\section{Warning}{
   Since R version 3.6.0, the behavior of function \code{\link{sample}} has changed 
   (correction of a biased previous behavior that should not be relevant for the randomization of designs). 
   For reproducing a randomized design that was produced with an earlier R version, 
   please follow the steps described with the argument \code{seed}.
}
\author{ Ulrike Groemping }
\note{ This function is still experimental.}
\seealso{ See Also \code{\link{param.design}} }
\examples{
   ## creating a Taguchi-style inner-outer array design
   ## with proper randomization
   ##   function param.design would generate such a design with all outer array runs 
   ##     for each inner array run conducted in sequence
   ##   alternatively, a split-plot approach can also handle control and noise factor 
   ##     designs without necessarily crossing two separate designs
   des.control <- oa.design(ID=L18)
   des.noise <- oa.design(ID=L4.2.3,nlevels=2,factor.names=c("N1","N2","N3"))
   crossed <- cross.design(des.control, des.noise)
   crossed
   summary(crossed)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
