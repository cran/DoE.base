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
  \item{design1}{ a data frame of class \code{design} that is not a blocked design; 
       cf. details for what is allowed regarding replications;
       
       if \code{design1} is not of class \code{design}, crossing will nevertheless work, 
       but the output object will be a data frame only without any design information; 
       there is no guaranteed support for this usage}
  \item{design2}{ a data frame of class \code{design} that is not a blocked design; \cr
      can also be a vector if \code{...} is not used; \cr 
      cf. details for what is allowed regarding replications }
  \item{\dots}{ optional further data frames of class \code{design} 
      that are to be crossed; the last element can also be a vector}
  \item{randomize}{ logical indicating whether randomization should take place 
      after crossing the designs }
  \item{seed}{ seed for random number generation }
}
\details{
  Crossing is carried out recursively, following the \code{direct.sum} approach 
  from package \pkg{conf.design}. Crossing blocked designs is not supported. 
  The last crossed design can also be a vector. 
  
  If replications are desired, it is recommended to accomodate them in the last 
  design. Only the last design may have \code{repeat.only} replications. If the 
  last design has repeat.only replications and there are also proper replications 
  in earlier designs, a warning is thrown, but the repeat.only replications are 
  nevertheless accomodated; this is experimental and may not yield the expected 
  results under all circumstances.
}
\value{
  A data frame of class \code{\link{design}} with type \dQuote{crossed} and 
  the following extraordinary elements: 
  \item{cross.nruns}{vector of run numbers of individual designs}
  \item{cross.nfactors}{vector of numbers of factors of individual designs}
  \item{cross.types}{vector of types of individual designs}
  \item{cross.randomize}{vector of logicals (randomized or not) of individual desigs}
  \item{cross.seed}{vector of seeds of individual designs}
  \item{cross.replications}{vector of numbers of replications of individual designs}
  \item{cross.repeat.only}{vector of logicals (repeat.only or not) of individual designs}
  \item{cross.selected.columns}{NULL (if no oa type design) or list of column vectors for each design}
  \item{cross.creator}{list of creator components for each design}
  
  Some of the standard elements are list or vectors (e.g. \code{creator}) for crossed designs.
  
  A simple data frame without design information, if design1 is not of class \code{design}
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
   des.control <- oa.design(ID=L18.2.1.3.7)
   des.noise <- oa.design(ID=L4.2.3,nlevels=2,factor.names=c("N1","N2","N3"))
   crossed <- cross.design(des.control, des.noise)
   crossed
   summary(crossed)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
