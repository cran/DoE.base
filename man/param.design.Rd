\name{param.design}
\alias{param.design}
\alias{paramtowide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to generate Taguchi style parameter designs }
\description{
  The functions create parameter designs for robustness experiments 
  and signal-to-noise investigations with inner and outer arrays 
  and facilitate their formatting and data aggregation.
}
\usage{
param.design(inner, outer, direction="long", responses=NULL, ...)
paramtowide(design, constant=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inner}{ an experimental design for the inner array, 
                data frame of class \code{\link{design}}; 
                as function \code{param.design} does not 
                randomize, its runs should already be randomized }
  \item{outer}{ an experimental design for the outer array, 
                data frame of class \code{\link{design}} or vector}
  \item{direction}{character taking the values \code{"wide"} or \code{"long"};
                if long, the outer array runs for each inner array run are 
                listed underneath each other; if wide, they are listed within the 
                same row}
  \item{responses}{ \code{NULL}, or character vector of response names;\cr
                for the long format, there are no response columns if \code{responses} is \code{NULL}, 
                while response columns of the specified 
                name(s) containing \code{NA} values are generated if \code{responses} is specified;\cr 
                for the wide format, response columns are always generated 
                (one column per run of the outer array for each response): 
                if \code{responses} is \code{NULL}, 
                response columns are called \code{"y.1"}, \code{"y.2"} etc., 
                if \code{responses} is specified, a set of response columns 
                for each specified name is generated 
                }
  \item{design}{ parameter design in long format (created by function \code{param.design}) }
  \item{constant}{ character vector giving names of variables in addition to the 
                experimental factors of the inner array that are constant over 
                outer array runs for each inner array run}
  \item{\dots}{ currently not used }
}
\details{
  A parameter design is an experimental plan for setting the so-called \dQuote{control parameters} 
  such that they achieve the intended function and at the same time minimize the effects 
  of the so-called \dQuote{noise parameters}. Note that the word parameters is used 
  here in an engineering sense rather than in the typical sense it is used in statistics. 
  The experiment crosses the control factors in the \dQuote{inner array} with the 
  noise factors in the \dQuote{outer array}. 
  
  Function \code{param.design} uses function \code{\link{cross.design}} for 
  creating an inner/outer array crossed design. There will be data aggregation 
  functions for such designs in the near future.
  
  Note that designs created by \code{param.design} are not properly randomized, 
  as they are conducted in the Taguchi inner / outer array sense with the runs of 
  the inner array as whole plots and the factors of the outer array as split-plot 
  factors. With analysis methods that work on data aggregated over the outer array 
  this is appropriate. If analysis of control and noise factor designs is to be conducted 
  in a combined approach, the experiment should be fully randomized. This can be 
  done using function \code{\link{cross.design}} directly (cf. example there). 
}
\value{
  A data frame of class \code{\link{design}} with type \dQuote{param} or 
  \dQuote{FrF2.param} for long version inner/outer array designs, and 
  type of the inner array suffixed with \dQuote{.paramwide} for wide version 
  inner/outer array designs. The \code{design.info} attribute of such designs has 
  the following extraordinary elements:\cr
  
  In long format, there are the same elements as for type \code{crossed} from 
  function \code{\link{cross.design}}, and the additional elements 
  \code{inner} and \code{outer} that give the names of the inner and outer array 
  variables. 
  
  In wide format, the \code{design.info} information refers to the inner array, 
  the elements \code{cross... something} are no longer available (except for \code{cross.types}), 
  and the element \code{outer} contains the outer array design the rows of which 
  correspond to the response columns. The additional element \code{format} with value 
  \dQuote{innerouterWide} indicates the wide format (introduced for analogy to wide 
  repeated measures designs), and \code{responselist} shows the responses and their 
  respective columns in support of subsequent aggregation. Finally, 
  if there are variables that are neither experimental factors nor responses and 
  change within one run of the inner array, these are listed in \code{restlist}. 
}
\references{
NIST/SEMATECH e-Handbook of Statistical Methods, Section 5.5.6 (What are Taguchi Designs?), 
accessed August 11, 2009. \url{https://www.itl.nist.gov/div898/handbook/pri/section5/pri56.htm}
}
\author{ Ulrike Groemping }
\note{ This function is still experimental.
}
\seealso{ See Also \code{\link{cross.design}} }
\examples{
## It is recommended to use param.design particularly with FrF2 designs. 
## For the examples to run without package FrF2 loaded, 
## oa.design designs are used here.

## quick preliminary checks to try out possibilities
control <- oa.design(L18, columns=1:4, factor.names=paste("C",1:4,sep=""))
noise <- oa.design(L4.2.3, columns=1:3, factor.names=paste("N",1:3,sep=""))
## long
long <- param.design(control,noise)
## wide
wide <- param.design(control,noise,direction="wide")
wide
long

## use proper labelled factors
## should of course be as meaningful as possible for your data
fnc <- c(list(c("current","new")),rep(list(c("type1", "type2","type3")),3))
names(fnc) <- paste("C", 1:4, sep="")
control <- oa.design(L18, factor.names=fnc)
fnn <- rep(list(c("low","high")),3)
names(fnn) <- paste("N",1:3,sep="")
noise <- oa.design(L4.2.3, factor.names = fnn)
ex.inner.outer <- param.design(control,noise,direction="wide",responses=c("force","yield"))
ex.inner.outer              
## export e.g. to Excel or other program with which editing is more convenient
\dontrun{
   ### design written to default path as html and rda by export.design
   ### html can be opened with Excel
   ### data can be typed in 
   ### for preparation of loading back into R, 
   ###     remove all legend-like comment that does not belong to the data table itself
   ###     and store as csv
   ### reimport into R using add.response
   ### (InDec and OutDec are for working with German settings csv 
   ###     in an R with standard OutDec, i.e. wrong default option)
   getwd()  ## look at default path, works on most systems
   export.design(ex.inner.outer, OutDec=",")
   add.response("ex.inner.outer", "ex.inner.outer.csv", "ex.inner.outer.rda", InDec=",")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
\keyword{ array }% __ONLY ONE__ keyword per line
