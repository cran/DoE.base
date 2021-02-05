\name{planor2design}
\alias{data2design}
\alias{planor2design}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert matrix, data frame or object of class planordesign 
to object of class design }
\description{
  function to convert matrix, data frame or object of class planordesign to class design 
  (allowing use of convenience functions, particularly plotting with mosaic plots)
}
\usage{
data2design(x, quantitative = rep(FALSE, ncol(x)), ...)
planor2design(x, ...)
}
\arguments{
  \item{x}{ an object of class \code{data.frame}, \code{matrix} 
    (function \code{data2design}) or \code{planordesign} }
  \item{quantitative}{ a logical vector, indicating which factors are quantitative;\cr 
     defaults to all factors being qualitative}
  \item{\dots}{currently not used}
}
\details{
    For matrices and data frames, an unreplicated and unrandomized design is 
    assumed (not crucial, but the some entries of the \code{design.info} attribute 
    and the entire \code{run.order} attribute of the result will 
    be wrong otherwise). Per default, all factors are treated as qualitative and 
    thus made into factors, if they are not factors already.
    
    Items of the S4 class \code{planordesign} are regular factorial designs 
    created by package \pkg{planor} (the designs itself is in the slot \code{design}). 
    Function \code{planor2design} transforms them into objects of the S3 class 
    \code{design}; currently, only the most basic information is included
    (nunit and the factor information); 
    the design is assumed to be unrandomized and unreplicated.   
}
\value{
     an object of class \code{\link{design}} with the \code{type} and 
     \code{creator} element of \code{design.info} given as \code{external} 
     or \code{planor}. For designs of type \code{planor}, the \code{generators} 
     element of the \code{design.info} 
     attribute contains the \code{designkey} from the original \code{planor} design.
}
\author{ Ulrike Groemping }
\seealso{ See also: the \code{planordesign} class of package \pkg{planor} (if that package is available), 
    \code{\link{design}}, \code{\link{plot.design}}
    }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
