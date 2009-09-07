\name{lm method for class design objects}
\alias{lm}
\alias{lm.default}
\alias{lm.design}
\alias{print.summary.lm.design}
\alias{print.lm.design}
\alias{summary.lm.design}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{lm method for class design objects}
\description{
  Method for automatic linear models for data frames of class design 
}
\usage{
lm(formula, ...)
\method{lm}{default}(formula, data, subset, weights, na.action, method = "qr", 
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
    contrasts = NULL, offset, ...)
\method{lm}{design}(formula, ..., response=NULL, degree=NULL, FUN=mean, 
    use.center=FALSE)
\method{print}{lm.design}(x, ...)
\method{summary}{lm.design}(object, ...)
\method{print}{summary.lm.design}(x, ...)
lm.design
summary.lm.design
}
\arguments{
  \item{formula}{ for the default method, cf. documentation for \code{\link[stats]{lm}} 
          in package \pkg{stats};cr
          for the class \code{\link{design}} method, a data frame of S3 class 
          \code{\link{design}} }
  \item{\dots}{ further arguments to functions \code{\link[stats]{lm}},
        \code{\link[stats:lm]{print.lm}} or \code{\link[stats:summary.lm]{print.summary.lm}}}
  \item{response}{ character string giving the name of the response variable 
          (must be among the responses of \code{x}; 
          for wide format repeated measurement or parameter designs, 
          \code{response} can also be among the column names of the \code{responselist} 
          element of the \code{design.info} attribute) \cr
          OR\cr
          integer number giving the position of the response in element 
          \code{response.names} of attribute \code{design.info}
          
          For the default \code{NULL}, the first available response variable 
          is used; for wide format designs, this is an aggregation of the variables 
          given in first column from the \code{responselist} 
          element of the \code{design.info} attribute of \code{x}.
          }
  \item{degree}{ degree for the formula; if \code{NULL}, 
        the default for the \code{formula} method is used }
  \item{FUN}{ function for the \code{\link{aggregate.design}} method; 
        this must be an unquoted function name;\cr
        This option is relevant for repeated measurement designs 
        and parameter designs in long format only}
  \item{use.center}{ logical indicating whether center points are to be used +
        in the analysis; relevant for \code{pb} and \code{FrF2} designs with 
        center points only;\cr
        the default \code{FALSE} allows application of analysis from package 
        \code{\link[FrF2]{FrF2-package}} (e.g. function \code{\link[FrF2]{MEPlot}})
        }
  \item{x}{ object of class \code{lm} or \code{summary.lm},\cr 
        for \code{lm.default} like in \code{\link[stats]{lm}}  }
  \item{object}{ object of class \code{lm.design} created by function \code{lm.design}}
  \item{lm.design}{a class that is identical in content to class \code{lm}; 
        its purpose is to call a specific print method that provides slightly more 
        detail than the standard printout for linear models}
  \item{summary.lm.design}{a class that is identical in content to class \code{summary.lm}; 
        its purpose is to call a specific print method that provides slightly more 
        detail than the standard summary for linear models}
  \item{data}{ like in \code{\link[stats]{lm}}}
  \item{subset}{ like in \code{\link[stats]{lm}}}
  \item{weights}{ like in \code{\link[stats]{lm}}}
  \item{na.action}{ like in \code{\link[stats]{lm}}}
  \item{method}{ like in \code{\link[stats]{lm}}}
  \item{model}{ like in \code{\link[stats]{lm}}}
  \item{y}{ like in \code{\link[stats]{lm}}}
  \item{qr}{ like in \code{\link[stats]{lm}}}
  \item{singular.ok}{ like in \code{\link[stats]{lm}}}
  \item{contrasts}{ like in \code{\link[stats]{lm}}}
  \item{offset}{ like in \code{\link[stats]{lm}}}
   }
\details{
    The \code{lm} method for class \code{\link{design}} 
    conducts a default linear model analysis for data frames of 
    class \code{\link{design}} that do contain at least one response.
    
    The intention for providing default analyses is to support convenient quick inspections. 
    In many cases, there will be good reasons to customize the analysis, for example by including 
    some but not all effects of a certain degree. Also, it may be statistically more wise 
    to work with mixed models for some types of design. \bold{The default analyses must not 
    be taken as a statistical recommendation!}

    The choice of default analyses 
    has been governed by simplicity: It uses fixed effects only and does either 
    main effects models (\code{degree=1}, default for \code{pb} and \code{oa} designs), 
    models with main effects and 2-factor interactions (\code{degree=2}, 
    default for most designs) or second order models (that contain 
    quadratic effects in addition to the 2-factor interactions, unchangeable default 
    for designs with quantitative variables). The \code{degree} parameter can be used 
    to modify the degree of interactions. If blocks are 
    present, the block main effect is always entered as a fixed effect without interactions. 
    
    Designs with center points are per default analysed without the center points; the main 
    reason for this is convenient usage of functions \code{\link[FrF2]{DanielPlot}}, 
    \code{\link[FrF2]{MEPlot}} and \code{\link[FrF2]{IAPlot}} from package \pkg{FrF2}. 
    With the \code{use.center} option, this default can be changed; in this case, significance 
    of the center point indicator implies that there are one or more quadratic effect(s) 
    in the model.
    
    Designs with repeated measurements (\code{repeat.only=TRUE}) and parameter 
    designs of long format are treated by \code{\link{aggregate.design}} 
    with aggregation function \code{FUN} (default: means are calculated) 
    before applying a linear model.
    
    For designs with repeated measurements (\code{repeat.only=TRUE}) and parameter 
    designs of wide format, the default is to use the first aggregated response, 
    if the design has been aggregated already. For a so far unaggregated design, 
    the default is to treat the design by \code{\link{aggregate.design}}, 
    using the function \code{FUN} (default: mean) and then use the first response. 
    The defaults can be overridden by specifying \code{response}: Here, 
    \code{response} can not only be one of the current responses but also a column name 
    of the \code{responselist} element of the \code{design.info} attribute of the 
    design (i.e. a response name from the long version of the design).

    The implementation of the formulae is not done in function \code{lm.design} itself 
    but based on the method for function formula (\code{\link{formula.design}}).
    
    The \code{print} methods prepend the formula and the number of experimental runs 
    underlying the analysis to the default printout. 
    The purpose of this is meaningful output in case a call from 
    inside function \code{\link{lm.design}} (method for function \code{\link[stats]{lm}} ) 
    does not reveal enough information, and another pointer that center points have been 
    omitted or repeated measurements aggregated over.
}
\value{
    The value for the \code{lm} functions is a linear model object, 
    exactly like for function \code{\link[stats]{lm}}, 
    except for the added class \code{lm.design}.
    
    The value of the \code{summary} function for class \code{lm.design} 
    is a linear model summary, exactly like documented in \code{\link[stats]{summary.lm}}, 
    except for the added class \code{lm.design}.
    
    The print functions return \code{NULL}; they are used for their side effects only.
   }
\note{ The package is currently subject to intensive development; most key functionality is now included. 
Some changes to input and output structures may still occur.}

\author{ Ulrike Groemping }
\seealso{ See also the information on class \code{\link{design}} 
    and its formula method \code{\link{formula.design}} }
\examples{
  oa12 <- oa.design(nlevels=c(2,2,6))
  ## add a few variables to oa12
  responses <- cbind(y=rexp(12),z=runif(12))
  oa12 <- add.response(oa12, responses)
  ## want treatment contrasts rather than the default
  ## polynomial contrasts for the factors 
  oa12 <- change.contr(oa12, "contr.treatment")
  linmod.y <- lm(oa12)
  linmod.z <- lm(oa12, response="z")
  linmod.y
  linmod.z
  summary(linmod.y)
  summary(linmod.z)
  
## examples with aggregation
  plan <- oa.design(nlevels=c(2,6,2), replications=2, repeat.only=TRUE)
  y <- rnorm(24)
  z <- rexp(24)
  plan <- add.response(plan, cbind(y=y,z=z))
  lm(plan)
  lm(plan, response="z")
  lm(plan, FUN=sd)
  ## wide format
  plan <- reptowide(plan)
  plan
  design.info(plan)$responselist
  ## default: aggregate variables for first column of responselist
  lm(plan)
  ## request z variables instead (z is the column name of response list)
  lm(plan, response="z") 
  ## force analysis of first z measurement only
  lm(plan, response="z.1")
  ## use almost all options 
  ## (option use.center can only be used with center point designs 
  ##          from package FrF2)
  summary(lm(plan, response="z", degree=2, FUN=sd))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
