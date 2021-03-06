\name{lm and aov method for class design objects}
\alias{lm}
\alias{lm.default}
\alias{lm.design}
\alias{coef.lm.design}
\alias{aov}
\alias{aov.default}
\alias{aov.design}
\alias{print.summary.lm.design}
\alias{print.lm.design}
\alias{summary.lm.design}
\alias{print.summary.aov.design}
\alias{print.aov.design}
\alias{summary.aov.design}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{lm and aov methods for class design objects}
\description{
  Methods for automatic linear models for data frames of class design 
}
\usage{
lm(formula, ...)
\method{lm}{default}(formula, data, subset, weights, na.action, method = "qr", 
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
    contrasts = NULL, offset, ...)
\method{lm}{design}(formula, ..., response=NULL, degree=NULL, FUN=mean, 
    use.center=NULL, use.star=NULL, use.dummies=FALSE)
aov(formula, ...)
\method{aov}{default}(formula, data = NULL, projections = FALSE, qr = TRUE,
    contrasts = NULL, ...)
\method{aov}{design}(formula, ..., response=NULL, degree=NULL, FUN=mean, 
    use.center=FALSE)
\method{print}{lm.design}(x, ...)
\method{summary}{lm.design}(object, ...)
\method{coef}{lm.design}(object, ...)
\method{print}{summary.lm.design}(x, ...)
\method{print}{aov.design}(x, ...)
\method{summary}{aov.design}(object, ...)
\method{print}{summary.aov.design}(x, ...)
lm.design
summary.lm.design
aov.design
summary.aov.design
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
  \item{use.center}{ \code{NULL} or logical indicating whether center points are to be used +
        in the analysis; \cr
        if \code{NULL}, the default is FALSE for \code{pb} and \code{FrF2} designs with 
        center points and TRUE for \code{ccd} designs; the option is irrelevant 
        for all other design types.\cr
        \code{FALSE} allows usage of simple analysis functions from package 
        \code{\link[FrF2]{FrF2-package}} (e.g. function \code{\link[FrF2]{IAPlot}})
        }
  \item{use.star}{ \code{NULL} or logical indicating whether the star portion of a CCD design 
        is to be used in the analysis (ignored for all other types of designs). \cr
        The default TRUE analyses the complete design. Specifying FALSE permits interim analyses of 
        the cube portion of a central composite design.
        }
  \item{use.dummies}{ logical indicating whether the error dummies of a Plackett Burman design 
        are to be used in the formula (ignored for all other types of designs). 
        }
  \item{projections}{ logical indicating whether the projections should be returned;
        for orthogonal arrays, these are helpful, as they provide the estimated 
        deviation from the overall average attributed to each particular factor; 
        it is not recommended to use them with unbalanced designs } 
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
    The \code{aov} and \code{lm} methods for class \code{\link{design}} 
    conduct a default linear model analysis for data frames of 
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
    \code{\link[FrF2:IAPlot]{MEPlot}} and \code{\link[FrF2]{IAPlot}} from package \pkg{FrF2}. 
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

    The implementation of the formulae is not done in functions \code{lm.design} or 
    \code{aov.design} themselves 
    but based on the method for function formula (\code{\link{formula.design}}).
    
    The \code{print} methods prepend the formula and the number of experimental runs 
    underlying the analysis to the default printout. 
    The purpose of this is meaningful output in case a call from 
    inside function \code{\link{lm.design}} or \code{\link{aov.design}} 
    (methods for functions \code{\link[stats]{lm}} and \code{\link[stats]{aov}} ) 
    does not reveal enough information, and another pointer that center points have been 
    omitted or repeated measurements aggregated over. The \code{coef} method for objects 
    of class \code{lm.design} suppresses \code{NA} coefficients, i.e. 
    returns valid coefficients only. For \code{aov} objects, this is the default 
    anyway.
}
\value{
    The value for the \code{lm} functions is a linear model object, 
    exactly like for function \code{\link[stats]{lm}}, 
    except for the added class \code{lm.design} in case of the method for class \code{design}, 
    and an added list element \code{WholePlotEffects} for split plot designs.
    
    The value for the \code{aov} functions is an aov object, 
    exactly like for function \code{\link[stats]{aov}}, 
    and an added list element \code{WholePlotEffects} for split plot designs.
   
    The value of the \code{summary} functions for class \code{lm.design} and 
    \code{aov.design} respectively
    is a linear model or aov summary, exactly like documented in \code{\link[stats]{summary.lm}} 
    or \code{\link[stats]{summary.aov}}, 
    except for the added classes \code{summary.lm.design} or \code{summary.aov.design}, 
    and an added list element \code{WholePlotEffects} (for \code{summary.lm.design}) 
    or attribute (for \code{summary.aov.design}) for split plot designs.
    
    The print functions return \code{NULL}; they are used for their side effects only.
   }
\note{ The package is currently subject to intensive development; most key functionality is now included. 
Some changes to input and output structures may still occur.}
\section{Warning}{ The generics for \code{lm} and \code{aov} replace the functions 
    from package \pkg{stats}. For normal use, this is not an issue, because their 
    default methods are exactly the functions from package \pkg{stats}.\cr 
    However, when programming on the language (or when using a package that relies on 
    such constructs), you may see unexpected results. 
    For example, \code{match.call(lm)} returns a different result, depending on 
    whether or not package \pkg{DoE.base} is loaded. This can be avoided by 
    explicitly requesting e.g. \code{match.call(stats::lm)}, which always works 
    in the same way.\cr
    Please report any additional issues that you may experience.
}
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
