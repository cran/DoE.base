\name{Class design and accessors}
\alias{design}
\alias{undesign}
\alias{redesign}
\alias{desnum}
\alias{desnum<-}
\alias{run.order}
\alias{run.order<-}
\alias{design.info}
\alias{design.info<-}
\alias{factor.names}
\alias{factor.names<-}
\alias{response.names}
\alias{response.names<-}
\alias{col.remove}
\alias{ord}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Class design and its accessor functions }
\description{
  Convenience functions to quickly access and modify attributes of data frames of the class 
  design; methods for the class are described in a separate help topic
}
\usage{
undesign(design)
redesign(design, undesigned)
desnum(design)
desnum(design) <- value
run.order(design)
run.order(design) <- value
design.info(design)
design.info(design) <- value
factor.names(design)
factor.names(design, contr.modify = TRUE, levordold = FALSE) <- value
response.names(design)
response.names(design, remove=FALSE) <- value
col.remove(design, colnames)
ord(matrix, decreasing=FALSE)
}
\arguments{
  \item{design}{ data frame of S3 class \code{design}. For the structures of design 
       objects, refer to the details section and to the value sections of the 
       functions that create them. }
  \item{undesigned}{an object that is currently not a design but could be (e.g. obtained by applying function \code{undesign}}
  \item{value}{ an appropriate replacement value:\cr
      a numeric version of the design matrix for function \code{desnum} (usage not encouraged for non-experts!)\cr
      a run order data frame for function \code{run.order} (usage not encouraged for non-experts!)\cr
      a list with appropriate design information for function \code{design.info} (usage not encouraged for non-experts!)\cr
      for function \code{`factor.names<-`} a character vector of new factor names (levels remain unchanged) 
      or a named list of level combinations for the factors, 
      like \code{factor.names} in function \code{\link{fac.design}} \cr
      for function \code{`response.names<-`} a character vector of response names referring to variables 
      which are already available in \code{design}
      }
  \item{contr.modify}{logical to indicate whether contrasts are to be modified to match the new levels;
      relevant for R factors only, not for numeric design variables;\cr 
      if \code{TRUE}, factors with 2 levels get -1/+1 contrasts, factors with more than two quantitative levels 
      get polynomial contrasts with scores identical to the factor levels, and factors with more than two 
      character levels get treatment contrasts; if FALSE, the contrasts remain unchanged from their previous state.\cr
      If solely the contrasts are to be changed, function \code{\link{change.contr}} is preferrable.}
  \item{levordold}{logical to indicate whether the level ordering should follow the old function behavior;\cr
      the new behavior (from version 0.27) is more plausible, in that the level ordering in the 
      new \code{factor.names} corresponds to the \code{factor.names} entry of the \code{design.info} 
      attribute; previously, the automatic level ordering of factor levels deviated from that order 
      which even led to a changed level order when reassigning exactly the \code{factor.names} element 
      of the \code{design.info} attribute}
  \item{remove}{logical to indicate whether responses not indicated in \code{value} are to be removed from 
      the design altogether. \cr
      If \code{TRUE}, the respective columns are deleted from the design. 
      Otherwise, the columns remain in the data frame but loose their status as a response variable. }
  \item{colnames}{character vector of names of columns to be removed from the design;
      design factors or the block factor cannot be removed; with non-numeric variables, 
      the \code{desnum} attribute of the design may have to be manually modified for 
      removing the respective columns in some cases.}
  \item{matrix}{matrix, data frame or also object of class design that is to be ordered column by column}
  \item{decreasing}{logical, indicates whether decreasing order or not (increasing is default)}
}
\details{
    Items of class \code{design} are data frames with attributes. They are generated 
    by various functions that create experimental designs (cf. see also section), and 
    by various utility functions for designs like 
    the above extractor function for class \code{design}.
    
    The data frame itself always contains the design in uncoded form. For many 
    design generation functions, these are factors. For designs for quantitative factors 
    (bbd, ccd, lhs, 2-level designs with center points), the design variables are numeric. 
    This is always indicated by the design.info element quantitative, for which all components 
    are \code{TRUE} in that case. 
    
    Generally, its attributes are \code{desnum}, 
    \code{run.order}, and \code{design.info}.\cr 
    Attribute \code{desnum} contains 
    a numeric coded version of the design. For factor design variables, the content of 
    \code{desnum} depends on the contrast information of the factors (cf. \code{\link{change.contr}} 
    for modifying this).\cr  
    Attribute \code{run.order} is a data frame 
    with run order information (standard order, randomized order, order with replication info), \cr
    and the details of \code{design.info} partly depend on the type of design. 
    
    \code{design.info} generally is a list with first element \code{type}, 
    further info on the design, 
    and some options of the design call regarding randomization and replication. 
    For almost all design types, elements include 
    \describe{
    \item{nruns}{number of runs (not adjusted for replications)}
    \item{nfactors}{number of factors}
    \item{factor.names}{named list, as can be handed to function \code{\link{oa.design}}}
    \item{replications}{the integer number of replications (1=unreplicated)} 
    \item{repeat.only}{logical indicating whether replications are only repeat runs but not 
         truly replicated}
    \item{randomize}{logical indicating whether the experiment was randomized} 
    \item{seed}{ integer seed for the random number generator \cr
        note that the randomization behavior has changed with R version 3.6.0;\cr
        section "Warning" provides information on reproducing randomized designs.}
    \item{response.names}{in the presence of response data only; 
        the character vector identifying response columns in the data frame} 
    \item{creator}{contains the call or 
    the list of menu settings within package \pkg{RcmdrPlugin.DoE} 
    that led to creation of the design.\cr
        Note that the randomization behavior has changed with R version 3.6.0;\cr
        section "Warning" provides information on reproducing randomized designs.}
    }
    
    For some design types, notably designs of types starting with \dQuote{FrF2} and 
    designs that have been created by combining other designs, 
    there can be substantial additional information available from the \code{design.info} 
    attribute in specialized situations. Detailed information on the structure of the 
    \code{design.info} attribute 
    can be found in the value sections of the respective functions. A tabular overview 
    of the available \code{design.info} elements is given on the authors homepage.
    
    Function \code{undesign} removes all design-related attributes from a class design 
    object; this may be necessary for making some independent code work on design objects. 
    (For example, function \code{\link[stats]{reshape}} from package \pkg{stats} does not 
    work on a class design object, presumably because of the specific extractor method for class \code{design}.) 
    Occasionally, one may also want 
    to reconnect a processed undesigned object to its design properties. This is the purpose of 
    function \code{redesign}.
    
    The functions \code{desnum}, \code{run.order}, and \code{design.info} extract 
    the respective attribute, i.e. e.g. function \code{design.info} 
    extracts the design information for the design. The corresponding assignment 
    functions should only be used by very experienced users, as they may 
    mess up things badly if they are used naively .
    
    The functions \code{factor.names} and \code{response.names} extract the 
    respective elements of the \code{design.info} attribute. The corresponding assignment 
    functions allow to change factor names and/or factor codes and to exclude or include 
    a numeric variable from the list of responses that are recognized as such by analysis 
    procedures. Note that the \code{response.names} function can (on request, not by default) 
    remove response variables from the data frame \code{design}. However, it is not directly able to 
    add new responses from outside the data frame \code{design}. This is what the 
    function \code{\link{add.response}} is for.
    
    Function \code{col.remove} removes columns from the design and returns the 
    design without these columns and an intact class \code{design} structure. 
}
\value{
    \item{desnum}{returns a numeric matrix, 
         the corresponding replacement function modifies a class design object }
    \item{run.order}{returns a 3-column data frame with standard and actual run order 
         as well as a run order with replication identifiers attached; 
         the corresponding replacement function modifies a class design object }
    \item{design.info}{returns the \code{design.info} attribute of the design; 
         the corresponding replacement function modifies a class design object }
    \item{factor.names}{returns a named list the names of which are the names of the 
         treatment factors of the design while the list elements are the vectors of levels 
         for each factor}
    \item{`factor.names<-`}{returns a class \code{design} object with modified 
         factor names information (renamed factors and/or changed factor levels);
         }
    \item{response.names}{returns a character vector of response names that 
         (names of numeric variables within the data frame \code{design} 
         that are to be treated as response variables ) ; 
         the corresponding replacement function modifies the design }
    \item{`response.names<-`}{returns a class \code{design} object with modified 
         response names information (add or remove numeric columns of the design 
         to or from set of response variables), and potentially response columns 
         removed from the design.
         }
    \item{col.remove}{returns a class \code{design} object with some columns 
         removed from both the design itself and the \code{desnum} attribute.
         Response columns may be removed, but factor or block columns may not.
         }
    \item{ord}{returns an index vector that orders the matrix or data frame; 
        for example, \code{design[ord(design),]} orders the design in increasing order with respect to the first,
        then the second etc. factor.}
}
\references{
    Chambers, J.M. and Hastie, T.J. (1993). \emph{Statistical Models in S}, 
    Chapman and Hall, London.
     }
\author{ Ulrike Groemping }
\note{
    Note that R contains a few functions that generate or work with an S class \code{design}, 
    which is cursorily documented in Appendix B of the white book (Chambers and Hastie 1993) 
    to consist of a data frame of R factors which will later be extended by numeric response columns. 
    Most class design objects as defined in packages \pkg{DoE.base} and \pkg{FrF2} are also 
    compatible with this older class \code{design}; they are not, however, as soon as quantitative 
    factors are involved, like for designs with center points in package \code{FrF2} or for most designs in 
    package \pkg{DoE.wrapper} (not yet on CRAN). If feasible with reasonable effort 
    and useful, functions for the class \code{design} documented here incorporate the functions 
    for the S class design (notably function \code{\link{plot.design}}). 

    This package is still under development; suggestions and bug reports are welcome.
    }
\section{Warning}{Function \code{\link{sample}} is used for the randomization 
    functionality of this package. With R version 3.6.0, the behavior of this 
    function has changed. Since the R version is not stored with a class \code{design} 
    object, please check carefully if a design you want to reproduce based on a given 
    \code{creator} or \code{seed} element of the \code{design.info} attribute 
    has the expected randomization order. 
    
    The randomization order of a design that was created with the default settings 
    under R version 3.6.0 or newer can only be reproduced with such a new R version. \cr
    If an R version 3.6.0 or newer is used for reproducing the randomization order 
    of a randomized design that was created with an R version before 3.6.0, 
    the \code{RNGkind} setting has to be modified: \cr
        \code{RNGkind(sample.kind="Rounding")}\cr
    activates the old behavior,\cr
        \code{RNGkind(sample.kind="default")}\cr
    switches back to the recommended new behavior.\cr
    For an example, see the documentation of the example data set \code{\link{VSGFS}}.
    }

\seealso{ See also the following functions known to produce objects of class 
    \code{design}: \code{\link[FrF2]{FrF2}}, \code{\link[FrF2]{pb}}, 
    \code{\link{fac.design}}, \code{\link{oa.design}}, 
    \code{\link[DoE.wrapper]{bbd.design}}, \code{\link[DoE.wrapper]{ccd.design}}, 
    \code{\link[DoE.wrapper]{ccd.augment}}, \code{\link[DoE.wrapper]{lhs.design}}, 
    as well as \code{\link{cross.design}}, \code{\link{param.design}}, and 
    utility functions in this package for reshaping designs.\cr 
    There are also special methods for class \code{design} (\code{\link{[.design}}, 
    \code{\link{print.design}}, \code{\link{summary.design}}, \code{\link{plot.design}})
    }
\examples{
oa12 <- oa.design(nlevels=c(2,2,6))


#### Examples for factor.names and response.names
  factor.names(oa12)
  ## rename factors
  factor.names(oa12) <- c("First.Factor", "Second.Factor", "Third.Factor")
  ## rename factors and relabel levels of first two factors
  namen <- c(rep(list(c("current","new")),2),list(""))
  names(namen) <- c("First.Factor", "Second.Factor", "Third.Factor")
  factor.names(oa12) <- namen
  oa12

  ## add a few variables to oa12
  responses <- cbind(temp=sample(23:34),y1=rexp(12),y2=runif(12))
  oa12 <- add.response(oa12, responses)
  response.names(oa12)
  ## temp (for temperature) is not meant to be a response 
  ## --> drop it from responselist but not from data
  response.names(oa12) <- c("y1","y2")

## looking at attributes of the design
  desnum(oa12)
  run.order(oa12)
  design.info(oa12)

## undesign and redesign
  u.oa12 <- undesign(oa12)
  str(u.oa12)  
  u.oa12$new <- rnorm(12)
  r.oa12 <- redesign(oa12, u.oa12)
## make known that new is also a response
  response.names(r.oa12) <- c(response.names(r.oa12), "new") 
## look at design-specific summary
  summary(r.oa12)
## look at data frame style summary instead
  summary.data.frame(r.oa12)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
