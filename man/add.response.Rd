\name{add.response}
\alias{add.response}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function to add response values to an experimental design}
\description{
This function allows to add numeric response variables to an experimental plan of 
class design. The responses are added both to the data frame and to its desnum 
attribute; the response.names element of the design.info attribute is updated - 
the function is still experimental. 
}
\usage{
add.response(design, response, rdapath=NULL, replace = FALSE, 
    InDec=options("OutDec")[[1]], ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{design}{a character string that gives the name of a class \code{\link{design}} 
       object, to which responses are to be added}
  \item{response}{EITHER 
  
      a numeric vector, numeric matrix or data frame with at least 
      one numeric variable (the treatment of these is explained in the details section)
      
      OR
      
      a character string indicating a csv file that contains the typed-in response values; 
      after reading the csv file with the csv version indicated in the \code{InDec} argument, 
      numeric variables from response will be added to the design as responses }
  \item{rdapath}{a character string indicating the path to a stored rda file that contains the 
      design }
  \item{replace}{ logical: TRUE implies that existing variables are overwritten in \code{design}; 
      cf. also the details section}
  \item{InDec}{ decimal separator in the external csv file; defaults to the 
      \code{OutDec} option (viewable under \code{options("OutDec"}), 
      and also governs whether the \code{csv}-file is read with \code{\link[utils:read.table]{read.csv}} or with 
      \code{\link[utils:read.table]{read.csv}}: 
      separator semicolon goes with decimal comma and triggers use of \code{\link[utils:read.table]{read.csv2}}, 
      separator comma goes with decimal point and trigggers use of \code{\link[utils:read.table]{read.csv}}. )}
  \item{\dots}{further arguments; currently not used}
}
\details{
      If \code{response} is a data frame or a matrix, responses are assumed to be 
      all the numeric variables that are neither factor names in \code{design} 
      (i.e. names of the \code{factor.names} element of the \code{design.info} attribute 
      nor column names of the \code{run.order} attribute. 
      
      If \code{design} already contains columns for the response(s), these are per default 
      overwritten if there are NA entries only in all such columns, but not otherwise. 
      
      The idea behind this function is as follows:
      After using \code{\link{export.design}} for storing an R work space with the 
      design object and either a csv or html file externally, 
      Excel or some other external software is used to type in experimental information. 
      The thus-obtained data sheet is saved as a csv-file and imported into R again (name provided 
      in argument \code{response}, and the design object with all attached information is 
      linked to the typed in response values using function \code{add.response}.
      
      Alternatively, it is possible to simply type in experimental results in R, both 
      using the R commander plugin (\pkg{RcmdrPlugin.DoE}) or simply function \code{\link[utils]{fix}}. 
      Copy-pasting into R from Excel is per default NOT possible, which has been the reason for programming this routine.
}
\value{
The value is a modified version of the argument object \code{design}, 
which remains an object of class \code{\link{design}} with the following modifications: 
\item{}{Response columns are added to the data frame}
\item{}{the same response columns are added to the desnum attribute}
\item{}{the \code{response.names} element of the \code{design.info} attribute 
     is added or modified}
}

\references{ ~put references to the literature/web site here ~ }
\author{ Ulrike Groemping }
\seealso{ See also \code{\link{export.design}} }
\examples{
       plan <- fac.design(nlevels=c(2,3,2,4))
       result <- rnorm(2*3*2*4)
       add.response(plan,response=result)
       ## direct use of rnorm() is also possible, but looks better with 48 
       add.response(plan,response=rnorm(48))
       
   \dontrun{   
       export.design(path="c:/projectA/experiments",plan)
       ## open exported file c:/projectA/experiments/plan.html
       ##        with Excel
       ## carry out the experiment, input data in Excel or elsewhere
       ##        store as csv file with the same name (or a different one, just use 
       ##        the correct storage name later in R), after deleting 
       ##        the legend portion to the right of the data area
       ##        (alternatively, input data by typing them in in R (function fix or R-commander)
       add.response(design="plan",response="c:/projectA/experiments/plan.csv",
                 rdapath="c:/projectA/experiments/plan.rda")
       ## plan is the name of the design in the workspace stored in rdapath
       ## assuming only responses were typed in
       ## should work on your computer regardless of system, 
       ##         if you adapt the path names accordingly
   }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }% __ONLY ONE__ keyword per line
