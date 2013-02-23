\name{oacat}
\alias{oacat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
data frame that lists available orthogonal arrays, 
mostly from the Kuhfeld collection
}
\description{
This data frame holds the list of available orthogonal arrays, 
except for a few structurally equivalent additional arrays known 
as Taguchi arrays (L18, L36, L54).
}
\usage{
oacat
}

\details{
   The data frame holds a list of orthogonal arrays, as described in Section \dQuote{value}. 
   Inspection of these arrays can be most easily done with function \code{\link{show.oas}}. 
   Some of the listed arrays are directly accessible through their names (\dQuote{parent} arrays, 
   also listed under \code{\link{arrays}}) or 
   are full factorials the construction of which is obvious. Others 
   can be constructed as \dQuote{child} arrays from the parent and full factorial 
   arrays, using a so-called \code{lineage} which is also included as a column 
   in data frame \code{oacat}. Most of the listed arrays have been taken 
   from Kuhfeld 2009. Exceptions: The three arrays \code{L128.2.15.8.1}, 
   \code{L256.2.19} and \code{L2048.2.63}) have been taken from Mee 2009; these 
   are irregular resolution V arrays for which all main effects and 2fis can be 
   orthogonally estimated.
   
   Note that most of the arrays, per default, are guaranteed to orthogonally estimate 
   all main effects, \bold{provided all higher order effects are negligible} 
   (again, the Mee arrays are an exception). This can be 
   a very severe limitation, of course, and arbitrary strong biases can distort the 
   estimates even of main effects, if this assumption is violated. 
   It is therefore strongly recommended to inspect 
   the quality of an orthogonal array quite closely before deciding to use it 
   for experimentation. Some functions for inspecting arrays are provided in the 
   package (cf. \code{\link{generalized.word.length}}).
}
\value{
   The data frame contains the columns \code{name}, \code{nruns}, \code{lineage} 
   and further columns \code{n2} to \code{n72}. \code{name} holds the name of the 
   array, \code{nruns} its number of runs, and \code{lineage} the way the array can 
   be constructed from other arrays, if applicable. The columns \code{n2} to \code{n72} 
   each contain the number of factors with the respective number of levels.
   
   The design names also indicate the number of runs and the numbers of factors: 
   The first portion of each array name (starting with L) indicates the number of runs, 
   each subsequent pair of numbers indicates a number of levels together with the frequency with which it occurs.
   For example, \code{L18.2.1.3.7} is an 18 run design with one factor with 
   2 levels and seven factors with 3 levels each. 
   
   The column \code{lineage} deserves particular attention: it is an empty string, 
   if the design is directly available and can be accessed via its name, or if the design 
   is a full factorial (e.g. L6.2.1.3.1). Otherwise, the lineage entry is structured as follows: 
   It starts with the specification of a parent array, given as \code{levels1~no of factors; levels2~no of factors;}. 
   After a colon, there are one or more replacements, each enclosed in brackets; within each pair of brackets, 
   the left-hand side of the exclamation mark shows the to-be-replaced factor, the right-hand side the 
   replacement array that has to be used for replacing the levels of such a factor one or more times. For example, 
   the lineage for \code{L18.2.1.3.7} is \code{3~6;6~1;:(6~1!2~1;3~1;)}, which means that the parent array in 
   18 runs with six 3 level factors and one 6 level factor has to be used, and the 6 level factor has to be replaced 
   with the full factorial with one 2 level factor and one 3 level factor.
}
\author{
   Ulrike Groemping, with contributions by Boyko Amarov 
}
\section{Warning}{
   For designs with only 2-level factors, it is usually more wise to 
   use package \pkg{\link[FrF2:FrF2-package]{FrF2}}. Exceptions: The three arrays by 
   Mee (2009; cf. section \dQuote{Details} above) are very useful for 2-level factors. 
   
   Most of the orthogonal arrays, especially when using all columns for experimentation, 
   are guaranteed to orthogonally estimate all main effects, 
   \bold{provided all higher order effects are negligible}. 

   Make sure you understand the implications of using an orthogonal main effects design 
   for experimentation. In particular, for some designs there is a very severe 
   risk of obtaining biased main effect estimates, if there are some interactions between 
   experimental factors. The documentation for \code{\link{generalized.word.length}} and 
   examples section below that illustrate this remark. 
   Cf. also the instructions in section \dQuote{Details}).
}
\references{ 
  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute \url{http://support.sas.com/techsup/technote/ts723.html}.
  
  Mee, R. (2009). \emph{A Comprehensive Guide to Factorial Two-Level Experimentation}. 
    New York: Springer.
}
\examples{
   head(oacat)
}
\seealso{
    \code{\link{oa.design}} for using the designs from \code{oacat} in design creation\cr
    \code{\link{show.oas}} for inspecting the available arrays from \code{oacat}\cr
    \code{\link{generalized.word.length}} for inspection functions for array properties\cr
    \code{\link{arrays}} for a list of orthogonal arrays which are directly accessible 
    within the package 
}

\keyword{ array }
\keyword{ design }
