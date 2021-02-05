\name{oacat}
\alias{oacat}
\alias{oacat3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Frames That List Available Orthogonal Arrays
}
\description{
These data frames hold the lists of available orthogonal arrays, 
except for a few structurally equivalent additional arrays known 
as Taguchi arrays (L18, L36, L54). Arrays in  
in oacat are mostly from the Kuhfeld collection, 
those in oacat3 from some other sources.
}
\usage{
oacat
oacat3
}

\details{
   The data frames hold a list of orthogonal arrays, as described in Section \dQuote{value}. 
   Inspection of these arrays can be most easily done with function \code{\link{show.oas}}. 
   Some of the listed arrays are directly accessible through their names (\dQuote{parent} arrays, 
   also listed under \code{\link{arrays}}) or 
   are full factorials the construction of which is obvious. Others 
   can be constructed as \dQuote{child} arrays from the parent and full factorial 
   arrays, using a so-called \code{lineage} which is also included as a column 
   in data frame \code{oacat}. Most of the listed arrays have been taken 
   from Kuhfeld 2009. Exceptions: The three arrays \code{L128.2.15.8.1}, 
   \code{L256.2.19} and \code{L2048.2.63}) have been taken from Mee 2009; these 
   are irregular resolution IV or V arrays for which all main effects can be 
   orthogonally estimated even in the presence of interactions, or even all 2fis 
   can be orthogonally estimated, provided there are no higher order effects.
   
   Note that most of the arrays in \code{oacat}, per default, are guaranteed to 
   orthogonally estimate 
   all main effects, \bold{provided all higher order effects are negligible} 
   (again, the Mee arrays are an exception). This can be 
   a very severe limitation, of course, and arbitrary strong biases can distort the 
   estimates even of main effects, if this assumption is violated. 
   It is therefore strongly recommended to inspect 
   the quality of an orthogonal array quite closely before deciding to use it 
   for experimentation. Some functions for inspecting arrays are provided in the 
   package (cf. \code{\link{generalized.word.length}}).
   
   The data frame \code{oacat3} contains stronger arrays that have at least the main 
   effects unconfounded with two-factor interactions. If only these are of interest, 
   function \code{\link{show.oas}} can be restricted to strong arrays 
   by option \code{Rgt3=TRUE}. Function \code{\link{oa.design}} will use a strong 
   array, if possible. It may also be worthwhile to check whether expansive replacement 
   of a strong array with a full factorial can yield a suitable strong array 
   (for an example, see function \code{\link{expansive.replace}}); this is not 
   automatically checked and can only be done by the user.
}
\value{
   The data frames contain the columns \code{name}, \code{nruns}, \code{lineage} 
   and further columns \code{n2} to \code{n72}; furthermore, some columns with 
   calculated metrics are included. \code{name} holds the name of the 
   array, \code{nruns} its number of runs, and \code{lineage} the way the array can 
   be constructed from other arrays, if applicable. The columns \code{n2} to \code{n72} 
   each contain the number of factors with the respective number of levels.
   
   The logical columns \code{ff}, \code{regular.strict} and \code{regular} indicate a  
   full factorial and a regular design in the strict or weak sense, respectively
   (strict: all ARFT entries are 0 or 1, defined as \dQuote{R^2 regular} in Groemping and Bailey (2016); 
   weak: all SCFT entries are 0 or 1, defined as \dQuote{CC regular} in 
   Groemping and Bailey (2016)). For R^2 regularity, it suffices to check all full resolution factor sets, 
   i.e., sets of j factors with resolution j; for CC regularity, this is conjectured to be also true.
   The entries in column \code{regular} are based on that conjecture (and for some larger designs, 
   even those checks were not completed);
   thus, designs denominated as CC regular might prove otherwise if the conjecture 
   proves wrong, and for larger designs also for unchecked full resolution factor sets of higher dimensions).
   
   Column \code{SCones} contains the number of worst case (=1)
   squared canonical correlations for the number of R factor subsets, with 
   R the resolution; if this number is 0, main effects can be considered 
   to have partial confounding only with any interactions of up to R-1 factors. 
   \code{GR}, \code{GRind}, \code{maxAR}
   and \code{maxSC} contain the generalized resolution in two versions, 
   the maximum average R^2 and the maximum squared canonical correlation.
   
   \code{dfe} contains the error degrees of freedom of a main effects model, 
   if all columns of the array are populated; if this is 0, the design is saturated. 
   \code{A3} to \code{A8} contain the numbers of words of lengths 3 to 8. 
   More information on these metrics can be found in 
   \code{\link{generalized.word.length}} and the literature therein. 
   
   The design names also indicate the number of runs and the numbers of factors: 
   The first portion of each array name (starting with L) indicates the number of runs, 
   each subsequent pair of numbers indicates a number of levels together with the frequency with which it occurs.
   For example, \code{L18.2.1.3.7} is an 18 run design with one factor with 
   2 levels and seven factors with 3 levels each. 
   
   The columns \code{gmarule} and \code{sgmarule} refer to the implementation of 
   known rules from the literature that certain subsets of array columns have 
   generalized minimum aberration (Butler 2005); if such a subset is requested, 
   there is no message of caution even if the array columns are used with 
   \code{column="order"} instead of optimizing the selection. Currently, 
   only the rules from Butler (2005) are implemented; hopefully, more rules will be added 
   in the future.
   
   The column \code{lineage} deserves particular attention for \code{oacat} (always empty for \code{oacat3}): 
   it is an empty string, if the design is directly available and can be accessed via its name, or if the design 
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
   
   Most of the orthogonal arrays from \code{oacat}, 
   especially when using all columns for experimentation, 
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
  Agrawal, V. and Dey, A. (1983). Orthogonal resolution IV designs for some asymmetrical factorials. 
  \emph{Technometrics} \bold{25}, 197--199.

  Brouwer, A. Small mixed fractional factorial designs of strength 3. \url{https://www.win.tue.nl/~aeb/codes/oa/3oa.html#toc1} accessed March 1 2016
  
  Brouwer, A., Cohen, A.M. and Nguyen, M.V.M. (2006). Orthogonal arrays of strength 3 and small run sizes. \emph{Journal of Statistical Planning and Inference} \bold{136}, 3268--3280.

  Butler, N.A. (2005). Generalised minimum aberration construction results for symmetrical orthogonal arrays. \emph{Biometrika} \bold{92}, 485 -- 491.

  Eendebak, P. and Schoen, E. Complete Series of Orthogonal Arrays. \url{http://pietereendebak.nl/oapage/} accessed March 1 2016
  
  Groemping, U. and Bailey, R.A. (2016). Regular fractions of factorial arrays. In: 
  \emph{mODa 11 -- Advances in Model-Oriented Design and Analysis}. 
    Cham: Springer International Publishing.

  Kuhfeld, W. (2009). Orthogonal arrays. Website courtesy of SAS Institute 
  \url{https://support.sas.com/techsup/technote/ts723b.pdf} and references therein.
  
  Mee, R. (2009). \emph{A Comprehensive Guide to Factorial Two-Level Experimentation}. 
    New York: Springer.
    
  Nguyen, M.V.M. (2005). \emph{Journal of Statistical Planning and Inference} \bold{138},
    220--233.

  Nguyen, M.V.M. (2008). Some new constructions of strength 3 mixed orthogonal arrays. \emph{Journal of Statistical Planning and Inference} \bold{138},
    220--233.

  Sloane, N. Orthogonal Arrays. \url{http://neilsloane.com/oadir/} accessed March 1 2016
  
  }
\examples{
   head(oacat)
   
   sapply(oacat3$name, function(nn) unlist(attributes(get(nn))[c("origin", "comment")]))
   
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
