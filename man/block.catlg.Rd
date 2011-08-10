\name{block.catlg}
\alias{block.catlg}
\alias{block.catlg3}
\alias{Yates}
\alias{Yates3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Catalogues for blocking full factorial 2-level and 3-level designs, 
and lists of generating columns for regular 2- and 3-level designs.
}
\description{
The block data frames hold Yates matrix column numbers for blocking full factorials 
with 2-level (up to 256 runs) and 3-level factors (up to 243 runs). 
The Yates lists translate these column numbers into effects.
}
\usage{
block.catlg
block.catlg3
Yates
Yates3
}

\details{
The constants documented here are used for blocking full factorial designs 
with function \code{\link{fac.design}}; \code{Yates} and \code{block.catlg} are 
also used by function \code{\link[FrF2]{FrF2}}.

The block data frames hold Yates matrix column numbers for blocking full factorials 
with 2-level (up to 256 runs) and 3-level factors (up to 243 runs). 
The Yates lists translate these column numbers into effects (see below).

Data frame \code{block.catlg} comes from Sun, Wu and Chen (1997).
Data frame \code{block.catlg3} comes from Cheng and Wu (2002, up to 81 runs) 
and has been derived from Hinkelmann and Kempthorne (2005, Table 10.6) 
for 243 runs. The blocking schemes from the papers are optimal; this has 
not been proven for the blocking scheme for 243 runs. 

   \code{Yates} is a user-visible constant that is useful in design construction: 
   
       \code{Yates} is a list of design column generators in Yates order (for 4096 runs), e.g. \code{Yates[1:8]} is identical to 
       
       \code{list(1,2,c(1,2),3,c(1,3),c(2,3),c(1,2,3))}.

   \code{Yates3} is a constant for 3-level designs, 
       for which there are coefficients rather than generating factor numbers in the list.
       

}

\author{
   Ulrike Groemping
}

\references{ 
Cheng, S.W. and Wu, C.F.J. (2002). Choice of Optimal Blocking Schemes in 
Two-Level and Three-Level Designs. \emph{Technometrics} \bold{44}, 269-277.

Hinkelmann, K. and Kempthorne, O. (2005). \emph{Design and analysis of experiments, Vol.2}. Wiley, New York.

Sun, D.X., Wu, C.F.J. and Chen, Y.Y. (1997). 
Optimal blocking schemes for 2\^p and 2\^(n-p) designs. \emph{Technometrics} \bold{39}, 
298-307. 

}
\keyword{ array }
\keyword{ design }
