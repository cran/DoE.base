\name{VSGFS}
\alias{VSGFS}

\title{ VSGFS: an experiment using an optimized orthogonal array in 72 runs }
\description{
  VSGFS: an experiment using an optimized orthogonal array in 72 runs
}
\usage{
VSGFS
}
%- maybe also 'usage' for other objects documented here.

\format{
   VSGFS is a data frame of class \code{\link{design}} with seven experimental factors and 
   three response variables. The data have been published in Vasilev et al. (2014).
  
   The experimental factors, all stored as R factors, with their levels are 
   \tabular{rll}{
    [,1]  \tab   Light       \tab   Lght-, Lght+\cr
    [,2]  \tab   ShakFreq    \tab   SF-, SF+\cr
    [,3]  \tab   InocSize    \tab   IS-, IS+\cr
    [,4]  \tab   FilledVol   \tab   FV-, FV0, FV+\cr
    [,5]  \tab   CM          \tab   CM-, CM+\cr
    [,6]  \tab   Carbo       \tab   Suc, Gluc, Mannit (Sucrose, Glucose, Mannitol)\cr
    [,7]  \tab   Cyclodextrin  \tab CD1, CD2, CD3, CD4 (beta, methyl-beta, triacetyl-beta, none)
    }
    
    The response variables, all stored as numerical variables, are
   \tabular{rll}{
    [,8]  \tab   Biomass     \tab   fresh weight in g\cr
    [,9]  \tab   Content     \tab   geraniol content in \eqn{\mu}g per g fresh weight\cr
    [,10] \tab   Yield       \tab   geraniol yield in \eqn{\mu}g per flask
    }
}
\details{
     The data set comes from an experiment that was created with function 
     \code{\link{oa.design}} using the array \code{\link{L72.2.43.3.8.4.1.6.1}}. 
     Column selection within the array was done with option \code{columns="min34"} 
     that picks the first set of columns obtained by function \code{\link{oa.min34}}. 
     (Optimization takes quite a while, so that the design was reconstructed later 
     by explicitly requesting the optimum set of columns.)
     
     Design creation and the experiment itself were conducted at the Fraunhofer 
     IME in Aachen by Nikolay Vasilev and colleagues. More detail on the experiment 
     and the variables can be found in Vasilev et al. (2014).
     
     The design was created under an R version before 3.6.0. For reproducing its 
     creation under R 3.6.0 and later, it is therefore necessary to switch 
     to the previous version of random number generation 
     (using the \code{RNGkind} function, see examples section). Note that the 
     previous discrete random uniform random number generator was not perfectly 
     uniform, especially for very large samples; for randomizing experiments 
     of typical sizes (like this one), this problem can be neglected.
    }
\references{ 
  Vasilev, N., Schmidt, C., Groemping, U., Fischer, R. and Schillberg, S. (2014). 
  Assessment of Cultivation Factors that Affect Biomass and Geraniol Production 
  in Transgenic Tobacco Cell Suspension Cultures. \emph{PLoS ONE} \bold{9}(8): 
  e104620. \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0104620}. 
}
\author{ Ulrike Groemping }
\seealso{ See also \code{\link{oacat}}, \code{\link{show.oas}}, \code{\link{oa.min34}}, 
    \code{\link{oa.design}}}
\examples{
## code used for creating the data frame
## option levordold is needed, because the level ordering 
## changed (improved) with version 0.27 
## and the design was originally created with an earlier version
\dontrun{
  if (getRversion()>='3.6.0') RNGkind(sample.kind="Rounding")
  VSGFS <- oa.design(ID=L72.2.43.3.8.4.1.6.1, 
   nlevels=c(2,2,2,3,2,3,4), 
   columns=c(4,22,37,46,41,48,52), 
   factor.names=list(Light=c("Lght-","Lght+"),
      ShakFreq=c("SF-","SF+"),
      InocSize=c("IS-","IS+"),
      FilledVol=c("FV-","FV0", "FV+"), 
      CM=c("CM-","CM+"),
      Sugar=c("Suc", "Gluc", "Mannit"),
      CDs=c("CD1","CD2","CD3","CD4")),
   seed = 9, randomize=TRUE, levordold=TRUE)
  if (getRversion()>='3.6.0') RNGkind(sample.kind="default")

response <- as.data.frame(scan(what=list(Biomass=0, Content=0, Yield=0), sep=" ")) 
5.80 24.13 139.98
4.97 16.96 84.28
1.28 21.08 26.99
6.83 17.71 120.95
0.86 21.28 18.30
4.09 18.86 77.14
2.39 17.08 40.81
4.05 17.84 72.23
5.84 17.74 103.61
3.38 18.08 61.11
0.40 24.82 9.93
3.86 18.10 69.88
4.58 21.29 97.49
6.29 17.32 108.91
4.85 15.50 75.17
1.25 23.14 28.92
2.09 18.43 38.51
4.26 17.75 75.62
4.78 18.53 88.57
6.63 17.82 118.14
0.77 18.79 14.47
4.89 18.23 89.15
4.53 17.69 80.11
4.27 18.05 77.07
3.90 15.84 61.77
4.15 18.73 77.74
3.95 17.12 67.63
6.92 16.86 116.68
5.00 16.96 84.80
0.37 21.79 8.06
2.36 19.57 46.18
5.11 18.13 92.66
4.69 17.38 81.50
1.20 19.57 23.49
1.76 17.98 31.65
6.21 17.03 105.76
5.63 15.71 88.43
3.98 18.42 73.32
2.31 19.38 44.76
1.86 18.41 34.25
4.22 17.93 75.68
2.77 17.17 47.55
0.40 23.10 9.24
1.42 18.89 26.83
1.54 17.44 26.86
5.03 17.40 87.53
8.70 14.41 125.38
3.21 19.29 61.92
5.36 18.46 98.93
3.87 16.89 65.35
7.70 18.60 143.20
1.71 17.67 30.22
4.38 16.79 73.54
2.24 19.61 43.92
3.79 19.35 73.35
3.09 18.67 57.70
1.57 17.64 27.70
5.43 18.45 100.19
3.86 17.09 65.96
7.44 19.07 141.85
5.87 17.13 100.53
2.65 17.51 46.39
6.14 15.85 97.34
6.32 14.80 93.56
5.19 16.53 85.78
5.09 17.30 88.04
4.40 17.52 77.08
1.68 21.89 36.78
0.93 23.06 21.45
1.79 22.88 40.95
2.64 18.38 48.52
7.78 16.22 126.19


VSGFS <- add.response(VSGFS, response)
VSGFS$Sugar <- relevel(VSGFS$Sugar, "Suc")
VSGFS$FilledVol <- relevel(VSGFS$FilledVol, "FV0")
VSGFS$FilledVol <- relevel(VSGFS$FilledVol, "FV-")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ design }
