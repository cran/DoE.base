\name{corrPlot}
\alias{corrPlot}
\title{
Function to Visualize Correlations Between Model Matrix Columns for an Experimental Design
}
\description{
Function corrplot plots absolute or squared values of correlations between model matrix 
columns of main effects up to three-factor interactions for factorial designs.
}
\usage{
corrPlot(design, scale = "corr", recode = TRUE, cor.out = TRUE, mm.out=FALSE,  
    main.only = TRUE, three = FALSE, run.order=FALSE,
    frml=as.formula(ifelse(three, ifelse(run.order, "~ run.no + .^3", "~ .^3"), 
                                   ifelse(run.order, "~ run.no + .^2", "~ .^2"))),
    pal = NULL, col.grid = "black", col.small = "grey", lwd.grid = 1.5, lwd.small = 0.5, 
    lty.grid = 1, lty.small = 3, cex.y = 1, cex.x = 0.7, x.grid = NULL,
    main = ifelse(scale == "corr", "Plot of absolute correlations", ifelse(scale == "R2",
         "Plot of squared correlations", 
         "Plot of absolute correlations of coefficient estimates")), 
    split = 0, ask = (split > 0), ...)
}
\arguments{
  \item{design}{
     a class \code{\link{design}} object, or an object that can be made into 
     that class by function \code{\link{data2design}}
  }
  \item{scale}{
     \code{"corr"} for absolute correlation of model matrix columns (default), \cr
     \code{"R2"} for squared correlation of model matrix columns, \cr
     \code{"corr.est"} for absolute correlation of coefficient estimates; \cr
     \code{"corr.est"} works for model matrices with full column rank only
     }
  \item{recode}{
     logical indicating whether or not to recode each column into normalized orthogonal 
     coding with function \code{\link{contr.XuWu}}
     }
  \item{cor.out}{
   logical; if TRUE (default), the correlation matrix is invisibly returned 
  }
  \item{mm.out}{
   logical; if TRUE (default: FALSE), the correlation matrix is invisibly returned, 
   with the model matrix attached to it as an attribute
  }
  \item{main.only}{
     logical; if TRUE (default), only correlations with main effects columns 
     are visualized, otherwise also those with two-factor interactions
    }
  \item{three}{
     logical; if FALSE (default), only two-factor interactions are included, 
     otherwise also three-factor interactions
  }  
  \item{run.order}{
     logical; if TRUE, the linear run order effect is included into the plot, 
     and main effects are shown on the horizontal axis; \cr
     used in conjunction with function \code{\link{rerandomize.design}}, 
     this option can help to find a suitable random order with reasonably low 
     correlation to the effects of interest.
  }  
  \item{frml}{
     the model formula; useful, if absolute correlation for the 
     coefficient estimates are desired in a situation 
     where a full model has a rank deficiency; for requirements on the formula, 
     see the Details section.
  }
  \item{pal}{
     \code{NULL} (default), or a color palette. If \code{NULL}, the color palette 
     depends on whether or not package \pkg{RColorBrewer} is available: 
     if so, the Blues palette with nine shades is used; otherwise, 
     a 10 level palette of heat colors augmented with white is used.
     The number of colors in \code{pal} determines the number of bins for plotting.
  }
  \item{col.grid}{
     color of the main grid lines
  }
  \item{col.small}{
     color of the small grid lines
   }
  \item{lwd.grid}{
     width of the main grid lines
  }
  \item{lwd.small}{
     with of the small grid lines
  }
  \item{lty.grid}{
     line type of the main grid lines
  }
  \item{lty.small}{
   line type of the small grid lines
  }
  \item{cex.y}{
  size of tick mark labels on vertical axis
  }
  \item{cex.x}{
  size of tick mark labels on horizontal axis
  }
  \item{x.grid}{
  vector of numerical positions for thicker vertical grid lines (default: \code{NULL} for none)
  }
  \item{main}{
  title
  }
  \item{split}{
  an integer number (default: 0, no split) of columns after which to split the horizontal axis; 
  if this number is nonzero and smaller than the number of columns to display, 
  several plots are created; note: the color legend needs attention, since it may differ between the 
  different plots, depending on the plot's range of values
  }
  \item{ask}{
  logical; if yes (default in case of splitting, otherwise not), the user is asked 
  to confirm creation of each new plot
  }
  \item{\dots}{
  additional arguments to function \code{\link{levelplot}}
}
}
\details{
The function can be used for visualizing confounding within an experimental design. 
It is strongly recommended to apply it to designs with columns coded in normalized 
orthogonal coding (\code{contr.XuWu}, \code{contr.XuWuPoly}, if applicable also \code{contr.FrF2}). 
Nevertheless, for factors with more than two levels, the picture shown depends on the 
choice of normalized orthogonal coding (see examples). Option \code{recode=FALSE} is there to allow 
to keep a suitably-chosen normalized orthogonal coding for each factor.

The function shows the absolute correlation or squared correlation between model matrix columns, 
or, on request and if possible, the absolute correlation between estimated coefficients (other than the intercept). 
In case the latter cannot be obtained for the full model, a model formula can be specified 
with option \code{frml}. Note that it is implicitly assumed that all main effects are included in the 
model formula, and for \code{main.only=FALSE} also all two-factor interactions.

For resolution III and higher designs, the vertical axis shows the main effects 
(and, if \code{main.only=FALSE}, also the two-factor interactions), the horizontal axis 
shows the two-factor interactions (and, if \code{three=TRUE}, also the three-factor 
interactions). For resolution II designs, the horizontal axis additionally shows 
the main effect columns (since they are correlated with other main effect columns).
For resolution VI and higher designs, the function stops with an error. 
For resolution V designs, the function shows correlations between two-factor interactions 
on the vertical axis and three-factor interactions on the horizontal axis, if both 
are activated.
The most interesting cases are designs of resolution up to IV. 

The diagonal of the correlation matrix is set to NA before plotting, in order to be able 
to better see differences in case there are only relatively low correlations.

With \code{scale="R2"}, and using normalized orthogonal coding, some sums of matrix entries 
coincide with contributions to generalized word counts (resolution II: main effects with main effects; 
resolution III: main effects with two-factor interactions; 
resolution IV: main effects with three-factor interactions; see Groemping and Xu (2014) for the background of this result 
and Groemping (2017).
}
\value{
The entire matrix of absolute correlations is output invisibly.
}
\references{

Groemping, U. (2017). Frequency Tables for the coding invariant quality assessment of factorial
designs. \emph{IISE Transactions} \bold{49}(5), 505--517.

Groemping, U. and Xu, H. (2014). Generalized resolution for orthogonal arrays. \emph{The Annals of Statistics} \bold{42}, 918--939.

The function works similarly  to \code{colormap} in package \pkg{daewr} (but offers significantly more choices). That package accompanies the following book: 

Lawson, J. (2013). Design and Analysis of Experiments with R. CRC, Boca Raton.
}
\author{
Ulrike Groemping, Berliner Hochschule fuer Technik
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{levelplot}}, ~~~
}
\examples{
## this is with the default contr.XuWu recoding
mat <- corrPlot(VSGFS)
round(mat, 2)

## NOT RECOMMENDED: force-keep non-normalized coding
corrPlot(VSGFS, recode=FALSE)  # not useful!

## custom normalized orthogonal coding
## that has correlations more concentrated on fewer columns
plan <- change.contr(VSGFS, "contr.XuWuPoly")
contrasts(plan$CDs) <- contr.FrF2(4)
corrPlot(plan, recode=FALSE)  # that is the purpose of recode=FALSE

corrPlot(VSGFS, main.only=FALSE, three=TRUE, cex.x=0.5, cex.y=0.5, split=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ design }
