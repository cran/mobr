% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mobr.R
\name{rarefaction}
\alias{rarefaction}
\title{Rarefied Species Richness}
\usage{
rarefaction(
  x,
  method,
  effort = NULL,
  coords = NULL,
  latlong = NULL,
  dens_ratio = 1,
  extrapolate = FALSE,
  return_NA = FALSE,
  quiet_mode = FALSE,
  spat_algo = NULL,
  sd = FALSE
)
}
\arguments{
\item{x}{can either be a: 1) mob_in object, 2) community matrix-like
object in which rows represent plots and columns represent species, or 3)
a vector which contains the abundance of each species.}

\item{method}{a character string that specifies the method of rarefaction 
  curve construction it can be one of the following: 
\itemize{
    \item \code{'IBR'} ... individual-based rarefaction in which species
    are accumulated by randomly sampling individuals
    \item \code{'SBR'} ... sample-based rarefaction in which species are 
    accumulated by randomly sampling samples (i.e., plots). Note that within plot spatial 
    aggregation is maintained with this approach. Although this curve
    is implemented here, it is not used in the current version of the MoB framework
    \item \code{'nsSBR'} ... non-spatial, sampled-based rarefaction in which
    species are accumulated by randomly sampling samples that represent a 
    spatially random sample of individuals (i.e., no with-in plot spatial 
    aggregation). The argument \code{dens_ratio} must also be set otherwise 
    this sampling results in a curve identical to the IBR (see Details). 
    \item \code{'sSBR'} ... spatial sample-based rarefaction in which species 
    are accumulated by including spatially proximate samples first.
    \item \code{'spexSBR'}  ... spatially-explicit sample-based rarefaction
    in which species are accumulated as in \code{'sSBR'} but sampling
    effort is not measured by no. of samples, but by cumulative distance or
    cumulative area as specified by \code{'spat_algo'} (see details)
}}

\item{effort}{optional argument to specify what number of individuals, 
number of samples, or spatial sampling effort (i.e., cumulative distance
or area) depending on 'method' to compute rarefied richness as. If
not specified all possible values from 1 to the maximum sampling effort are
used}

\item{coords}{an optional matrix of geographic coordinates of the samples.
Only required when using the spatial rarefaction method and this information
is not already supplied by \code{x}. The first column should specify 
the x-coordinate (e.g., longitude) and the second coordinate should 
specify the y-coordinate (e.g., latitude)}

\item{latlong}{Boolean if coordinates are latitude-longitude decimal degrees}

\item{dens_ratio}{the ratio of individual density between a reference group
and the community data (i.e., x) under consideration. This argument is
used to rescale the rarefaction curve when estimating the effect of
individual density on group differences in richness.}

\item{extrapolate}{Boolean which specifies if richness should be extrapolated
when effort is larger than the number of individuals using the chao1 method.
Defaults to FALSE in which case it returns observed richness. Extrapolation
is only implemented for individual-based rarefaction 
(i.e., \code{method = 'indiv'})}

\item{return_NA}{Boolean defaults to FALSE in which the function returns the
observed S when \code{effort} is larger than the number of individuals or
number of samples (depending on the method of rarefaction). If set to TRUE
then NA is returned. Note that this argument is only relevant when
\code{extrapolate = FALSE}.}

\item{quiet_mode}{Boolean defaults to FALSE, if TRUE then warnings and other
non-error messages are suppressed.}

\item{spat_algo}{character string that can be either: \code{'kNN'},
\code{'kNCN'}, or \code{'convexhull'} for k-nearest neighbor, 
k-nearest centroid neighbor sampling, or convex-hull polygon calculation 
respectively. It defaults to k-nearest neighbor which is a 
more computationally efficient algorithm that closely approximates the 
potentially more correct k-NCN algo (see Details). Currently, \code{'kNN'} and
\code{'k-NCN'} are available for method \code{'ssBR'}, while \code{'kNN'}
\code{'convexhull'} are available for method \code{'spexSBR'}.}

\item{sd}{Boolean defaults to FALSE, if TRUE then standard deviation of 
richness is also returned using the formulation of Heck 1975 Eq. 2.}
}
\value{
A vector of rarefied species richness values
}
\description{
The expected number of species given a particular number of individuals or
samples under random and spatially explicit nearest neighbor sampling
}
\details{
The analytical formulas of Cayuela et al. (2015) are used to compute
  the random sampling expectation for the individual and sampled based
  rarefaction methods. The spatially constrained rarefaction curve (Chiarucci
  et al. 2009) also known as the sample-based accumulation curve (Gotelli and
  Colwell 2001) can be computed in one of two ways which is determined by the
  argument \code{spat_algo}. In the kNN approach each plot is accumulated by
  the order of their spatial proximity to the original focal cell. If plots
  have the same distance from the focal plot then one is chosen randomly to
  be sampled first. In the kNCN approach, a new centroid is computed after
  each plot is accumulated, then distances are recomputed from that new
  centroid to all other plots and the next nearest is sampled. The kNN is
  faster because the distance matrix only needs to be computed once, but the
  sampling of kNCN which simultaneously minimizes spatial distance and extent
  is more similar to an actual person searching a field for species. For both
  kNN and kNCN, each plot in the community matrix is treated as a starting
  point and then the mean of these n possible accumulation curves is
  computed.
  
  

For individual-based rarefaction if effort is greater than the number of
individuals and \code{extrapolate = TRUE} then the Chao1 method is used 
(Chao 1984, 1987). The code used to perform the extrapolation was ported
from \code{iNext::D0.hat} found at \url{https://github.com/JohnsonHsieh/iNEXT}. 
T. C. Hsieh, K. H. Ma and Anne Chao are the original authors of the
\code{iNEXT} package. 

If effort is greater than sample size and \code{extrapolate = FALSE} then the 
observed number of species is returned.

Standard deviation of richness can only be computed for individual based rarefaction 
and it is assigned as an attribute (see examples). The code for this
computation was ported from vegan::rarefy (Oksansen et al. 2022)
}
\examples{
data(inv_comm)
data(inv_plot_attr)
sad = colSums(inv_comm)
inv_mob_in = make_mob_in(inv_comm, inv_plot_attr, coord_names = c('x', 'y'))
# rarefaction can be performed on different data inputs
# all three give same answer
# 1) the raw community site-by-species matrix
rarefaction(inv_comm, method='IBR', effort=1:10)
# 2) the SAD of the community
rarefaction(inv_comm, method='IBR', effort=1:10)
# 3) a mob_in class object
# the standard deviation of the richness estimates for IBR may be returned
# which is helpful for computing confidence intervals
S_n <- rarefaction(inv_comm, method='IBR', effort=1:10, sd=TRUE)
attr(S_n, 'sd')
plot(1:10, S_n, ylim=c(0,8), type = 'n')
z <- qnorm(1 - 0.05 / 2)
hi <- S_n + z * attr(S_n, 'sd')
lo <- S_n - z * attr(S_n, 'sd')
attributes(hi) <- NULL
attributes(lo) <- NULL
polygon(c(1:10, 10:1),  c(hi, rev(lo)), col='grey', border = NA)
lines(1:10, S_n, type = 'o')
# rescaling of individual based rarefaction 
# when the density ratio is 1 the richness values are 
# identical to the unscaled rarefaction
rarefaction(inv_comm, method='IBR', effort=1:10, dens_ratio=1)
# however the curve is either shrunk when density is higher than 
# the reference value (i.e., dens_ratio < 1)
rarefaction(inv_comm, method='IBR', effort=1:10, dens_ratio=0.5)
# the curve is stretched when density is lower than the 
# reference value (i.e., dens_ratio > 1)
rarefaction(inv_comm, method='IBR', effort=1:10, dens_ratio=1.5)
# sample based rarefaction under random sampling
rarefaction(inv_comm, method='SBR')
\donttest{ 
# sampled based rarefaction under spatially explicit nearest neighbor sampling
rarefaction(inv_comm, method='sSBR', coords=inv_plot_attr[ , c('x','y')],
            latlong=FALSE)
# the syntax is simpler if supplying a mob_in object
rarefaction(inv_mob_in, method='sSBR', spat_algo = 'kNCN')
rarefaction(inv_mob_in, method='sSBR', spat_algo = 'kNN')
rarefaction(inv_mob_in, method='spexSBR', spat_algo = 'kNN')
}
}
\references{
Cayuela, L., N.J. Gotelli, & R.K. Colwell (2015) Ecological and 
 biogeographic null hypotheses for comparing rarefaction curves. Ecological
 Monographs, 85, 437-454. Appendix A: 
 http://esapubs.org/archive/mono/M085/017/appendix-A.php
 
Chao, A. (1984) Nonparametric estimation of the number of classes in a
 population. Scandinavian Journal of Statistics, 11, 265-270.
 
Chao, A. (1987) Estimating the population size for capture-recapture data
 with unequal catchability. Biometrics, 43, 783-791.
 
Chiarucci, A., G. Bacaro, D. Rocchini, C. Ricotta, M. Palmer, & S. Scheiner 
 (2009) Spatially constrained rarefaction: incorporating the autocorrelated
 structure of biological communities into sample-based rarefaction. Community
 Ecology, 10, 209-214.
 
Gotelli, N.J. & Colwell, R.K. (2001) Quantifying biodiversity: procedures
 and pitfalls in the measurement and comparison of species richness. Ecology
 Letters, 4, 379-391.

Heck, K.L., van Belle, G. & Simberloff, D. (1975). Explicit calculation of
 the rarefaction diversity measurement and the determination of sufficient
 sample size. Ecology 56, 1459–1461.

Oksanen, J. et al. 2022. Vegan: community ecology package. R package version 2.6-4.
 https://CRAN.R-project.org/package=vegan
}
\author{
Dan McGlinn and Xiao Xiao
}
