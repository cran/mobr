% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mobr_boxplots.R
\name{calc_PIE}
\alias{calc_PIE}
\title{Calculate probability of interspecific encounter (PIE)}
\usage{
calc_PIE(x, ENS = FALSE)
}
\arguments{
\item{x}{can either be a: 1) mob_in object, 2) community matrix-like
object in which rows represent plots and columns represent species, or 3)
a vector which contains the abundance of each species.}

\item{ENS}{Boolean that determines if the effective number of species should
be returned or the raw PIE value. Defaults to FALSE}
}
\description{
\code{calc_PIE} returns the probability of interspecific  encounter (PIE)
 which is also known as Simpson's evenness index and Gini-Simpson index. For \code{ENS=TRUE},
 PIE will be converted to an asymptotic effective number of species (S_PIE).
}
\details{
The formula of Hurlbert (1971) is used to calculate PIE:

\eqn{PIE = N /(N - 1) * (1 - p_i^2)}

where N is the total number of individuals and \eqn{p_i} is the relative abundance
of species i. This formulation uses sampling without replacement and it is
sometimes referred to as the bias corrected formulation of PIE.

For \code{ENS = TRUE}, S_PIE will be returned which represents the species richness of
a hypothetical community with equally-abundant species and infinitely many individuals
corresponding to the observed value of PIE. It is computed as
\eqn{S_PIE = 1 /(1 - PIE)}, which is equal to the
asymptotic estimator for Hill numbers of diversity order 2 provided by Chao et al (2014).
Note that S_PIE is undefined for communities with exactly one individual per species.
 
The code in this function borrows heavily from the function vegan::diversity()
but computes a different quantity. The function vegan::diversity() computes
PIE when sampling with replacement is assumed. The difference between the two 
formulations will decrease as N becomes large. Jari Oksanen and Bob O'Hara are
the original authors of the function vegan::diversity().
}
\examples{
data(inv_comm)
calc_PIE(inv_comm)
calc_PIE(inv_comm, ENS=TRUE)
}
\references{
Hurlbert, S. H. (1971) The nonconcept of species diversity: a critique and
 alternative parameters. Ecology 52, 577-586.
 
Chao, A., Gotelli, N. J., Hsieh, T. C., Sander, E. L., Ma, K. H., Colwell, R. K., & Ellison, A. M. (2014).
 Rarefaction and extrapolation with Hill numbers: A framework for sampling and estimation in species diversity studies.
 Ecological Monographs 84(1), 45-67.
}
\author{
Dan McGlinn, Thore Engel
}
