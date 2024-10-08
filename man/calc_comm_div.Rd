% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mobr_boxplots.R
\name{calc_comm_div}
\alias{calc_comm_div}
\title{Calculate biodiversity statistics from sites by species table.}
\usage{
calc_comm_div(
  abund_mat,
  index,
  effort = NA,
  extrapolate = TRUE,
  return_NA = FALSE,
  rare_thres = 0.05,
  scales = c("alpha", "gamma", "beta"),
  replace = FALSE,
  C_target_gamma = NA,
  ...
)
}
\arguments{
\item{abund_mat}{Abundance based site-by-species table. Species as
columns}

\item{index}{The calculated biodiversity indices. The options are
\itemize{
   \item \code{N} ... Number of individuals (total abundance)
   \item \code{S} ... Number of species
   \item \code{S_n} ... Rarefied or extrapolated number of species for n individuals
   \item \code{S_C} ... Estimate species richness of a given level of coverage by \code{C_target_gamma}
   \item \code{S_asymp} ... Estimated asymptotic species richness
   \item \code{f_0} ... Estimated number of undetected species 
   \item \code{pct_rare} ... The percent of rare species as defined by \code{rare_thres}
   \item \code{PIE} ... Hurlbert's PIE (Probability of Interspecific Encounter)
   \item \code{S_PIE} ... Effective number of species based on PIE
   
}
  See \emph{Details} for additional information on the
  biodiversity statistics.}

\item{effort}{The standardized number of individuals used for the 
calculation of rarefied species richness. This can a be
single integer or a vector of integers.}

\item{extrapolate}{Boolean which specifies if richness should be extrapolated
when effort is larger than the number of individuals using the chao1
method.}

\item{return_NA}{Boolean in which the rarefaction function
returns the observed S when \code{effort} is larger than the number of
individuals. If set to TRUE then NA is returned. Note that this argument
is only relevant when \code{extrapolate = FALSE}.}

\item{rare_thres}{The threshold that determines how pct_rare is computed.
It can range from (0, 1] and defaults to 0.05 which specifies that any 
species with less than or equal to 5% of the total abundance in a sample is
considered rare. It can also be specified as "N/S" which results in using
average abundance as the threshold which McGill (2011) found to have the 
best small sample behavior.}

\item{scales}{The scales to compute the diversity indices for: 
\itemize{
    \item \code{alpha} ... for each row of the site x species community matrix
    \item \code{gamma} ... for the entire site x species community matrix
    \item \code{beta} ... the ratio of diversity at the \code{gamma} and 
                           \code{alpha} scales.  
} Defaults to all three scales: \code{c('alpha', 'gamma', 'beta')}}

\item{replace}{Used for \code{PIE} and \code{SPIE}.  If TRUE, sampling with
replacement is used. Otherwise, sampling without replacement (default).}

\item{C_target_gamma}{When computing coverage based richness (\code{S_C}) then 
this argument can be used to specify the coverage to be used for the gamma scale
richness estimate. This defaults to \code{NA} in which case the target cover
is computed by \code{\link{calc_C_target}} (i.e., the largest allowable sample
size).}

\item{...}{additional arguments that can be passed to \code{\link{calc_div}}}
}
\value{
A \code{data.frame} with four columns:
\itemize{
   \item \code{scale} ... Group label for sites
   \item \code{index} ... Name of the biodiversity index
   \item \code{sample_size} ... The number of samples used to compute the
    statistic, helpful for interpreting beta and gamma metrics. 
   \item \code{effort} ... Sampling effort for rarefied richness 
   (NA for the other indices)
   \item \code{gamma_coverage} ... The coverage value for that particular 
   effort value on the gamma scale rarefaction curve. Will be \code{NA} unless
   coverage based richness (\code{S_C}) and/or beta diversity is computed.  
   \item \code{value} ... Value of the biodiversity index
}
}
\description{
Calculate biodiversity statistics from sites by species table.
}
\details{
\strong{BIODIVERSITY INDICES}

\strong{N: total community abundance} is the total number of individuals 
observed across all species in the sample

\strong{S: species richness} is the observed number of species that occurs at
least once in a sample

\strong{S_n: Rarefied species richness} is the expected number of species, given a
defined number of sampled individuals (n) (Gotelli & Colwell 2001). Rarefied
richness at the alpha-scale is calculated for the values provided in 
\code{effort_samples} as long as these values are not smaller than the 
user-defined minimum value \code{effort_min}. In this case the minimum value 
is used and samples with less individuals are discarded. When no values for
\code{effort_samples} are provided the observed minimum number of individuals
of the samples is used, which is the standard in rarefaction analysis
(Gotelli & Colwell 2001). Because the number of individuals is expected to
scale linearly with sample area or effort, at the gamma-scale the number of
individuals for rarefaction is calculated as the minimum number of samples
within groups multiplied by \code{effort_samples}. For example, when there are 10
samples within each group, \code{effort_groups} equals \code{10 *
effort_samples}. If n is larger than the number of individuals in sample and
\code{extrapolate = TRUE} then the Chao1 (Chao 1984, Chao 1987) method is
used to extrapolate the rarefaction curve.

\strong{pct_rare: Percent of rare species} Is the ratio of the number of rare
species to the number of observed species x 100 (McGill 2011). Species are 
considered rare in a particular sample if they have fewer individuals than 
\code{rare_thres * N} where \code{rare_thres} can be set by the user and 
\code{N} is the total number of individuals in the sample. The default value 
of \code{rare_thres} of 0.05 is arbitrary and was chosen because McGill 
(2011) found this metric of rarity performed well and was generally less 
correlated with other common metrics of biodiversity. Essentially this metric
attempt to estimate what proportion of the species in the same occur in the
tail of the species abundance distribution and is therefore sensitive to
presence of rare species.

\strong{S_asymp: Asymptotic species richness} is the expected number of 
species given complete sampling and here it is calculated using the Chao1
estimator (Chao 1984, Chao 1987) see \code{\link{calc_chao1}}. Note: this metric
is typically highly correlated with S (McGill 2011).
 
\strong{f_0: Undetected species richness} is the number of undetected species
or the number of species observed 0 times which is an indicator of the degree
of rarity in the community. If there is a greater rarity then f_0 is expected
to increase. This metric is calculated as \code{S_asymp - S}. This metric is less 
correlated with S than the raw \code{S_asymp} metric. 

\strong{PIE: Probability of intraspecific encounter} represents the
probability that two randomly drawn individuals belong to the same species.
Here we use the definition of Hurlbert (1971), which considers sampling
without replacement. PIE is closely related to the well-known Simpson
diversity index, but the latter assumes sampling with replacement.

\strong{S_PIE: Effective number of species for PIE} represents the effective
number of species derived from the PIE. It is calculated using the asymptotic
estimator for Hill numbers of diversity order 2 (Chao et al, 2014). S_PIE
represents the species richness of a hypothetical community with
equally-abundant species and infinitely many individuals corresponding to the
same value of PIE as the real community. An intuitive interpretation of S_PIE
is that it corresponds to the number of dominant (highly abundant) species in
the species pool.

For species richness \code{S}, rarefied richness \code{S_n}, undetected
richness \code{f_0}, and the Effective Number of Species \code{S_PIE} we also
calculate beta-diversity using multiplicative partitioning (Whittaker 1972,
Jost 2007). That means for these indices we estimate beta-diversity as the
ratio of gamma-diversity (total diversity across all plots) divided by
alpha-diversity (i.e., average plot diversity).
}
\examples{
data(tank_comm)
div_metrics <- calc_comm_div(tank_comm, 'S_n', effort = c(5, 10))
div_metrics
div_metrics <- calc_comm_div(tank_comm, 'S_C', C_target_gamma = 0.75)
div_metrics
}
\references{
McGill, B. J. 2011. Species abundance distributions. Pages 105-122 Biological
Diversity: Frontiers in Measurement and Assessment, eds. A.E. Magurran and
B.J. McGill.
}
\author{
Felix May and Dan McGlinn
}
