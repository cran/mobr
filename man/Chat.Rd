% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_C.R
\name{Chat}
\alias{Chat}
\title{Calculate expected sample coverage C_hat}
\usage{
Chat(x, m)
}
\arguments{
\item{x}{integer vector (species abundances)}

\item{m}{integer a number of individuals that is smaller than observed total
community abundance.}
}
\value{
a numeric value that is the expected coverage.
}
\description{
Returns expected sample coverage of a sample `x` for a smaller than observed
sample size `m` (Chao & Jost, 2012). This code was copied from INEXT's internal
function \code{iNEXT::Chat.Ind} (Hsieh et al 2016).
}
\examples{
data(inv_comm)
# What is the expected coverage at a sample size of 50 at the gamma scale?
Chat(colSums(inv_comm), 50)
}
\references{
Chao, A., and L. Jost. 2012. Coverage-based rarefaction and extrapolation:
 standardizing samples by completeness rather than size. Ecology 93:2533–2547.
 
Anne Chao, Nicholas J. Gotelli, T. C. Hsieh, Elizabeth L. Sander, K. H. Ma,
 Robert K. Colwell, and Aaron M. Ellison 2014. Rarefaction and extrapolation
 with Hill numbers: a framework for sampling and estimation in species
 diversity studies.  Ecological Monographs 84:45-67.

T. C. Hsieh, K. H. Ma and Anne Chao. 2024. 
 iNEXT: iNterpolation and EXTrapolation for
 species diversity. R package version 3.0.1
 URL: http://chao.stat.nthu.edu.tw/wordpress/software-download/.
}
