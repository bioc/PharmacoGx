% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeSlope.R
\name{computeSlope}
\alias{computeSlope}
\title{Return Slope (normalized slope of the drug response curve) for an experiment of a pSet by taking
its concentration and viability as input.}
\usage{
computeSlope(concentration, viability, trunc = TRUE, verbose = TRUE)
}
\arguments{
\item{concentration}{\code{numeric} A concentration range that the AUC should be computed for that range.
Concentration range by default considered as not logarithmic scaled. Converted to numeric by function if necessary.}

\item{viability}{\code{numeric} Viablities corresponding to the concentration range passed as first parameter.
The range of viablity values by definition should be between 0 and 100. But the viabalities greater than
100 and lower than 0 are also accepted.}

\item{trunc}{\code{logical(1)} A flag that identify if the viabality values should be truncated to be in the
range of (0,100)}

\item{verbose}{\code{logical(1)} If 'TRUE' the function will retrun warnings and other infomrative messages.}
}
\value{
Returns the normalized linear slope of the drug response curve
}
\description{
Return Slope (normalized slope of the drug response curve) for an experiment of a pSet by taking
its concentration and viability as input.
}
\examples{
dose <- c(0.0025,0.008,0.025,0.08,0.25,0.8,2.53,8)
viability <- c(108.67,111,102.16,100.27,90,87,74,57)
computeSlope(dose, viability)

}
