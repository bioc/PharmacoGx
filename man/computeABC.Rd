% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeABC.R
\name{computeABC}
\alias{computeABC}
\title{Fits dose-response curves to data given by the user
and returns the ABC of the fitted curves.}
\usage{
computeABC(
  conc1,
  conc2,
  viability1,
  viability2,
  Hill_fit1,
  Hill_fit2,
  conc_as_log = FALSE,
  viability_as_pct = TRUE,
  trunc = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{conc1}{\code{numeric} is a vector of drug concentrations.}

\item{conc2}{\code{numeric} is a vector of drug concentrations.}

\item{viability1}{\code{numeric} is a vector whose entries are the viability values observed in the presence of the
drug concentrations whose logarithms are in the corresponding entries of conc1, expressed as percentages
of viability in the absence of any drug.}

\item{viability2}{\code{numeric} is a vector whose entries are the viability values observed in the presence of the
drug concentrations whose logarithms are in the corresponding entries of conc2, expressed as percentages
of viability in the absence of any drug.}

\item{Hill_fit1}{\code{list} or \code{vector} In the order: c("Hill Slope", "E_inf", "EC50"), the parameters of a Hill Slope
as returned by logLogisticRegression. If conc_as_log is set then the function assumes logEC50 is passed in, and if
viability_as_pct flag is set, it assumes E_inf is passed in as a percent. Otherwise, E_inf is assumed to be a decimal,
and EC50 as a concentration.}

\item{Hill_fit2}{\code{lis} or \code{vector} In the order: c("Hill Slope", "E_inf", "EC50"), the parameters of a Hill Slope
as returned by logLogisticRegression. If conc_as_log is set then the function assumes logEC50 is passed in, and if
viability_as_pct flag is set, it assumes E_inf is passed in as a percent. Otherwise, E_inf is assumed to be a decimal,
and EC50 as a concentration.}

\item{conc_as_log}{\code{logical}, if true, assumes that log10-concentration data has been given rather than concentration data.}

\item{viability_as_pct}{\code{logical}, if false, assumes that viability is given as a decimal rather
than a percentage, and returns ABC as a decimal. Otherwise, viability is interpreted as percent, and AUC is returned 0-100.}

\item{trunc}{\code{logical}, if true, causes viability data to be truncated to lie between 0 and 1 before
curve-fitting is performed.}

\item{verbose}{\code{logical}, if true, causes warnings thrown by the function to be printed.}
}
\value{
The numeric area of the absolute difference between the two hill slopes
}
\description{
Fits dose-response curves to data given by the user
and returns the ABC of the fitted curves.
}
\examples{
dose1 <- c(0.0025,0.008,0.025,0.08,0.25,0.8,2.53,8)
viability1 <- c(108.67,111,102.16,100.27,90,87,74,57)
dose2 <- c(0.0025,0.008,0.025,0.08,0.25,0.8,2.53,8)
viability2 <- c(100.94,112.5,86,104.16,75,68,48,29)
computeABC(dose1, dose2, viability1, viability2)

}
\author{
Mark Freeman
}
