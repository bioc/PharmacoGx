% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeSynergy.R
\name{effectToDose}
\alias{effectToDose}
\title{Inverse function of Hill equation}
\usage{
effectToDose(viability, EC50, HS, E_inf, is_pct = FALSE)
}
\arguments{
\item{viability}{\code{numeric} is a vector whose entries are the viability values
in the range [0, 1] if \code{is_pct} is \code{FALSE} or [0, 100] if it is
\code{TRUE}.}

\item{EC50}{\code{numeric} is a vector of relative EC50 for drug-response equation.}

\item{HS}{\code{numeric} Hill coefficient of the drug-response equation
that represents the sigmoidity of the curve.}

\item{E_inf}{\code{numeric} the maximum attanable effect of a drug
when it is administered with a infinitely high concentration.}

\item{is_pct}{\code{logical} whether both the input viabiliy and \code{E_inf} are given
in percentage ([0, 100]) rather than decimal ([0, 1]). Default FALSE.}
}
\value{
\code{numeric} concentrations in micromoles required to produce
\code{viability} in the corresponding entries.
}
\description{
For the dose-response Hill equation of a drug defined by
\eqn{E(x) = E_{inf}+\frac{1-E_{inf}}{1+(\frac{x}{EC50})^(\frac{1}{HS})}},
that computes the response in viability from a dose in micromole for a drug,
this function is the inverse function of the Hill curve that
computes the dose required to produce a given response:
\eqn{
    f^{-1}(E) = EC50 (
    \frac{1-E}{E-E_{inf}} )^{\frac{1}{HS}}
    )
}
}
\examples{
dose <- effectToDose(viability = 80,
                     EC50 = 42,
                     HS = 1,
                     E_inf = 10,
                     is_pct = TRUE)

}
