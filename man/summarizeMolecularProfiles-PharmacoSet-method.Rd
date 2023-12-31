% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-summarizeMolecularProfiles.R
\name{summarizeMolecularProfiles,PharmacoSet-method}
\alias{summarizeMolecularProfiles,PharmacoSet-method}
\title{Takes molecular data from a PharmacoSet, and summarises them
into one entry per drug}
\usage{
\S4method{summarizeMolecularProfiles}{PharmacoSet}(
  object,
  mDataType,
  cell.lines,
  features,
  summary.stat = c("mean", "median", "first", "last", "and", "or"),
  fill.missing = TRUE,
  summarize = TRUE,
  verbose = TRUE,
  binarize.threshold = NA,
  binarize.direction = c("less", "greater"),
  removeTreated = TRUE
)
}
\arguments{
\item{object}{\code{PharmacoSet} The PharmacoSet to summarize}

\item{mDataType}{\code{character} which one of the molecular data types
to use in the analysis, out of all the molecular data types available for the pset
for example: rna, rnaseq, snp}

\item{cell.lines}{\code{character} The cell lines to be summarized.
If any cell.line has no data, missing values will be created}

\item{features}{\code{caracter} A vector of the feature names to include in the summary}

\item{summary.stat}{\code{character} which summary method to use if there are repeated
cell.lines? Choices are "mean", "median", "first", or "last"
In case molecular data type is mutation or fusion "and" and "or" choices are available}

\item{fill.missing}{\code{boolean} should the missing cell lines not in the
molecular data object be filled in with missing values?}

\item{summarize}{A flag which when set to FALSE (defaults to TRUE) disables summarizing and
returns the data unchanged as a ExpressionSet}

\item{verbose}{\code{boolean} should messages be printed}

\item{binarize.threshold}{\code{numeric} A value on which the molecular data is binarized.
If NA, no binarization is done.}

\item{binarize.direction}{\code{character} One of "less" or "greater", the direction of binarization on
binarize.threshold, if it is not NA.}

\item{removeTreated}{\code{logical} If treated/perturbation experiments are present, should they
be removed? Defaults to yes.}
}
\value{
\code{matrix} An updated PharmacoSet with the molecular data summarized
per cell line.
}
\description{
Given a PharmacoSet with molecular data, this function will summarize
the data into one profile per cell line, using the chosen summary.stat. Note
that this does not really make sense with perturbation type data, and will
combine experiments and controls when doing the summary if run on a
perturbation dataset.
}
\examples{
data(GDSCsmall)
GDSCsmall <- summarizeMolecularProfiles(GDSCsmall, mDataType = "rna", cell.lines=sampleNames(GDSCsmall), summary.stat = 'median', fill.missing = TRUE, verbose=TRUE)
GDSCsmall

}
\keyword{internal}
