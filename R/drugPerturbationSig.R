#' Creates a signature representing gene expression (or other molecular profile)
#' change induced by administrating a drug, for use in drug effect analysis.
#'
#' Given a Pharmacoset of the perturbation experiment type, and a list of drugs,
#' the function will compute a signature for the effect of drug concentration on
#' the molecular profile of a cell. The algorithm uses a regression model which
#' corrects for experimental batch effects, cell specific differences, and
#' duration of experiment to isolate the effect of the concentration of the drug
#' applied. The function returns the estimated coefficient for concentration,
#' the t-stat, the p-value and the false discovery rate associated with that
#' coefficient, in a 3 dimensional array, with genes in the first direction,
#' drugs in the second, and the selected return values in the third.
#'
#' @examples
#' data(CMAPsmall)
#' drug.perturbation <- drugPerturbationSig(CMAPsmall, mDataType="rna", nthread=1)
#' print(drug.perturbation)
#'
#' @param pSet [PharmacoSet] a PharmacoSet of the perturbation experiment type
#' @param mDataType `character` which one of the molecular data types to use
#'   in the analysis, out of dna, rna, rnaseq, snp, cnv
#' @param drugs `character` a vector of drug names for which to compute the
#'   signatures. Should match the names used in the PharmacoSet.
#' @param cells `character` a vector of cell names to use in computing the
#'   signatures. Should match the names used in the PharmacoSet.
#' @param features `character` a vector of features for which to compute the
#'   signatures. Should match the names used in correspondant molecular data in PharmacoSet.
#' @param nthread `numeric` if multiple cores are available, how many cores
#'   should the computation be parallelized over?
#' @param returnValues `character` Which of estimate, t-stat, p-value and fdr
#'   should the function return for each gene drug pair?
#' @param verbose `logical(1)` Should diagnostive messages be printed? (default false)
#'
#' @return `list` a 3D array with genes in the first dimension, drugs in the
#'   second, and return values in the third.
#'
#' @export
drugPerturbationSig <- function(pSet, mDataType, drugs, cells, features,
  nthread=1, returnValues=c("estimate","tstat", "pvalue", "fdr"),
  verbose=FALSE)
{
	availcore <- parallel::detectCores()
	if ( nthread > availcore) {
	  nthread <- availcore
	}
  options("mc.cores"=nthread)
  if(!missing(cells)){
    if(!all(cells%in%sampleNames(pSet))){
      stop("The cell names should match to the names used in sampleNames(pSet)")
    }
    pSet <- subsetTo(pSet, cells=cells)
  }
  if (mDataType %in% names(pSet@molecularProfiles)) {
    #eset <- pSet@molecularProfiles[[mDataType]]
		if(S4Vectors::metadata(pSet@molecularProfiles[[mDataType]])$annotation != "rna"){
			stop(sprintf("Only rna data type perturbations are currently implemented"))
		}
  } else {
    stop (sprintf("This pSet does not have any molecular data of type %s, choose among: %s", mDataType), paste(names(pSet@molecularProfiles), collapse=", "))
  }


  if (missing(drugs)) {
    drugn <- treatmentNames(pSet)
  } else {
    drugn <- drugs
  }
  dix <- is.element(drugn, PharmacoGx::phenoInfo(pSet, mDataType)[ , "treatmentid"])
  if (verbose && !all(dix)) {
    warning (sprintf("%i/%i drugs can be found", sum(dix), length(drugn)))
  }
  if (!any(dix)) {
    stop("None of the drugs were found in the dataset")
  }
  drugn <- drugn[dix]

  if (missing(features)) {
    features <- rownames(featureInfo(pSet, mDataType))
  } else {
    fix <- is.element(features, rownames(featureInfo(pSet, mDataType)))
    if (verbose && !all(fix)) {
      warning (sprintf("%i/%i features can be found", sum(fix), length(features)))
    }
    features <- features[fix]
  }

  # splitix <- parallel::splitIndices(nx=length(drugn), ncl=nthread)
  # splitix <- splitix[vapply(splitix, length, FUN.VALUE=numeric(1)) > 0]
  mcres <- lapply(drugn, function(x, exprs, sampleinfo) {
    res <- NULL
    i = x
    ## using a linear model (x ~ concentration + cell + batch + duration)
    res <- rankGeneDrugPerturbation(data=exprs, drug=i, drug.id=as.character(sampleinfo[ , "treatmentid"]), drug.concentration=as.numeric(sampleinfo[ , "concentration"]), type=as.character(sampleinfo[ , "sampleid"]), xp=as.character(sampleinfo[ , "xptype"]), batch=as.character(sampleinfo[ , "batchid"]), duration=as.character(sampleinfo[ , "duration"]) ,single.type=FALSE, nthread=nthread, verbose=FALSE)$all[ , returnValues, drop=FALSE]
    res <- list(res)
    names(res) <- i
    return(res)
  }, exprs=t(molecularProfiles(pSet, mDataType)[features, , drop=FALSE]), sampleinfo=PharmacoGx::phenoInfo(pSet, mDataType))
  res <- do.call(c, mcres)
  res <- res[!vapply(res, is.null, FUN.VALUE=logical(1))]
  drug.perturbation <- array(NA, dim=c(nrow(featureInfo(pSet, mDataType)[features,, drop=FALSE]), length(res), ncol(res[[1]])), dimnames=list(rownames(featureInfo(pSet, mDataType)[features,,drop=FALSE]), names(res), colnames(res[[1]])))
  for (j in seq_len(ncol(res[[1]]))) {
    ttt <- vapply(res, function(x, j, k) {
              xx <- array(NA, dim=length(k), dimnames=list(k))
              xx[rownames(x)] <- x[ , j, drop=FALSE]
              return (xx)
              }, j=j, k=rownames(featureInfo(pSet, mDataType)[features,, drop=FALSE]),
            FUN.VALUE=numeric(dim(drug.perturbation)[1]))
    drug.perturbation[rownames(featureInfo(pSet, mDataType)[features,, drop=FALSE]), names(res), j] <- ttt
  }

  drug.perturbation <- PharmacoSig(drug.perturbation, PSetName = name(pSet), Call = as.character(match.call()), SigType='Perturbation')

  return(drug.perturbation)
}
