#' Computes the ICn for any n in 0-100 for a Drug Dose Viability Curve
#' 
#' Returns the ICn for any given nth percentile when given concentration and viability as input, normalized by the concentration
#' range of the experiment. A Hill Slope is first fit to the data, and the ICn is inferred from the fitted curve. Alternatively, the parameters
#' of a Hill Slope returned by logLogisticRegression can be passed in if they already known. 
#' 
#' @examples
#' dose <- c(0.0025,0.008,0.025,0.08,0.25,0.8,2.53,8) 
#' viability <- c(108.67,111,102.16,100.27,90,87,74,57)
#' computeIC50(dose, viability)
#' computeICn(dose, viability, n=10)
#' 
#' @param concentration `numeric` is a vector of drug concentrations.
#' @param viability `numeric` is a vector whose entries are the viability values observed in the presence of the
#' drug concentrations whose logarithms are in the corresponding entries of conc, where viability 0
#' indicates that all cells died, and viability 1 indicates that the drug had no effect on the cells. 
#' @param Hill_fit `list` or `vector` In the order: c("Hill Slope", "E_inf", "EC50"), the parameters of a Hill Slope 
#' as returned by logLogisticRegression. If conc_as_log is set then the function assumes logEC50 is passed in, and if
#' viability_as_pct flag is set, it assumes E_inf is passed in as a percent. Otherwise, E_inf is assumed to be a decimal, 
#' and EC50 as a concentration. 
#' @param n `numeric` The percentile concentration to compute. If viability_as_pct set, assumed to be percentage, otherwise
#' assumed to be a decimal value.
#' @param conc_as_log `logical`, if true, assumes that log10-concentration data has been given rather than concentration data,
#' and that log10(ICn) should be returned instead of ICn.
#' @param viability_as_pct `logical`, if false, assumes that viability is given as a decimal rather
#' than a percentage, and that E_inf passed in as decimal.
#' @param trunc `logical`, if true, causes viability data to be truncated to lie between 0 and 1 before
#' curve-fitting is performed.
#' @param verbose `logical`, if true, causes warnings thrown by the function to be printed.
#' @return a numeric value for the concentration of the nth precentile viability reduction 
#' @export
computeICn <- function(concentration,
                       viability,
                       Hill_fit,
                       n,
                       conc_as_log = FALSE,
                       viability_as_pct = TRUE, 
                       verbose = TRUE,
                       trunc = TRUE) {

  if (missing(Hill_fit) & !missing(concentration) & !missing(viability)) {

    Hill_fit <- logLogisticRegression(conc = concentration,
      viability,
      conc_as_log = conc_as_log,
      viability_as_pct = viability_as_pct,
      trunc = trunc,
      verbose = verbose)
    cleanData <- sanitizeInput(conc=concentration, 
      Hill_fit=Hill_fit,
      conc_as_log = conc_as_log,
      viability_as_pct = viability_as_pct,
      trunc = trunc,
      verbose = verbose)
    pars <- cleanData[["Hill_fit"]]
    concentration <- cleanData[["log_conc"]]
  } else if (!missing(Hill_fit)){

    cleanData <- sanitizeInput(conc = concentration, 
      viability = viability,
      Hill_fit = Hill_fit,
      conc_as_log = conc_as_log,
      viability_as_pct = viability_as_pct,
      trunc = trunc,
      verbose = verbose)
    pars <- cleanData[["Hill_fit"]]
  } else {

    stop("Insufficient information to calculate ICn. Please enter concentration and viability or Hill parameters.")

  }
  if(viability_as_pct){
    n <- n/100
  }

  
  n <- 1 - n
  
  if (n < pars[2] || n > 1) {
    return(NA_real_)
  } else if (n == pars[2]) {

    return(Inf)

  } else if (n == 1) {

    return(ifelse(conc_as_log, -Inf, 0))

  } else {

    return(ifelse(conc_as_log,
      log10(10 ^ pars[3] * ((n - 1) / (pars[2] - n)) ^ (1 / pars[1])),
      10 ^ pars[3] * ((n - 1) / (pars[2] - n)) ^ (1 / pars[1])))

  }

}