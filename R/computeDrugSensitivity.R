#' @importFrom BiocParallel bplapply
.calculateSensitivitiesStar <- function (pSets=list(), exps=NULL, cap=NA,
        na.rm=TRUE, area.type=c("Fitted", "Actual"), nthread=1) {
    if (missing(area.type)) {
        area.type <- "Fitted"
    }
    if (is.null(exps)) {
        stop("expriments is empty!")
    }
    for (study in names(pSets)) {
        sensitivityProfiles(pSets[[study]])$auc_recomputed_star <- NA
    }
    if (!is.na(cap)) {
        trunc <- TRUE
    }else{
        trunc <- FALSE
    }

    for (i in seq_len(nrow(exps))) {
        ranges <- list()
        for (study in names(pSets)) {
            ranges[[study]] <- as.numeric(sensitivityRaw(pSets[[study]])[
                exps[i, study], , "Dose"
            ])
        }
        ranges <- .getCommonConcentrationRange(ranges)
        names(ranges) <- names(pSets)
        for (study in names(pSets)) {
            myx <- as.numeric(sensitivityRaw(pSets[[study]])[
                exps[i, study],,"Dose"]) %in% ranges[[study]
            ]
            sensitivityRaw(pSets[[study]])[exps[i, study], !myx, ] <- NA
        }
    }

    op <- options()
    options(mc.cores=nthread)
    on.exit(options(op))

    for (study in names(pSets)) {
        auc_recomputed_star <- unlist(
            bplapply(rownames(sensitivityRaw(pSets[[study]])),
                FUN=function(experiment, exps, study, dataset, area.type) {
                    if (!experiment %in% exps[,study]) return(NA_real_)
                    return(computeAUC(
                        concentration=as.numeric(dataset[experiment, , 1]),
                        viability=as.numeric(dataset[experiment, , 2]),
                        trunc=trunc, conc_as_log=FALSE, viability_as_pct=TRUE,
                        area.type=area.type
                        ) / 100
                        )
                    },
                exps=exps, study=study, dataset=sensitivityRaw(pSets[[study]]),
                area.type=area.type)
            )
        sensitivityProfiles(pSets[[study]])$auc_recomputed_star <-
            auc_recomputed_star
    }
    return(pSets)
}

## This function computes AUC for the whole raw sensitivity data of a pset
.calculateFromRaw <- function(raw.sensitivity, cap=NA, nthread=1,
        family=c("normal", "Cauchy"), scale=0.07, n=1) {
    family <- match.arg(family)

    AUC <- vector(length=dim(raw.sensitivity)[1])
    names(AUC) <- dimnames(raw.sensitivity)[[1]]

    IC50 <- vector(length=dim(raw.sensitivity)[1])
    names(IC50) <- dimnames(raw.sensitivity)[[1]]

    trunc <- !is.na(cap)

    if (nthread == 1) {
        pars <- lapply(names(AUC),
            FUN=function(exp, raw.sensitivity, family, scale, n) {
                if (length(grep("///", raw.sensitivity[exp, , "Dose"])) > 0 ||
                        all(is.na(raw.sensitivity[exp, , "Dose"]))) {
                    NA
                } else{
                    logLogisticRegression(raw.sensitivity[exp, , "Dose"],
                        raw.sensitivity[exp, , "Viability"], trunc=trunc,
                        conc_as_log=FALSE, viability_as_pct=TRUE, family=family,
                        scale=scale, median_n=n)
                }
            },
            raw.sensitivity=raw.sensitivity, family=family, scale=scale,
            n=n
        )
        names(pars) <- dimnames(raw.sensitivity)[[1]]
        AUC <- unlist(lapply(names(pars),
            FUN=function(exp, raw.sensitivity, pars) {
                if (any(is.na(pars[[exp]]))) {
                    NA
                } else{
                    computeAUC(concentration=raw.sensitivity[exp, , "Dose"],
                        Hill_fit=pars[[exp]], trunc=trunc, conc_as_log=FALSE,
                        viability_as_pct=TRUE)
                }
            },
            raw.sensitivity=raw.sensitivity, pars=pars
        ))
        IC50 <- unlist(lapply(names(pars), function(exp, pars) {
            if (any(is.na(pars[[exp]]))) {
                NA
            } else{
                computeIC50(Hill_fit=pars[[exp]], trunc=trunc,
                    conc_as_log=FALSE, viability_as_pct=TRUE)
            }
        }, pars=pars))
    } else {
        pars <- parallel::mclapply(names(AUC),
            FUN=function(exp, raw.sensitivity, family, scale, n, trunc) {
                if (length(grep("///", raw.sensitivity[exp, , "Dose"])) > 0 ||
                        all(is.na(raw.sensitivity[exp, , "Dose"]))) {
                    NA
                } else {
                    logLogisticRegression(
                        raw.sensitivity[exp, , "Dose"],
                        raw.sensitivity[exp, , "Viability"],
                        trunc=trunc, conc_as_log=FALSE, viability_as_pct=TRUE,
                        family=family, scale=scale, median_n=n)
                }
            },
            raw.sensitivity=raw.sensitivity, family=family, scale=scale, n=n,
            trunc=trunc, mc.cores=nthread
        )
        names(pars) <- dimnames(raw.sensitivity)[[1]]
        AUC <- unlist(parallel::mclapply(names(pars),
            FUN=function(exp, raw.sensitivity, pars, trunc) {
                if (any(is.na(pars[[exp]]))) {
                    NA
                } else{
                    computeAUC(
                        concentration=raw.sensitivity[exp, , "Dose"],
                        Hill_fit=pars[[exp]],
                        trunc=trunc, conc_as_log=FALSE, viability_as_pct=TRUE)
                }
            },
            raw.sensitivity=raw.sensitivity, pars=pars, trunc=trunc,
            mc.cores=nthread
        ))
        IC50 <- unlist(parallel::mclapply(names(pars),
            FUN=function(exp, pars, trunc) {
                if(any(is.na(pars[[exp]]))) {
                    NA
                } else{
                    computeIC50(Hill_fit=pars[[exp]], trunc=trunc,
                        conc_as_log=FALSE, viability_as_pct=TRUE)
                }
            }, pars=pars, trunc=trunc, mc.cores=nthread
        ))
    }
    names(AUC) <- dimnames(raw.sensitivity)[[1]]
    names(IC50) <- dimnames(raw.sensitivity)[[1]]

    return(list("AUC"=AUC, "IC50"=IC50, "pars"=pars))
}


## This function computes intersected concentration range between a list of
## concentration ranges
.getCommonConcentrationRange <- function(doses) {
    min.dose <- 0
    max.dose <- 10^100
    for (i in seq_len(length(doses))) {
        min.dose <- max(min.dose, min(as.numeric(doses[[i]]), na.rm=TRUE),
            na.rm=TRUE)
        max.dose <- min(max.dose, max(as.numeric(doses[[i]]), na.rm=TRUE),
            na.rm=TRUE)
    }
    common.ranges <- list()
    for (i in seq_len(length(doses))) {
        common.ranges[[i]] <- doses[[i]][
            seq(which.min(abs(as.numeric(doses[[i]]) - min.dose)), max(
                which(abs(as.numeric(doses[[i]]) - max.dose) ==
                    min(abs(as.numeric(doses[[i]]) - max.dose), na.rm=TRUE)
                ))
            )
        ]
    }
    return(common.ranges)
}

## predict viability from concentration data and curve parameters
.Hill <- function(x, pars) {
    return(pars[2] + (1 - pars[2]) / (1 + (10 ^ x / 10 ^ pars[3]) ^ pars[1]))
}

## calculate residual of fit
## FIXME:: Why is this different from CoreGx?
#' @importFrom CoreGx .dmedncauchys .dmednnormals .edmednnormals .edmedncauchys
.residual <- function(x, y, n, pars, scale=0.07, family=c("normal", "Cauchy"),
        trunc=FALSE) {
    family <- match.arg(family)
    Cauchy_flag=(family == "Cauchy")
    if (Cauchy_flag == FALSE) {
        # return(sum((.Hill(x, pars) - y) ^ 2))
        diffs <- .Hill(x, pars)-y
        if (trunc == FALSE) {
            return(sum(-log(.dmednnormals(diffs, n, scale))))
        } else {
            down_truncated <- abs(y) >= 1
            up_truncated <- abs(y) <= 0

            # For up truncated, integrate the cauchy dist up until -
            #>because anything less gets truncated to 0, and thus the residual
            #>is -diff, and the prob function becomes discrete For
            #>down_truncated, 1-cdf(diffs)=cdf(-diffs)
            return(
                sum(-log(.dmednnormals(diffs[!(down_truncated | up_truncated)],
                    n, scale))) +
                sum(-log(.edmednnormals(-diffs[up_truncated | down_truncated],
                    n, scale)))
            )

        }
    } else {
        diffs <- .Hill(x, pars) - y
        if (trunc == FALSE) {
            return(sum(-log(.dmedncauchys(diffs, n, scale))))
        } else {
            down_truncated <- abs(y) >= 1
            up_truncated <- abs(y) <= 0
            # For up truncated, integrate the cauchy dist up until -diff because
            #> anything less gets truncated to 0, and thus the residual is -diff,
            #>and the prob function becomes discrete For down_truncated,
            #>1 - cdf(diffs) = cdf(-diffs)
            return(
                sum(-log(.dmedncauchys(diffs[!(down_truncated | up_truncated)],
                    n, scale))) +
                sum(-log(.edmedncauchys(-diffs[up_truncated | down_truncated],
                    n, scale))))
        }
    }
}

##FIXME:: Why is this different from CoreGx?
.meshEval <- function(log_conc, viability, lower_bounds=c(0, 0, -6),
        upper_bounds=c(4, 1, 6), density=c(2, 10, 2), scale=0.07, n=1,
        family=c("normal", "Cauchy"), trunc=FALSE) {
    family <- match.arg(family)
    guess <- c(pmin(pmax(1, lower_bounds[1]), upper_bounds[1]),
        pmin(pmax(min(viability), lower_bounds[2]), upper_bounds[2]),
        pmin(pmax(log_conc[which.min(abs(viability - 1 / 2))], lower_bounds[3]),
        upper_bounds[3]))
    guess_residual <- .residual(log_conc, viability, pars=guess, n=n,
        scale=scale, family=family, trunc=trunc)
    for (i in seq(from=lower_bounds[1], to=upper_bounds[1],
            by=1 / density[1])) {
        for (j in seq(from=lower_bounds[2], to=upper_bounds[2],
                by=1 / density[2])) {
            for (k in seq(from=lower_bounds[3], to=upper_bounds[3],
                    by=1 / density[3])) {
                test_guess_residual <- .residual(log_conc, viability,
                    pars=c(i, j, k), n=n, scale=scale, family=family,
                    trunc=trunc)
                if (!is.finite(test_guess_residual)) {
                    warning(paste0(" Test Guess Residual is: ",
                        test_guess_residual, "\n Other Pars: log_conc: ",
                        paste(log_conc, collapse=", "), "\n Viability: ",
                        paste(viability, collapse=", "), "\n Scale: ", scale,
                        "\n Family: ", family, "\n Trunc ", trunc, "\n HS: ",
                        i, ", Einf: ", j, ", logEC50: ", k, "\n n: ", n))
                }
                if (!length(test_guess_residual)) {
                    warning(paste0(" Test Guess Residual is: ",
                        test_guess_residual,  "\n Other Pars: log_conc: ",
                        paste(log_conc, collapse=", "), "\n Viability: ",
                        paste(viability, collapse=", "), "\n Scale: ", scale,
                        "\n Family: ", family, "\n Trunc ", trunc, "\n HS: ", i,
                        ", Einf: ", j, ", logEC50: ", k, "\n n: ", n))
                }
                if (test_guess_residual < guess_residual) {
                    guess <- c(i, j, k)
                    guess_residual <- test_guess_residual
                }
            }
        }
    }
    return(guess)
}

## FIXME:: Documentation?
#  Fits dose-response curves to data given by the user
#  and returns the AUC of the fitted curve, normalized to the length of the concentration range.
#
#  @param concentration `numeric` is a vector of drug concentrations.
#
#  @param viability `numeric` is a vector whose entries are the viability values observed in the presence of the
#  drug concentrations whose logarithms are in the corresponding entries of the log_conc, expressed as percentages
#  of viability in the absence of any drug.
#
#  @param trunc `logical`, if true, causes viability data to be truncated to lie between 0 and 1 before
#  curve-fitting is performed.
#' @importFrom CoreGx .getSupportVec
#' @export
#' @keywords internal
.computeAUCUnderFittedCurve <- function(concentration, viability, trunc=TRUE,
        verbose=FALSE) {
    log_conc <- concentration
    #FIT CURVE AND CALCULATE IC50
    pars <- unlist(logLogisticRegression(log_conc, viability,
        conc_as_log=TRUE, viability_as_pct=FALSE, trunc=trunc))
    x <- .getSupportVec(log_conc)
    return(1 - trapz(x, .Hill(x, pars)) /
        (log_conc[length(log_conc)] - log_conc[1]))
}

#This function is being used in computeSlope
.optimizeRegression <- function(x, y, x0=-3, y0=100) {
    beta1 <- (sum(x * y) - y0 * sum(x)) / (sum(x * x) - x0 * sum(x))
    return(beta1)
}

updateMaxConc <- function(pSet) {
    sensitivityInfo(pSeto)$max.conc <- apply(sensitivityRaw(pSet)[, , "Dose"],
        1, max, na.rm=TRUE)
    return(pSet)
}