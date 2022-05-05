# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?step_optim

#' Forward and backward model selection
#'
#' This function takes a model and carry out a model selection by stepping
#' backward, forward or in both directions.
#'
#' Note that mclapply is used. In order to control the number of cores to use,
#' then set it, e.g. to one core `options(mc.cores=1)`, which is needed for
#' debugging to work.
#' 
#' The full model containing all inputs must be given. In each step new models
#' are generated, with either one removed input or one added input, and then all
#' the generated models are optimized and their scores compared. If any new
#' model have an improved score compared to the currently selected model, then
#' the new is selected and the process is repeated until no new improvement is
#' obtained.
#'
#' In addition to selecting inputs, then integer parameters can also be stepped
#' through, e.g. the order of basis splined or the number of harmonics in
#' Fourier series.
#' 
#' The stepping process is different depending on the direction. In addition to
#' the full model, a starting model can be given, then the selection process
#' will start from that model.
#' 
#' If the direction is "both", which is default (same as "backwardboth") then the
#' stepping is:
#'  - In first step inputs are removed one-by-one
#'  - In following steps, inputs still in the model are removed one-by-one, and
#'    inputs not in the model are added one-by-one
#'
#' If the direction is "backwards":
#'  - Inputs are only removed in each step
#' 
#' If the direction is "forwardboth":
#'  - In the first step all inputs are removed
#'  - In following steps (same as "both")
#'
#' If the direction is "forward":
#' - In the first step all inputs are removed and from there inputs are only added
#'  
#'
#' For stepping through integer variables in the transformation stage, then
#' these have to be set in the "prm" argument. The stepping process will follow
#' the input selection described above.
#'
#' In case of missing values, especially in combination with auto-regressive
#' models, it can be very important to make sure that only complete cases are
#' included when calculating the score. By providing the `fitfun` argument then
#' the score will be calculated using only the complete cases across horizons
#' and models in each step, see the last examples.
#'
#' Note, that either kseq or kseqopt must be set on the modelfull object. If kseqopt
#' is set, then it is used no matter the value of kseq.
#'
#' @title Forward and backward model selection
#' @param modelfull The full forecastmodel containing all inputs which will be
#'     can be included in the selection.
#' @param data The data.list which holds the data on which the model is fitted.
#' @param prm A list of integer parameters to be stepped. Given using the same
#'     syntax as parameters for optimization, e.g. `list(I__degree = c(min=3,
#'     max=7))` will step the "degree" for input "I".
#' @param direction The direction to be used in the selection process.
#' @param modelstart A forecastmodel. If it's set then it will be used as the
#'     selected model from the first step of the stepping. It should be a sub
#'     model of the full model.
#' @param keepinputs If TRUE no inputs can be removed in a step, if FALSE then
#'     any input can be removed. If given as a character vector with names of
#'     inputs, then they cannot be removed in any step.
#' @param optimfun The function which will carry out the optimization in each
#'     step.
#' @param fitfun A fit function, should be the same as used in optimfun(). If
#'     provided, then the score is caculated with this function (instead of the
#'     one called in optimfun(), hence the default is rls_fit(), which is called
#'     in rls_optim()). Furthermore, information on complete cases are printed
#'     and returned.
#' @param scorefun The score function used.
#' @param printout Logical. Passed on to fitting functions.
#' @param mc.cores The mc.cores argument of mclapply. If debugging it can be
#'     necessary to set it to 1 to stop execution.
#' @param ... Additional arguments which will be passed on to optimfun. For
#'     example control how many steps
#'
#' @return A list with the result of each step:
#'  - '$model' is the model selected in each step
#'  - '$score' is the score for the model selected in each step
#'  - '$optimresult' the result return by the the optimfun
#'  - '$completecases' a logical vector (NA if fitfun argument is not given) indicating which time points were complete across all horizons and models for the particular step.
#'
#' @examples
#'
#'
#' # The data, just a rather short period to keep running times short
#' D <- subset(Dbuilding, c("2010-12-15", "2011-02-01"))
#' # Set the score period
#' D$scoreperiod <- in_range("2010-12-22", D$t)
#' #
#' D$tday <- make_tday(D$t, 1:36)
#' # Generate an input which is just random noise, i.e. should be removed in the selection
#' set.seed(83792)
#' D$noise <- make_input(rnorm(length(D$t)), 1:36)
#' 
#' # The full model
#' model <- forecastmodel$new()
#' # Set the model output
#' model$output = "heatload"
#' # Inputs (transformation step)
#' model$add_inputs(Ta = "Ta",
#'                  noise = "noise",
#'                  mu_tday = "fs(tday/24, nharmonics=5)",
#'                  mu = "one()")
#' # Regression step parameters
#' model$add_regprm("rls_prm(lambda=0.9)")
#' # Optimization bounds for parameters
#' model$add_prmbounds(lambda = c(0.9, 0.99, 0.9999))
#' 
#' # Select a model, in the optimization just run it for a single horizon
#' # Note that kseqopt could also be set
#' model$kseq <- 5
#' 
#' # Set the parameters to step on, note the 
#' prm <- list(mu_tday__nharmonics = c(min=3, max=7))
#' 
#' # Note the control argument, which is passed to optim, it's now set to few
#' # iterations in the offline parameter optimization (MUST be increased in real applications)
#' control <- list(maxit=1)
#'
#' # On Windows multi cores are not supported, so for the examples use only one core
#' mc.cores <- 1
#' 
#' # Run the default selection scheme, which is "both"
#' # (same as "backwardboth" if no start model is given)
#' \donttest{L <- step_optim(model, D, prm, control=control, mc.cores=mc.cores)
#'
#' # The optim value from each step is returned
#' getse(L, "optimresult")
#' getse(L,"score")
#' 
#' # The final model
#' L$final$model
#'
#' # Other selection schemes
#' Lforward <- step_optim(model, D, prm, "forward", control=control, mc.cores=mc.cores)
#' Lbackward <- step_optim(model, D, prm, "backward", control=control, mc.cores=mc.cores)
#' Lbackwardboth <- step_optim(model, D, prm, "backwardboth", control=control, mc.cores=mc.cores)
#' Lforwardboth <- step_optim(model, D, prm, "forwardboth", control=control, mc.cores=mc.cores)
#'
#' # It's possible avoid removing specified inputs
#' L <- step_optim(model, D, prm, keepinputs=c("mu","mu_tday"), control=control, mc.cores=mc.cores)
#' 
#' # Give a starting model
#' modelstart <- model$clone_deep()
#' modelstart$inputs[2:3] <- NULL
#' L <- step_optim(model, D, prm, modelstart=modelstart, control=control, mc.cores=mc.cores)
#'
#' # If a fitting function is given, then it will be used for calculating the forecasts.
#' # Below it's the rls_fit function, so the same as used internally in rls_fit, so only 
#' # difference is that now ONLY on the complete cases for all models in each step are used
#' # when calculating the score in each step
#' L1 <- step_optim(model, D, prm, fitfun=rls_fit, control=control, mc.cores=mc.cores)
#'
#' # The easiest way to conclude if missing values have an influence is to
#' # compare the selection result running with and without
#' L2 <- step_optim(model, D, prm, control=control, mc.cores=mc.cores)
#'
#' # Compare the selected models
#' tmp1 <- capture.output(getse(L1, "model"))
#' tmp2 <- capture.output(getse(L2, "model"))
#' identical(tmp1, tmp2)}
#' 
#' 
#' # Note that caching can be really smart (the cache files are located in the
#' # cachedir folder (folder in current working directory, can be removed with
#' # unlink(foldername)) See e.g. `?rls_optim` for how the caching works
#' # L <- step_optim(model, D, prm, "forward", cachedir="cache", cachererun=FALSE, mc.cores=mc.cores)
#' 
#' @importFrom parallel mclapply
#'
#' @export

step_optim <- function(modelfull, data, prm=list(NA), direction = c("both","backward","forward","backwardboth","forwardboth"), modelstart=NA, keepinputs = FALSE, optimfun = rls_optim, fitfun = NA, scorefun = rmse, printout = FALSE, mc.cores = getOption("mc.cores", 2L), ...){
    # Do:
    # - checking of input, model, ...
    # - Maybe have "cloneit" argument in optimfun, then don't clone inside optim.
    # - Add argument controlling how much is kept in each iteration (e.g all fitted models)
    # - Is raised as an issue: For score make sure that only the same points are included for all models, or print warning if not!
    # - With lm we should use have some regularization, so use AIC or BIC as the score
    # - Differentiate the parameters given to optimfun for the selected model and for the new models in each step. E.g. take more steps on the selected model.
    #
    # - Help: prm <- list(I__degree = c(min=1, max=7), mu_tday__nharmonics = c(min=1, max=7))
    #
    # For keeping all the results
    L <- list()
    # The first value in direction is default
    direction <- match.arg(direction)
    # Init
    istep <- 1
    # Different start up, if a start model is given
    # Note the use of inherits() instead of: class(modelstart)[1] == "forecastmodel". See https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/
    if( inherits(modelstart, "forecastmodel") ){
        # The full model will not be changed from here, so no need to clone it
        mfull <- modelfull
        m <- modelstart$clone()
    }else{
        # No start model if given
        mfull <- modelfull$clone_deep()
        # Insert the starting prm values into the full model (must be in it, since added inputs are taken from there)
        if(!is.na(prm[1])){
            if( direction == "backward" ){
                mfull$insert_prm(unlist(getse(prm, "max")))
            }else if( direction == "forward" ){
                mfull$insert_prm(unlist(getse(prm, "min")))
            }else{
                # Both directions, then start at init, or halfway between min and max
                i <- which(sapply(prm, function(x){ "init" %in% names(x) }))
                if(length(i)){
                    mfull$insert_prm(unlist(getse(prm[i], "init")))
                }
                i <- which(sapply(prm, function(x){ !"init" %in% names(x) }))
                if(length(i)){
                    tmp <- unlist(getse(prm[i], "min"))
                    mfull$insert_prm(tmp + round((unlist(getse(prm[i], "max")) - tmp) / 2))
                }
            }
        }
        # Init current model
        m <- mfull$clone_deep()
        # 
        if(length(grep("forward",direction))){
            # Remove all inputs
            m$inputs <- list()
            # Start with no fit
            istep <- 0
            scoreCurrent <- Inf
        }
    }
    # Find the inputs to keep, if any
    if(inherits(keepinputs, "logical")){
        if(keepinputs){
            keepinputs <- nams(mfull$inputs)
        }else{
            keepinputs <- ""
        }
    }else{
        if(!inherits(keepinputs,"character")){ stop("keepinputs must be a logical or a character with the names of the inputs to keep in the model.") }
    }
    # Helper
    c_mStep <- function(l, nms){
        names(l) <- nms
        l <- l[!is.na(l)]
        c(mStep, l)
    }
    # Go
    done <- FALSE
    while(!done){
        message("\n------------------------------------------------------------------------\n")
        message(pst("Step ",istep,". Current model:"))
        message(print(m))
        # If the init model is not yet optimized
        if(istep == 1 & length(L) == 0){
            # Optimize
            res <- optimfun(m, data, printout=printout, scorefun=scorefun, ...)
            # Should we forecast only on the complete cases?
            if(inherits(fitfun, "function")){
                # Forecast to get the complete cases
                mtmp <- m$clone_deep()
                # If kseqopt is set, then make sure that it is used when fitting here
                if(!is.na(m$kseqopt)){
                    mtmp$kseq <- m$kseqopt
                }
                Yhat <- fitfun(res$par, mtmp, data, printout=printout)$Yhat
                scoreCurrent <- sum(score(residuals(Yhat,data[[m$output]]),data$scoreperiod))
                casesCurrent <- complete_cases(Yhat)
            }else{
                scoreCurrent <- res$value
                casesCurrent <- NA
            }
            # Keep it
            istep <- 1
            L[[istep]] <- list(model = m$clone_deep(), score = scoreCurrent, optimresult = res, completecases = casesCurrent)
        }
        
        message("Current score: ",format(scoreCurrent,digits=7))
        if(inherits(fitfun, "function")){
            message("Current complete cases: ",sum(casesCurrent)," (Diff in score from optim:",L[[istep]]$optimresult$value-scoreCurrent,")")
        }
        # Next step
        istep <- istep + 1
        # --------
        mStep <- list()

        # Generate the input modified models
        # Remove inputs
        if(length(grep("backward|both", direction))){
            irm <- which(!nams(m$inputs) %in% keepinputs)
            # Remove any? If only one input, then don't remove it
            if(length(irm) & length(m$inputs) > 1){
                tmp <- lapply(irm, function(i){
                    ms <- m$clone_deep()
                    ms$inputs[[i]] <- NULL
                    return(ms)
                })
                mStep <- c_mStep(tmp, pst("-",names(m$inputs)[irm]))
            }
        }

        # Add inputs
        if(length(grep("forward|both", direction))){
            # Add input one by one
            iin <- which(!names(mfull$inputs) %in% names(m$inputs))
            if(length(iin)){
                tmp <- lapply(iin, function(i){
                    ms <- m$clone_deep()
                    # Add one input
                    ms$inputs[[length(ms$inputs) + 1]] <- mfull$inputs[[i]]
                    names(ms$inputs)[length(ms$inputs)] <- names(mfull$inputs)[i]
                    # Sort according to the order in the full model
                    ms$inputs <- ms$inputs[order(match(names(ms$inputs), names(mfull$inputs)))]
                    return(ms)
                })
                mStep <- c_mStep(tmp, pst("+",names(mfull$inputs)[iin]))
            }
        }

        # Step parameters
        if(!is.na(prm[1])){
            if(length(grep("backward|both", direction))){
                # Count down the parameters one by one
                tmp <- lapply(1:length(prm), function(i){
                    p <- m$get_prmvalues(names(prm[i]))
                    # If the input is not in the current model, then p is NA, so don't include it for fitting
                    if(!is.na(p)){
                        # Only the ones with prms above minimum
                        if(prm[[i]]["min"] < p){
                            ms <- m$clone_deep()
                            p <- p - 1
                            ms$insert_prm(p)
                            # Return both the prm value and the name of the model to be printed
                            return(list(ms, pst(names(p),"=",p)))
                        }
                    }
                    return(list(NA,NA))
                })
                mStep <- c_mStep(getse(tmp,1), getse(tmp,2))
            }
            if(length(grep("forward|both", direction))){
                # Count up the parameters one by one
                tmp <- mclapply(1:length(prm), function(i){
                    p <- m$get_prmvalues(names(prm[i]))
                    # If the input is not in the current model, then p is NA, so don't include it for fitting
                    if(!is.na(p)){
                        # Only the ones with prms above minimum
                        if(p < prm[[i]]["max"]){
                            ms <- m$clone_deep()
                            p <- p + 1
                            ms$insert_prm(p)
                            # Return both the prm value and the name of the model to be printed
                            return(list(ms, pst(names(p),"=",p)))
                        }
                    }
                    return(list(NA,NA))
                })
                mStep <- c_mStep(getse(tmp,1), getse(tmp,2))
            }
        }
        
        # Optimize all the new models
        if(length(mStep) == 0){
            # Nothing to do, so stop
            done <- TRUE
        }else{

            # Run the optimization
            Lstep <- mclapply(1:length(mStep), function(i, ...){
                optimfun(mStep[[i]], data, printout=printout, scorefun=scorefun, ...)
            }, mc.cores=mc.cores, ...)
            names(Lstep) <- names(mStep)

            # Complete cases considered: Should we forecast and recalculate the score on complete cases from all models?
            if(inherits(fitfun, "function")){
                LYhat <- mclapply(1:length(mStep), function(i){
                    mtmp <- mStep[[i]]$clone_deep()
                    # If kseqopt is set, then make sure that it is used when fitting here
                    if(!is.na(m$kseqopt)){
                        mtmp$kseq <- m$kseqopt
                    }
                    fitfun(Lstep[[i]]$par, mtmp, data, printout=printout)$Yhat
                }, mc.cores=mc.cores)
                # Use complete cases across models and horizons per default
                scoreStep <- apply(score(residuals(LYhat,data[[m$output]]), data$scoreperiod), 2, sum)
                casesStep <- sapply(LYhat, complete_cases)
            }else{
                # Use the scores from optimfun
                scoreStep <- unlist(getse(Lstep, "value"))
                casesStep <- matrix(rep(NA,length(mStep)), nrow=1)
            }
                
            # Print out
            tmp <- as.matrix(unlist(getse(Lstep, "value")))
            if(scoreCurrent == Inf){
                tmp[ ,1] <- pst(format(tmp, digits=2))
                nams(tmp) <- "Score"
            }else{
                tmp[ ,1] <- pst(format(100 * (scoreCurrent - tmp) / scoreCurrent, digits=2),"%")
                nams(tmp) <- "Improvement"
            }
            if(inherits(fitfun, "function")){
                tmp <- cbind(tmp, apply(casesStep != casesCurrent, 2, sum))
                nams(tmp)[2] <- "CasesDiff"
            }
            print_to_message(tmp)

            # Compare scores: Is one the step models score smaller than the current ref?
            imin <- which.min(scoreStep)
                if( scoreStep[imin] < scoreCurrent ){
                    # Keep the best model (with it's optimized parameters)
                    m <- mStep[[imin]]
                    res <- Lstep[[imin]]
                    scoreCurrent <- scoreStep[[imin]]
                    casesCurrent <- casesStep[ ,imin]
                    # Insert the optimized parameters, such that next step starts at the optimized parameter values
                    if(is.null(names(res$par))){
                        names(res$par) <- row.names(m$prmbounds)
                    }
                    m$prmbounds[names(res$par),"init"] <- res$par
                    # Keep for the final result
                    m$insert_prm(res$par)
                    L[[istep]] <- list(model = m$clone_deep(), score = scoreCurrent, optimresult = res, completecases = casesCurrent)
                }else{
                    # No improvement obtained from reduction, so return the current model (last in the list)
                done <- TRUE
            }
        }
        if(done){
            message("\n------------------------------------------------------------------------\n\nFinal model:")
            message(print(m))
        }
    }
    if(length(L) == 1){
        names(L) <- "final"
    }else{
        names(L) <- c(pst("step",1:(length(L)-1)),"final")
    }
    invisible(L)
}
