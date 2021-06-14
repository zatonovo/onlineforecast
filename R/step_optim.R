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
#' @title Forward and backward model selection
#' @param modelfull The full forecastmodel containing all inputs which will be
#'     can be included in the selection.
#' @param data The data.list which holds the data on which the model is fitted.
#' @param kseq The horizons to fit for (if not set, then model$kseq is used)
#' @param prm A list of integer parameters to be stepped. Given using the same
#'     syntax as parameters for optimization, e.g. `list(I__degree = c(min=3,
#'     max=7))` will step the "degree" for input "I".
#' @param direction The direction to be used in the selection process.
#' @param modelstart A forecastmodel. If it's set then it will be used as the
#'     selected model from the first step of the stepping. It should be a sub
#'     model of the full model.
#' @param optimfun The function which will carry out the optimization in each step.
#' @param scorefun The score function used.
#' @param ... Additional arguments which will be passed on to optimfun. For example control how many steps 
#'
#' @return A list with the result of each step:
#'  - '$model' is the model selected in each step
#'  - '$result' the result return by the the optimfun
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
#' # Select a model, just run it for a single horizon
#' kseq <- 5
#' # 
#' prm <- list(mu_tday__nharmonics = c(min=3, max=7))
#' 
#' # Note the control argument, which is passed to optim, it's now set to few
#' # iterations in the prm optimization (must be increased in real applications)
#' control <- list(maxit=1)
#'
#' # Run all selection schemes
#' Lboth <- step_optim(model, D, kseq, prm, "forward", control=control)
#' Lforward <- step_optim(model, D, kseq, prm, "forward", control=control)
#' Lbackward <- step_optim(model, D, kseq, prm, "backward", control=control)
#' Lbackwardboth <- step_optim(model, D, kseq, prm, "backwardboth", control=control)
#' Lforwardboth <- step_optim(model, D, kseq, prm, "forwardboth", control=control)
#'
#' # Give a starting model
#' modelstart <- model$clone_deep()
#' modelstart$inputs[2:3] <- NULL
#' Lboth <- step_optim(model, D, kseq, prm, modelstart=modelstart, control=control)
#' 
#' 
#' # Note that caching can be really smart (the cache files are located in the
#' # cachedir folder (folder in current working directory, can be removed with
#' # unlink(foldername)) See e.g. `?rls_optim` for how the caching works
#' # L <- step_optim(model, D, kseq, prm, "forward", cachedir="cache", cachererun=FALSE)
#' 
#' 
#' @importFrom parallel mclapply
#'
#' @export

step_optim <- function(modelfull, data, kseq = NA, prm=list(NA), direction = c("both","backward","forward","backwardboth","forwardboth"), modelstart=NA, optimfun = rls_optim, scorefun = rmse, printout = FALSE, ...){
    # Do:
    # - checking of input, model, ...
    # - change all lapply to mclapply
    # - Maybe have "cloneit" argument in optimfun, then don't clone inside optim.
    # - Add argument controlling how much is kept in each iteration (e.g all fitted models)
    # - Insert the inputs in the order they are in the full model, then cache might be more consistent (and the order is kept)
    # - For score make sure that only the same points are included for all models, or print warning if not!
    # - With lm we should use have some regularization, so use AIC or BIC as the score
    # - Differentiate the parameters given to optimfun for the selected model and for the new models in each step. E.g. take more steps on the selected model.
    #
    # - Help: prm <- list(I__degree = c(min=1, max=7), mu_tday__nharmonics = c(min=1, max=7))
    # - help: It's not checked that it's the score is calculated on the same values! WARNING should be printed if some models don't forecast same points
    #
    # For keeping all the results
    L <- list()
    # First val of direction is default if not set
    if(length(direction) > 1){ direction <- direction[1] }
    # Different start up if start model is given
    if( is.na(modelstart)[1] ){
        #
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
                    mfull$insert_prm(round(unlist(getse(prm[i], "max")) - unlist(getse(prm[i], "min")) / 2))
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
            res <- list(value=Inf, par=m$get_prmbounds("init"))
        }else{
            # Optimize from the full model
            res <- optimfun(m, data, kseq, printout=printout, ...)
            # Keep it
            istep <- 1
            L[[istep]] <- list(model = m$clone_deep(), result = res)
        }
    }else{
        # The full model will not be changed from here, so don't need to clone it
        mfull <- modelfull
        m <- modelstart$clone()
        # Optimize from the model
        res <- optimfun(m, data, kseq, printout=printout, ...)
        # Keep it
        istep <- 1
        L[[istep]] <- list(model = m$clone_deep(), result = res)
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
        # Next step
        istep <- istep + 1
        # Insert the optimized parameters from last step
        m$prmbounds[names(res$par),"init"] <- res$par
        #
        message("------------------------------------\n")
        message("Current model:")
        print(m)
        message("Current score: ",res$value,"\n")
        # --------
        mStep <- list()
        # Generate the input modified models
        if(length(grep("backward|both", direction))){
            # Remove input from the current model one by one
            if(length(m$inputs) > 1){
                tmp <- mclapply(1:length(m$inputs), function(i){
                    ms <- m$clone_deep()
                    # Remove one input
                    ms$inputs[[i]] <- NULL
                    return(ms)
                })
                mStep <- c_mStep(tmp, pst("-",names(m$inputs)))
            }
        }
        if(length(grep("forward|both", direction))){
            # Add input one by one
            iin <- which(!names(mfull$inputs) %in% names(m$inputs))
            if(length(iin)){
                tmp <- mclapply(iin, function(i){
                    ms <- m$clone_deep()
                    # Add one input
                    ms$inputs[[length(ms$inputs) + 1]] <- mfull$inputs[[i]]
                    names(ms$inputs)[length(ms$inputs)] <- names(mfull$inputs)[i]
                    return(ms)
                })
                mStep <- c_mStep(tmp, pst("+",names(mfull$inputs)[iin]))
            }
        }

        # Step parameters
        if(!is.na(prm[1])){
            if(length(grep("backward|both", direction))){
                # Count down the parameters one by one
                tmp <- mclapply(1:length(prm), function(i){
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
        resStep <- mclapply(1:length(mStep), function(i, ...){
            res <- optimfun(mStep[[i]], data, kseq, printout=printout, ...)
#            res <- try()
#            if(class(res) == "try-error"){ browser() }
            message(names(mStep)[[i]], ": ", res$value)
            return(res)
        }, ...)
        names(resStep) <- names(mStep)
        
        # Is one the step models score smaller than the current ref?
        valStep <- unlist(getse(resStep, "value"))
        imin <- which.min(valStep)
        if( valStep[imin] < res$value ){
            # Keep the best model
            m <- mStep[[imin]]
            res <- resStep[[imin]]
            # Keep for the result
            L[[istep]] <- list(model = m$clone_deep(), result = resStep[[imin]])
        }else{
            # No improvement obtained from reduction, so return the current model (last in the list)
            message("------------------------------------\n\nDone")
            message(print(m))
            done <- TRUE
        }
    }
    invisible(L)
}
