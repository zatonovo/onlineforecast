#' @importFrom parallel mclapply
#'

step_optim <- function(model, data, kseq = NA, prm=list(NA), direction = c("backward","forward","backwardboth","forwardboth"), optimfun = rls_optim, scorefun = rmse, ...){
    # Do:
    # - change all lapply 
    # - Maybe have "cloneit" argument in optimfun, then don't clone inside optim.
    # - Add argument controlling how much is kept in each iteration (e.g all fitted models)
    #
    # - Help: prm <- list(I__degree = c(min=1, max=7), mu_tday__nharmonics = c(min=1, max=7))
    # - help: It's not checked that it's the score is calculated on the same values! WARNING should be printed if some models don't forecast same points
    #
    # First direction is default
    if(length(direction) > 1){ direction <- direction[1] }
    # Don't change the given
    mfull <- model$clone_deep()
    # Insert the starting prm values
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
    # For keeping all the results
    L <- list()
    # 
    m <- mfull$clone_deep()
    if(length(grep("backward",direction))){
        # Optimize from the full model
        res <- optimfun(m, data, kseq, printout=TRUE, ...)
        # Keep it
        istep <- 1
        L[[istep]] <- list(model = m$clone_deep(), result = res)
    }else{
        # Optimize from the null model
        m$inputs <- list()
        # Must be set
        istep <- 0
        res <- list(value=Inf, par=m$get_prmbounds("init"))
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
        message("------------------------------------")
        message("Reference score value: ",res$value)
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
        
        # Optimize all the step models
        resStep <- mclapply(1:length(mStep), function(i, ...){
            res <- try(optimfun(mStep[[i]], data, kseq, printout=FALSE, ...))
            if(class(res) == "try-error"){ browser() }
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
