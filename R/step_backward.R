#' @importFrom parallel mclapply
#'

step_backward <- function(object, data, kseq = NA, prm=list(NA), optimfun = rls_optim, scorefun = rmse, ...){
    # Do:
    # - Maybe have "cloneit" argument in optimfun, then don't clone inside optim.
    # - Add argument controlling how much is kept in each iteration (e.g all fitted models)
    #
    # - Help: prm <- list(I__degree = c(min=1, max=7), mu_tday__nharmonics = c(min=1, max=7))
    # - help: It's not checked that it's the score is calculated on the same values! WARNING should be printed if some models don't forecast same points
    #
    model <- object
    #
    m <- model$clone_deep()
    # Insert the starting prm reduction values
    if(!is.na(prm[1])){
        prmMin <- unlist(getse(prm, "min"))
        # ??insert_prm should keep only the ones that can be changed
        m$insert_prm(unlist(getse(prm, "max")))
    }
    # For keeping all the results
    L <- list()
    istep <- 1
    # Optimize the reference model
    res <- optimfun(m, data, kseq, printout=TRUE, ...)
    valRef <- res$value
    L[[istep]] <- list(model = m$clone_deep(), result = res)
    #
    done <- FALSE
    while(!done){

        #
        istep <- istep + 1
        # Insert the optimized parameters from last step
        m$prmbounds[names(res$par),"init"] <- res$par
        #
        message("------------------------------------")
        message("Reference score value: ",valRef)
        # --------
        # Generate the reduced models
        mReduced <- mclapply(1:length(m$inputs), function(i){
            mr <- m$clone_deep()
            # Insert the optimized parameters from the reference model
            mr$inputs[[i]] <- NULL
            return(mr)
        })
        names(mReduced) <- names(m$inputs)
        if(!is.na(prm[1])){
            tmp <- mclapply(1:length(prm), function(i){
                p <- m$get_prmvalues(names(prm[i]))
                # If the input is not in model, then p is NA, so don't include it for fitting
                if(!is.na(p)){
                    # Only the ones with prms above minimum
                    if(p > prmMin[i]){
                        p <- p - 1
                        mr <- m$clone_deep()
                        mr$insert_prm(p)
                        return(mr)
                    }
                }                
                return(NA)
            })
            names(tmp) <- names(prm)
            tmp <- tmp[!is.na(tmp)]
            mReduced <- c(mReduced, tmp)
        }

        resReduced <- lapply(1:length(mReduced), function(i, ...){
            res <- optimfun(mReduced[[i]], data, kseq, printout=FALSE, ...)
            message(names(mReduced)[[i]], ": ", res$value)
            return(res)
        }, ...)
        names(resReduced) <- names(mReduced)
        valReduced <- unlist(getse(resReduced, "value"))
        imin <- which.min(valReduced)

        # Is one the reduced smaller than the current ref?
        if( valReduced[imin] < valRef ){
            # Keep the best model
            m <- mReduced[[imin]]
            res <- resReduced[[imin]]
            valRef <- res$value
            # Keep for the result
            L[[istep]] <- list(model = m$clone_deep(), result = resReduced[[imin]])
        }else{
            # No improvement obtained from reduction, so return the current model (last in the list)
            message("------------------------------------\n\nDone")
            done <- TRUE
        }
    }
    invisible(L)
}
