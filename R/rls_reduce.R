rls_reduce <- function(model, data, preduce=list(NA), scorefun = rmse){
    ## prm test
    ##preduce <- list(I__degree = c(min=1, init=7), mu_tday__nharmonics = c(min=1, init=7))
    prmin <- unlist(getse(preduce, 1))
    pr <- unlist(getse(preduce, 2))
    ##!! deep=TRUE didn't work, gave: "Error: C stack usage  9524532 is too close to the limit"
    m <- model$clone_deep()
    ## Insert the starting p reduction values
    if(!is.na(preduce[1])){
        m$insert_prm(pr)
    }
    ##
    library(parallel)
    ##
    valref <- rls_optim(m, data, printout=FALSE)$value
    ##
    while(TRUE){
        ##
        cat("------------------------------------\nReference score value",valref,"\n")
        ## --------
        ## Remove inputs one by one
        cat("\nRemoving inputs one by one\n")
        valsrm <- mclapply(1:length(model$inputs), function(i){
            mr <- m$clone_deep()
            mr$inputs[[i]] <- NULL
            rls_optim(mr, data, printout=FALSE)$value
        })
        valsrm <- unlist(valsrm)
        names(valsrm) <- names(m$inputs)
        cat("Scores\n")
        print(valsrm)
        ## --------
        ## Reduce parameter values if specified
        if(!is.na(pr[1])){
            cat("\nReducing prm with -1 one by one\n")
            valspr <- mclapply(1:length(pr), function(i){
                mr <- m$clone_deep()
                p <- pr
                ## Only count down if above minimum
                if( p[i] >= prmin[i] ){
                    p[i] <- p[i] - 1
                }
                mr$insert_prm(p)
                val <- rls_optim(mr, data, printout=FALSE)$value
                ##
                return(val)
            })
            valspr <- unlist(valspr)
            names(valspr) <- names(pr)
            cat("Scores\n")
            print(valspr)
        }
        ## Is one the reduced smaller than the current ref?
        if( min(c(valsrm,valspr)) < valref ){
            if(which.min(c(min(valsrm),min(valspr))) == 1){
                ## One of the models with one of the inputs removed is best
                imin <- which.min(valsrm)
                cat("Removing input",names(m$inputs)[imin],"\n")
                m$inputs[[imin]] <- NULL
            }else{
                ## One of the models with reduced parameter values is best
                imin <- which.min(valspr)
                pr[imin] <- pr[imin] - 1
                m$insert_prm(pr)
                cat("Reduced parameter",names(pr)[imin],"to:",pr[imin],"\n")
            }
            valref <- min(c(valsrm,valspr))
        }else{
            ## No improvement obtained from reduction, so return the current model
            cat("------------------------------------\n\nDone\n")
            return(m)
        }
    }
}
