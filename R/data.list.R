# Do this in a separate tmp.R file to check the documentation
# library(devtools)
# document()
# load_all(as.package("../../onlineforecast"))
# ?as.data.list
# ?data.list
#?as.data.list.data.frame


#' Make a data.list of the vectors and data.frames given.
#'
#' See the vignette 'setup-data' on how a data.list must be setup.
#' 
#' It's simply a list of class \code{data.list} holding:
#' 
#'   - vector \code{t}
#' 
#'   - vector(s) of observations
#' 
#'   - data.frames (or matrices) of forecast inputs
#' 
#' 
#' @title Make a data.list
#' @param ... Should hold: time t, observations as vectors and forecasts as data.frames
#' @return a data.list.
#' @examples
#' # Put together a data.list
#' # The time vector
#' time <- seq(ct("2019-01-01"),ct("2019-01-02"),by=3600)
#' # Observations time series (as vector)
#' xobs <- rnorm(length(time))
#' # Forecast input as a data.frame with columns names 'kxx', where 'xx' is the horizon
#' X <- data.frame(matrix(rnorm(length(time)*3), ncol=3))
#' names(X) <- pst("k",1:3)
#' 
#' D <- data.list(t=time, xobs=xobs, X=X)
#'
#' # Check it (see \code{?\link{summary.data.list}})
#' summary(D)
#' 
#' @export
data.list <- function(...) {
    structure(list(...), class = c("data.list","list"))
}


#' Take a subset of a data.list.
#'
#' Different arguments can be given to select the subset. See the examples.
#' 
#' @title Take a subset of a data.list.
#' @param x The data.list to take a subset of.
#' @param subset Given as the integer indexes or a logical vector, or alternatively \code{c(tstart,tend)}, where tstart and tend are either as POSIX or characters.
#' @param nms The names of the variables in \code{x} to be included.
#' @param kseq The k horizons of forecasts to be included.
#' @param lagforecasts Should the forecasts be lagged k steps (thus useful for plotting etc.).
#' @param pattern Regex pattern applied to select the variables in x to be included.
#' @param ... Not implemented.
#' @return a data.list with the subset.
#' @examples
#' # Use the data.list with building heat load 
#' D <- Dbuilding
#' # Take a subset for the example
#' D <- subset(D, 1:10, nms=c("t","Taobs","Ta","Iobs","I"), kseq=1:3)
#' 
#' # Take subset index 2:4
#' subset(D, 2:4)
#' 
#' # Take subset for a period
#' subset(D, c("2010-12-15 02:00","2010-12-15 04:00"))
#' 
#' # Cannot request a variable not there
#' try(subset(D, nms=c("x","Ta")))
#' 
#' # Take specific horizons
#' subset(D, nms=c("I","Ta"), kseq = 1:2)
#' subset(D, nms=c("I","Ta"), kseq = 1)
#' 
#' # Lag the forecasts such that they are aligned in time with observations
#' subset(D, nms=c("Taobs","Ta"), kseq = 2:3, lagforecasts = TRUE)
#' 
#' # The order follows the order in nms
#' subset(D, nms=c("Ta","I"), kseq = 2)
#' 
#' # Return variables mathing a regex
#' subset(D, kseq=2, pattern="^I")
#' 
#' # Take data for Ta and lag the forecasts (good for plotting and fitting a model)
#' X <- subset(Dbuilding, 1:1000, pattern="^Ta", kseq = 10, lagforecasts = TRUE)
#' 
#' # A scatter plot between the forecast and the observations
#' # (try lagforecasts = FALSE and see the difference)
#' plot(X$Ta$k10, X$Taobs)
#'
#' # Fit a model for the 10-step horizon
#' abline(lm(Taobs ~ Ta.k10, as.data.frame(X)), col=2)
#'
#' @export
subset.data.list <- function(x, subset = NA, nms = NA, kseq = NA, lagforecasts = FALSE, pattern = NA, ...) {
    D <- x
    # --------------------------------
    # Set nms if needed (find the columns to take)
    if(is.na(nms[1])){
        nms <- names(D)
    }
    # If a pattern is given then find the columns
    if(!is.na(pattern[1])){
        # If the pattern has an or "|", then split on it to get the right order of the names
        nms <- unlist(sapply(strsplit(pattern, "\\|")[[1]], function(pat){
            grep(pat, names(D), value=TRUE)
        }))
    }
    # --------------------------------
    # Input checks
    # Check if all variables are in nms
    if(!all(nms %in% names(D))){ stop(pst("The variable ",nms[nms %in% names(D)]," is not in D"))}
    #
    if(!is.na(kseq)[1]){
        lapply(1:length(nms), function(i){
            X <- D[[nms[i]]]
            if(class(X)[1] == "data.frame" ){
                # Check if holds forecasts by checking if any name is "kxx"
                if(length(grep("k[[:digit:]]+$", names(X))) > 0){
                    # If it holds forecasts, check that they are all there
                    if( !all(pst("k",kseq) %in% names(X)) ){
                        warning(pst("The variable ",nms[i]," contains ",pst(names(X),collapse=",")," hence doesn't contain all k in kseq = ",pst(kseq,collapse=",")))
                    }
                }
            }
        })
    }
    # --------------------------------
    # If subset is NA then set it
    if(is.na(subset[1])){
        if(is.null(dim(D[[1]]))){
            subset <- 1:length(D[[1]])
        }else{
            subset <- 1:dim(D[[1]])[1]
        }
    }else if(length(subset) == 2){
        if(inherits(subset,c("character","POSIXlt","POSIXct","POSIXt"))){
            # Start and end of a period is given
            subset <- in_range(subset[1], D$t, subset[2])
        }
    }else{
        # Check if a non-meaningful subset is given
        if(inherits(subset,"character")){
            stop("subset cannot be a character, except if it is of length 2 and can be converted in a POSIX, e.g. subset=c('2020-01-01','2020-01-10'. ")
        }
    }
    # Take all horizons k?
    if(is.na(kseq[1])){
        val <- lapply(D[nms], function(X) {
            if (inherits(X,"data.frame")) {
                return(X[subset, , drop=FALSE]) # drop = FALSE needed in case data frame only has 1 column, otherwise this does not return a data frame
            } else {
                return(X[subset])
            }
        })
    }else{
        # Multiple horizons (hence length(kseq) > 1)
        # Take the specified horizons
        val <- lapply(D[nms], function(X) {
            if (inherits(X,"data.frame")) {
                # Check if holds forecasts by checking if any name is "kxx"
                if(length(grep("k[[:digit:]]+$", names(X))) > 0){
                    return(X[subset,pst("k",kseq), drop=FALSE])
                }else{
                    return(X[subset, , drop=FALSE])
                }
            } else {
                return(X[subset])
            }
        })
    }
    # Lag the forecasts k if specified
    if(lagforecasts){
        val <- lapply(val, function(X){
            if(inherits(X,"data.frame") & length(grep("k[[:digit:]]+$",names(X))) > 0) {
                return(lagdf.data.frame(X, lagseq="+k"))
            }else{
                return(X)
            }
        })
    }
    class(val) <- c("data.list","list")
    return(val)
}


#' Converts a data.list to a data.frame.
#'
#' The forecasts in the data.list will result in columns named \code{varname.kxx} in the data.frame.
#' 
#' @title Convert to data.frame
#' @param x The data.list to be converted.
#' @param row.names Not used.
#' @param optional Not used.
#' @param ... Not used.
#' @return A data.frame
#' @examples
#'
#' #' # Use the data.list with building heat load 
#' D <- Dbuilding
#' # Take a subset
#' D <- subset(D, 1:5, nms=c("t","Taobs","Ta","Iobs","I"), kseq=1:3)
#'
#' # Convert to a data.frame, note the names of the forecasts are appended .kxx (i.e. for Ta and I)
#' as.data.frame(D)
#'
#' @export
as.data.frame.data.list <- function(x, row.names=NULL, optional=FALSE, ...){
    # Then convert into a data.frame
    val <- do.call("cbind", x)
    if(inherits(val,"matrix")){
        val <- as.data.frame(val)
    }
    # Fix names of data.frames (i.e. forecasts, if their names are now "kxx", but should be X.kxx)
    i <- grep("^k[[:digit:]]+$", names(val))
    if(length(i) > 0){
        names(val)[i] <- pst(names(x)[i],".",names(val)[i])
    }
    return(val)
}


#' Generate a pairs plot for the vectors in the data.list.
#'
#' A very useful plot for checking what is in the forecasts, how they are synced and match the observations.
#' 
#' @title Generation of pairs plot for a data.list.
#' @param x The data.list from which to plot.
#' @param subset The subset to be included. Passed to \code{\link{subset.data.list}()}.
#' @param nms The names of the variables to be included. Passed to \code{\link{subset.data.list}()}.
#' @param kseq The horizons to be included. Passed to \code{\link{subset.data.list}()}.
#' @param lagforecasts Lag the forecasts such that they are synced with obervations. Passed to \code{\link{subset.data.list}()}.
#' @param pattern Regex pattern to select the included variables. Passed to \code{\link{subset.data.list}()}.
#' @param lower.panel Passed to \code{\link{pairs}()}.
#' @param panel Passed to \code{\link{pairs}()}.
#' @param pch Passed to \code{\link{pairs}()}.
#' @param cex Passed to \code{\link{pairs}()}.
#' @param ... Passed to \code{\link{pairs}()}.
#' @examples
#' # Take a subset for the example
#' D <- subset(Dbuilding, c("2010-12-15","2011-01-15"), pattern="^Ta|^I", kseq=1:3)
#' pairs(D)
#'
#' # If the forecasts and the observations are not aligned in time,
#' # which is easy to see by comparing to the previous plot.
#' pairs(D, lagforecasts=FALSE)
#' # Especially for the solar I syncronization is really important!
#' # Hence if the forecasts were not synced properly, then it can be detected using this type of plot.
#'
#' # Alternatively, lag when taking the subset
#' D <- subset(Dbuilding, c("2010-12-15","2011-01-15"), pattern="^Ta|^I", kseq=1:3, lagforecasts=TRUE)
#' pairs(D, lagforecasts=FALSE)
#'
#' @importFrom graphics panel.smooth pairs
#' @export
pairs.data.list <- function(x, subset = NA, nms = NA, kseq = NA, lagforecasts = TRUE, pattern = NA, lower.panel=NULL, panel=panel.smooth, pch=20, cex=0.7, ...){
    # First take the subset
    X <- as.data.frame(subset(x, subset = subset, nms = nms, kseq = kseq, lagforecasts = lagforecasts, pattern = pattern))
    #
    pairs(X, lower.panel=lower.panel, panel=panel, pch=pch, cex=cex, ...)
}


#' Summary including checks of the data.list for appropriate form. 
#'
#' Prints on table form the result of the checks.
#' 
#' @title Summary with checks of the data.list for appropriate form. 
#' @param object The object to be summarized and checked
#' @param printit A boolean deciding if check results tables are printed
#' @param stopit A boolean deciding if the function stop with an error if the check is not ok
#' @param nms A character vector. If given specifies the variables (vectors or matrices) in object to check
#' @param msgextra A character which is added in the printout of an (potential) error message
#' @param ... Not used
#' @return The tables generated.
#'
#' Checking the data.list for appropriate form:
#'
#' A check of the time vector t, which must have equidistant time points and no NAs.
#'
#' Then the results of checks of vectors (observations):
#' 
#'   - NAs: Proportion of NAs
#' 
#'   - length: Same length as the 't' vector?
#' 
#'   - class: The class of the vector
#' 
#' Then the results of checking data.frames and matrices (forecasts):
#' 
#'   - maxHorizonNAs: The proportion of NAs for the horizon (i.e. column) with the highest proportion of NAs
#' 
#'   - meanNAs: The proportion of NAs of the entire matrix
#' 
#'   - nrow: Same length as the 't' vector?
#' 
#'   - colnames: Columns must be names 'kx', where 'x' is the horizon (e.g. k12 is 12-step horizon)
#' 
#'   - sameclass: Error if not all columns are the same class
#' 
#'   - class: Prints the class of the columns if they are all the same
#' 
#' @examples
#' 
#' summary(Dbuilding)
#' 
#' # Some NAs in k1 forecast
#' D <- Dbuilding
#' D$Ta$k1[1:1500] <- NA
#' summary(D)
#'
#' # Vector with observations not same length as t throws error
#' D <- Dbuilding
#' D$heatload <- D$heatload[1:10]
#' try(summary(D))
#' 
#' # Forecasts wrong count
#' D <- Dbuilding
#' D$Ta <- D$Ta[1:10, ]
#' try(summary(D))
#' 
#' # Wrong column names
#' D <- Dbuilding
#' names(D$Ta)[4] <- "xk"
#' names(D$Ta)[2] <- "x2"
#' try(summary(D))
#' 
#' # No column names
#' D <- Dbuilding
#' names(D$Ta) <- NULL
#' try(summary(D))
#' 
#' # Don't stop or only print if stopped 
#' onlineforecast:::summary.data.list(D, stopit=FALSE)
#' try(onlineforecast:::summary.data.list(D, printit=FALSE))
#'
#' # Only check for specified variables
#' # (e.g. do like this in model functions to check only variables used in model)
#' onlineforecast:::summary.data.list(D, nms=c("heatload","I"))
#' 
#' @export
summary.data.list <- function(object, printit=TRUE, stopit=TRUE, nms=names(object), msgextra="", ...){
    D <- object

    # The final message
    msg <- NULL

    # Check the time vector
    if(!"t" %in% names(D)){ msg <- c(msg,"'t' is missing in the data.list: It must be a vector of equidistant time points (can be an integer, but preferably POSIXct class with tz 'GMT' or 'UTC'.)")}
    if(length(D$t) > 1){
        if(length(unique(diff(D$t))) != 1){ msg <- c(msg,"'t' is not equidistant or have NA values.") }
    }

    # Which elements are data.frame or matrix?
    isMatrix <- sapply(D, function(x){ inherits(x,c("matrix","data.frame")) })
    
    # Vectors check
    vecseq <- which(!isMatrix  &  names(isMatrix) != "t"  & names(isMatrix) %in% nms)
    Observations <- NA
    if(length(vecseq) > 0){
        vecchecks <- c("NAs","length","class")
        Observations <- data.frame(matrix("ok", nrow=length(vecseq), ncol=length(vecchecks), dimnames=list(pst("$",names(vecseq)),vecchecks)), stringsAsFactors=FALSE)
        #
        for(i in 1:length(vecseq)){
            #
            nm <- names(vecseq)[i]
            # NAs
            NAs <- round(max(sum(is.na(D[nm])) / length(D[nm])))
            Observations$NAs[i] <- pst(NAs,"%")
            # Check the length
            if(length(D[[nm]]) != length(D$t)){
                Observations$length[i] <- "ERROR"
                msg <- c(msg,pst(rownames(Observations)[i]," (length ",length(D[[nm]]),"), not same length as t (length ",length(D$t),")"))
            }
            # Its class
            Observations$class[i] <- class(D[[nm]])
        }
    }

    # Forecasts check
    dfseq <- which(isMatrix  &  names(isMatrix) %in% nms)
    Forecasts <- NA
    if(length(dfseq) > 0){
        dfchecks <- c("maxHorizonNAs","NAs","nrow","colnames","sameclass","class")
        Forecasts <- data.frame(matrix("ok", nrow=length(dfseq), ncol=length(dfchecks), dimnames=list(pst("$",names(dfseq)),dfchecks)), stringsAsFactors=FALSE)
        #
        for(i in 1:length(dfseq)){
            #
            nm <- names(dfseq)[i]
            colnms <- nams(D[[nm]])
            if(is.null(colnms)){
                msg <- c(msg, pst("'",nm,"' has no column names! Columns in forecast matrices must be named 'kx', where x is the horizon (e.g. 'k12' is the column with the 12 step forecast)"))
                Forecasts[i, ] <- rep(NA,ncol(Forecasts))
            }else{
                # max NAs
                tmp <- round(max(sapply(colnms, function(colnm){ 100*sum(is.na(D[[nm]][ ,colnm])) / nrow(D[[nm]]) })))
                Forecasts$maxHorizonNAs[i] <- pst(tmp,"%")
                # Mean NAs
                tmp <- round(mean(sapply(colnms, function(colnm){ 100*sum(is.na(D[[nm]][ ,colnm])) / nrow(D[[nm]]) })))
                Forecasts$NAs[i] <- pst(tmp,"%")
                # Check the number of rows
                if(nrow(D[[nm]]) != length(D$t)){
                    Forecasts$nrow[i] <- "ERROR"
                    msg <- c(msg, pst(nm," has ",nrow(D[[nm]])," rows, must be equal to length of t (n=",length(D$t),")"))
                }
                # Check the colnames, are they unique and all k+integer?
                tmp <- unique(grep("k[[:digit:]]+$",colnms,value=TRUE))
                if(!length(tmp) == length(colnms)){
                    Forecasts$colnames[i] <- "ERROR"
                    msg <- c(msg, pst(nm," has columns named: '",pst(colnms[!(colnms %in% tmp)],collapse="','"),"'. Columns in forecast matrices must be named 'kx', where x is the horizon (e.g. 'k12' is the column with the 12 step forecast)"))
                }
                if(!length(unique(sapply(colnms, function(colnm){ class(D[[nm]][ ,colnm]) }))) == 1){
                    Forecasts$sameclass[i] <- "ERROR"
                    msg <- c(msg, pst(nm," doesn't have same class for all columns"))
                }else{
                    Forecasts$class[i] <- class(D[[nm]][ ,1])
                }
            }
        }
    }

    # Print the results
    if(printit){
        cat("\nLength of time vector 't': ",length(D$t),"\n\n", sep="")
        if(length(vecseq) > 0){
        #    cat("\n- Observation vectors:\n")
            print(Observations)
        }
        if(length(dfseq) > 0){
            #   cat("\n- Forecast data.frames or matrices:\n")
            cat("\n")
            print(Forecasts)
        }
    }

    # Error message to print?
    if(length(msg) > 0){
        cat("\n")
        msg <- c(msg,"\nSee '?summary.data.list' for more information")
        # Stop or just print
        if(stopit){
            stop(pst(msg,collapse="\n"))
        }else{
            cat("ERRORS: \n",pst(msg,collapse="\n"),"\n")
        }
    }

    # Return
    invisible(list(Observations=Observations, Forecasts=Forecasts))
}



#' Compare two data.lists
#'
#' Returns TRUE if the two data.lists are fully identical, so all data, order of variables etc. must be fully identical
#' 
#' @title Determine if two data.lists are identical
#'
#' @param x first data.list  
#' @param y second data.list
#' @return logical
#'
#' @examples
#'
#' Dbuilding == Dbuilding
#'
#' D <- Dbuilding
#' D$Ta$k2[1] <- NA
#' Dbuilding == D
#'
#' D <- Dbuilding
#' names(D)[5] <- "I"
#' names(D)[6] <- "Ta"
#' Dbuilding == D
#' 
#' 
#' @export

"==.data.list" <- function(x, y) {
    if(length(x) != length(y)){
        return(FALSE)
    }
    if(any(names(x) != names(y))){
        return(FALSE)
    }
    # Check each variable
    tmp <- lapply(1:length(x), function(i){
        xi <- x[[i]]
        yi <- y[[i]]
        if(length(class(xi)) != length(class(yi))){
            return(FALSE)
        }
        if(any(class(xi) != class(yi))){
            return(FALSE)
        }
        if(is.null(dim(xi))){
            # It's a vector
            if(length(xi) != length(yi)){
                return(FALSE)
            }
        }else{
            # It's a data.frame or matrix
            if(any(dim(xi) != dim(yi))){
                return(FALSE)
            }
        }
        # Check the NA values are the same
        if(any(is.na(xi) != is.na(yi))){
            return(FALSE)
        }
        # Check the values
        all(xi == yi, na.rm=TRUE)
    })
    if(any(!unlist(tmp))){
        return(FALSE)
    }
    # All checks passed
    return(TRUE)
}
