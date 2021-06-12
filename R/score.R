# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?score

#' Calculates the score for each horizon for a matrix with residuals for each horizon.
#'
#' Applies the \code{scorefun} on all horizons (each column) of the residuals matrix. See the description of each parameter for more details.
#' 
#' @title Calculate the score for each horizon.
#' @param object ??list or A matrix with residuals (columns named \code{hxx}) for which to calculate the score for each horizon.
#' @param scoreperiod as a logical vector controlling which points to be included in the score calculation. If NA then all values are included (depeding on 'complete').
#' @param usecomplete TRUE then only the values available for all horizons are included (i.e. if at one time point there is a missing value, then values for this time point is removed for all horizons in the calculation).
#' @param scorefun The score function.
#' @param ... is passed on to the score function.
#' @return A list with the a numeric vector with the score value for each horizon and the applied \code{scoreperiod} (note can be different from the given scoreperiod, if only complete observations are used (as per default)).
#' @examples
#'
#' # Just a vector to be forecasted
#' y <- c(filter(rnorm(100), 0.95, "recursive"))
#' # Generate a forecast matrix with a simple persistence model
#' Yhat <- persistence(y, kseq=1:4)
#' # The residuals for each horizon
#' Resid <- residuals(Yhat, y)
#'
#' # Calculate the score for the k1 horizon
#' score(Resid)
#'
#' # In the beginning the horizons have NAs
#' head(Resid)
#' # Default is that only complete cases over all horizons are included
#' score(Resid)
#' # So including all cases for each horizon will give different values
#' score(Resid, usecomplete=FALSE)
#'
#' # Given a list
#' # The residuals for each horizon
#' Resid2 <- residuals(persistence(y,kseq=1:4)+rnorm(100), y)
#'
#' score(list(Resid,Resid2))

#' @rdname score
#' @export
score <- function(object, scoreperiod = NA, usecomplete = TRUE, scorefun = rmse, ...){
    UseMethod("score")
}


#' @rdname score
#' @export
score.list <- function(object, scoreperiod = NA, usecomplete = TRUE, scorefun = rmse, ...){
    # Use only complete cases
    if(usecomplete){
        tmp <- complete_cases(object)
    }else{
        tmp <- rep(TRUE,nrow(object))
    }
    if(!is.na(scoreperiod[1])){
        scoreperiod <- tmp & scoreperiod
    }else{
        scoreperiod <- tmp
    }
    # Run on each element, usecomplete has been dealt with
    sapply(object, score, usecomplete=FALSE, scoreperiod=scoreperiod, scorefun=scorefun, ...=...)
}


#' @rdname score
#' @export
score.data.frame <- function(object, scoreperiod = NA, usecomplete = TRUE, scorefun = rmse, ...){
    if(is.na(scoreperiod[1])){
        scoreperiod <- rep(TRUE,nrow(object))
    }
    if(usecomplete){
        scoreperiod <- complete_cases(object) & scoreperiod
    }
    # If no scoreperiod is given, then use all
    # Do checking of scoreperiod
    txt <- "It must be set to an index (int or logical) defining which points to be evaluated in the scorefun()."
    if( length(scoreperiod) != nrow(object) ){
        stop("scoreperiod is not same length as nrow(object): ",txt)
    }else{
        if( all(is.na(scoreperiod)) ){ stop("scoreperiod is all NA: ",txt) }
    }
    # Calculate the objective function for each horizon
    scoreval <- sapply(1:ncol(object), function(i){
        scorefun(object[scoreperiod,i], ...)
    })
    nams(scoreval) <- gsub("h","k",nams(object))
    # 
    return(scoreval)
}

