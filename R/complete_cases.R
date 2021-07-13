#' Returns a logical vector indicating the time points which 
#'
#' Given a forecast matrix the forecasts are lagged "+k" steps to align them and
#' then 'complete.cases()' is run on that .
#'
#' Gieven a list of forecast matrices the points where all are complete (also all horizons) are complete are TRUE.
#' 
#' @title Find complete cases in forecast matrices
#' @param object A data.frame (with columns named 'kxx') or a list of
#'     data.frames.
#' @param kseq integer vector: If given then only these horizons are processed.
#' @return A logical vector specifying if there is no missing
#'     values across all horizonsd.
#' @author Peder Bacher
#'
#' @examples
#' # Take a small data set
#' D <- subset(Dbuilding, 1:20, kseq=1:5)
#' # Check the forecast matrix of ambient temperature
#' D$Ta
#' # Which are complete over all horizons? The first are not since not all horizons
#' # have a value there (after lagging)
#' complete_cases(D$Ta)
#' # Same goes if given as a list
#' complete_cases(D["Ta"])
#' # and if more than one is given
#' complete_cases(D[c("Ta","I")])
#' 
#' # Set some NA of some horizon
#' D$I$k3[8:9] <- NA
#' # Now they are recognized as not complete
#' complete_cases(D[c("Ta","I")])
#'
#' # If we deal with residuals, which are observations and there for have column names "hxx"
#' Resid <- residuals(D$Ta, D$Taobs)
#' names(Resid)
#' # With columns with "h" instead of "k" no lagging occurs in complete_cases
#' complete_cases(Resid)
#' #
#' Resid2 <- Resid
#' Resid$h3[8:9] <- NA
#' complete_cases(list(Resid,Resid2))
#' 
#' @rdname complete_cases
#' @importFrom stats complete.cases
#' @export
complete_cases <- function(object, kseq = NA){
    UseMethod("complete_cases")
}


#' @rdname complete_cases
#' @importFrom stats complete.cases
#' @export
complete_cases.list <- function(object, kseq=NA){
    if(length(grep("[k][[:digit:]]+$", nams(object[[1]])))){
        prefix <- "k"
    }else{
        prefix <- "h"
    }
    # Do it only for the given kseq horizons, if kseq is not set, then take it from the first
    if(is.na(kseq[1])){
        kseq <- as.integer(gsub(prefix, "", nams(object[[1]])))
    }
    # Check that they all have kseq horizons
    # Init a logical matrix with each point
    ok <- matrix(TRUE, nrow(object[[1]]), length(kseq))
    #
    for(i in 1:length(object)){
        if(!all(kseq %in% as.integer(gsub(prefix, "", nams(object[[i]]))))){
            stop("Not all forecast matrices have kseq horizons: ",kseq)
        }
        ok <- ok & !is.na(object[[i]][ ,pst(prefix,kseq)])
    }
    #
    ok <- as.data.frame(ok)
    if(prefix == "k"){
        # Lag to match resiuduals in time
        names(ok) <- pst("k",kseq)
        ok <- lagdf(ok, "+k")
    }
    # Finally, the vector with TRUE for all points with no NAs for any forecast
    ok <- apply(ok, 1, all)
    # Just set NAs to false
    ok[is.na(ok)] <- FALSE
    #
    return(ok)
}


#' @rdname complete_cases
#' @importFrom stats complete.cases
#' @export
complete_cases.data.frame <- function(object, kseq=NA){
    # Make a distinction between "kxx" which are forecasts or "hxx", which are observations
    if(length(grep("[k][[:digit:]]+$", nams(object)[1]))){
        # Forecasts, so lag them
        object <- lagdf(object, "+k")        
    }
    complete.cases(object)
}
