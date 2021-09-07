## Do this in a separate tmp.R file to check the documentation
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?make_tday

#' Make an hour-of-day forecast matrix
#'
#' This function creates a data.frame with k-steps-ahead values of hour of day,
#' such that it can be added to a data.list and used inputs to a forecast model.
#' 
#' @param time vector of times of class "POSIXct" "POSIXt".
#' @param kseq vector of integers, representing the desired "k-steps ahead".
#' @param tstep step time of k in seconds.
#' @return Returns a forecast matrix (data.frame) with rownames = times, colnames = k1, k2, k3, ...
#' The content of the data frame is the hour of day.
#' @keywords hourofday lags data.frame
#' @seealso make_periodic
#' @examples
#' # Create a time sequence of 30 min sample period
#' tseq <- seq(ct("2019-01-01"), ct("2019-02-01 12:00"), by=1800)
#' 
#' # Make the time of day sequence (assuming time between k steps is same as for tseq)
#' make_tday(tseq, 1:10)
#' 
#' # With 0.5 hour steps, kstep in hours
#' make_tday(tseq, 1:10, tstep=3600)
#'
#' 
#' @export

make_tday <- function(time, kseq, tstep=NA){
    # If tstep not given, then take it from time assuming same k-step is the sampling period
    if(is.na(tstep)){
        tstep <- as.numeric(time[2] - time[1], units="secs")
    }
    # The time of day (in the specified units)
    tday <- sapply(kseq, function(k){
        tk <- time + k * tstep
        as.numeric( tk - trunc(tk, units="days"), units="hours")
    })
    # set row and column names
    nams(tday) <- paste0('k', kseq)
    return( as.data.frame(tday) )
}
