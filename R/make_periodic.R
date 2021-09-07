## Do this in a separate tmp.R file to check the documentation
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?make_periodic

#' Make an forecast matrix with a periodic time signal.
#'
#' This function creates a data.frame with k-steps-ahead values of a periodic time signal,
#' such that it can be added to a data.list and used inputs to a forecast model.
#' 
#' @param time vector of times of class "POSIXct" "POSIXt".
#' @param kseq vector of integers, representing the desired "k-steps ahead".
#' @param period a numeric setting the length of the period in seconds.
#' @param offset a numeric setting an offset in the period start in seconds.
#' @param tstep step time of k in seconds.
#' @return Returns a forecast matrix (data.frame) with rownames = times, colnames = k1, k2, k3, ...
#' The content of the data frame is the hour of day.
#' @keywords periodic lags data.frame
#' @seealso make_tday
#' @examples
#' # Create a time sequence of 30 min sample period
#' tseq <- seq(ct("2019-01-01"), ct("2019-02-01 12:00"), by=1800)
#' 
#' # Make the three hourly periodic sequence
#' make_periodic(tseq, 1:10, 3*3600)
#'
#' # With an offset of one hour
#' make_periodic(tseq, 1:10, 3*3600, 3600)
#' 
#' @export

make_periodic <- function(time, kseq, period, offset=0, tstep=NA){
    # If tstep not given, then take it from time assuming same k-step is the sampling period
    if(is.na(tstep)){
        tstep <- as.numeric(time[2] - time[1], units="secs")
    }
    # The time of day (in the specified units)
    tday <- sapply(kseq, function(k){
        tk <- time + k * tstep - offset
        as.numeric(tk,units="secs") %% period
    })
    # set row and column names
    nams(tday) <- paste0('k', kseq)
    return( as.data.frame(tday) )
}
