## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?long_format

#' Creates a long format of the predictions
#'
#' This functions creates a useful prediction data.frame which can be useful for analysis and plotting.
#'
#' 
#' @title Long format of prediction data.frame
#' @param fit The result from either lm_fit or rls_fit
#' @param Time If the timestamps are missing from the fit object
#' @return Data.frame of when the prediction where made, also the prediction value and timestamp.
#' @examples
#'
#' 
#' @export

long_format <- function(fit, Time = NULL){
    if(!("t" %in% names(fit))) {
        if(is.null(Time)) stop("Missing Time")
        fit$t <- Time
    }
    if(!("Yhat" %in% names(fit))) stop("Missing forecasts")
  
    predDF <- do.call(rbind, lapply(1:length(fit$t), function(i)
    {
        DF <- data.frame(PredTime = fit$t[i],
                   Time = fit$t[(i+1):(dim(fit$Yhat)[2]+i)],
                   k = 1:(dim(fit$Yhat)[2]),
                   Pred = as.numeric(fit$Yhat[i,]))
        return(DF)
    }))
    return(predDF)
}
