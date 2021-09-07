#' Calculate the residuals given a forecast matrix and the observations.
#'
#' Simply give the forecast matrix and the observations to get the residuals for each horizon in the forecast matrix.
#'
#' The residuals returned are synced with the observations (i.e. k0) and the columns are names "hxx" (not kxx) to indicate this and will not be lagged in \code{\link{plot_ts}()}.
#' 
#' @title Calculate the residuals given a forecast matrix and the observations.
#' @param object The forecast matrix (a data.frame with kxx as column names, Yhat in returned fits).
#' @param y The observations vector.
#' @param ... Not used.
#' @return If object is a matrix or data.frame: a data.frame with the residuals for each horizon.
#' If object is a list: A list with residuals from each element.
#'
#' @examples
#' # ?? list example
#' # Just a vector to be forecasted
#' n <- 100
#' D <- data.list()
#' D$t <- 1:n
#' D$y <- c(filter(rnorm(n), 0.95, "recursive"))
#' plot(D$y, type="l")
#' 
#' # Generate a forecast matrix with a simple persistence model
#' D$Yhat <- persistence(D$y, kseq=1:4)
#' 
#' # The residuals for each horizon
#' D$Resid <- residuals(D$Yhat, D$y)
#' D$Resid
#' # Note the names of the columns
#' names(D$Resid)
#' # which means that they are aligned with the observations and will not be lagged in the plot
#' plot_ts(D, c("y|Yhat","Resid"))
#'
#' # Check that it matches (the forecasts is lagged in the plot_ts
#' # such that the forecast for t+k is at t+k (and not t))
#' plot_ts(D, c("y|Yhat","Resid"), xlim=c(1,10), kseq=1,
#'         plotfun=function(x,...){lines(x,...,type="b")})
#'
#' # Just for fun, see the auto-correlation function of the persistence 
#' acf(D$Resid$h1, na.action=na.pass)
#' acf(D$Resid$h4, na.action=na.pass)
#'
#' @rdname residuals
#' @export
residuals.data.frame <- function(object, y, ...){
    # Improvements: Add some checking at some point
    # Calculate the residuals
    Residuals <- y - lagdf(object, "+k")
    # Named with hxx (it's not a forecast, but an observation available at t)
    nams(Residuals) <- gsub("k","h",nams(Residuals))
    #
    return(Residuals)
}

#' @rdname residuals
#' @export
residuals.matrix <- residuals.data.frame

#' @rdname residuals
#' @export
residuals.list <- function(object, y, ...){
    # Each element is a forecast matrix, so do it on each
    return(lapply(object, residuals, y=y))
}

#' @rdname residuals
#' @export
residuals.forecastmodel_fit <- function(object, ...){
    fit <- object
    return(residuals(fit$Yhat, fit$data[[fit$model$output]]))
}

