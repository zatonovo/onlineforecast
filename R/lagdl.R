## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?lagdl

#' Lagging by shifting the values back or fourth always returning a data.list.
#'
#' This function lags (shifts) the values of the vector. A data.list is always returned with each data.frame lagged with \code{lagdf}.
#'
#' 
#' @title Lagging which returns a data.list
#' @param x The data.list to be lagged.
#' @param lagseq The integer(s) setting the lag steps.
#' @return A data.list.
#' @seealso \code{\link{lagdl.data.frame}} which is run when \code{x} is a data.frame.
#' @examples
#' # The values are simply shifted in each data.frame with lagdf
#'
#'@export

lagdl <- function(D, lagseq){
    iseq <- which(sapply(D,class) %in% c("data.frame","matrix"))
    D[iseq] <- lapply(iseq, function(i){
        lagdf(D[[i]], lagseq)
    })
    return(D)
}
