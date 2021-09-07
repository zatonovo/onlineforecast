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
#' @param DL The data.list to be lagged.
#' @param lagseq The integer(s) setting the lag steps.
#' @return A data.list.
#' @examples
#' # The values are simply shifted in each data.frame with lagdf
#'
#'@export

lagdl <- function(DL, lagseq){
    iseq <- which(sapply(DL,class) %in% c("data.frame","matrix"))
    DL[iseq] <- lapply(iseq, function(i){
        lagdf(DL[[i]], lagseq)
    })
    return(DL)
}
