## Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?lg

lag_vector <- function(x, lag){
    if (lag > 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(rep(NA, lag), x[1:(length(x) - lag)]))
    }else if(lag < 0) {
        ## Lag x, i.e. delay x lag steps
        return(c(x[(abs(lag) + 1):length(x)], rep(NA, abs(lag))))
    }else{
        ## lag = 0, return x
        return(x)
    }
}

#' Lagging of a vector by simply the values back or fourth.
#'
#' This function lags (shifts) the values of the vector. If \code{lagseq} is a single integer value, then a
#' vector is returned. If \code{lagseq} is an integer vector, then a data.frame is returned with the columns
#' as the vectors lagged with the values in lagseq.
#'
#' 
#' @title Lagging of a vector
#' @param x The vector to be lagged.
#' @param lagseq The integer(s) setting the lag steps.
#' @return A vector or a data.frame.
#' @rdname lg
#' @seealso \code{\link{lg.data.frame}} which is run when \code{x} is a data.frame.
#' @examples
#' # The values are simply shifted
#' # Ahead in time
#' lg(1:10, 3)
#' # Back in time
#' lg(1:10, -3)
#' # Works but returns a numric
#' lg(as.factor(1:10), 3)
#' # Works and returns a character
#' lg(as.character(1:10), 3)
#' # Giving several lag values
#' lg(1:10, c(1:3))
#' lg(1:10, c(5,3,-1))
#'
#' # See also how to lag a forecast data.frame
#' ?lg.data.frame
#'
#'
#'@export

lg <- function(x, lagseq){
    UseMethod("lg")
}


#' @export
lg.numeric <- function(x, lagseq) {
    if(length(lagseq) == 1){
        return(lag_vector(x, lagseq))
    }else{
        ## Return a data.frame
        tmp <- lapply_cbind_df(lagseq, function(lag){
            return(lag_vector(x, lag))
        })
        names(tmp) <- pst("k",lagseq)
        return(tmp)
    }
}


#' @export
lg.factor <- function(x, lagseq) {
    lg.numeric(x, lagseq)
}


#' @export
lg.character <- function(x, lagseq) {
    lg.numeric(x, lagseq)
}

#' @export
lg.logical <- function(x, lagseq) {
    lg.numeric(x, lagseq)
}


#' Lagging of a data.frame
#'
#' This function lags the columns with the integer values specified with the argument \code{lagseq}.
#' 
#' @title Lagging of a data.frame
#' @param x The data.frame to have columns lagged
#' @param lagseq The sequence of lags as an integer. Alternatively, as a character "+k", "-k", "+h" or "-h", e.g. "k12" will with "+k" be lagged 12.
#' @return A data.frame with columns that are lagged
#' @rdname lg
#' @examples
#' 
#' # dataframe of forecasts
#' X <- data.frame(k1=1:10, k2=1:10, k3=1:10)
#' X
#'
#' # Lag all columns
#' lg(X, 1)
#' \dontshow{if(!all(is.na(lg(X, 1)[1, ]))){stop("Lag all columns didn't work")}}
#'
#' # Lag each column different steps
#' lg(X, 1:3)
#' # Lag each columns with its k value from the column name
#' lg(X, "+k")
#' \dontshow{
#'     if(any(lg(X, 1:3) != lg(X, "+k"),na.rm=TRUE)){stop("Couldn't lag +k")}
#' }
#' # Also works for columns named hxx
#' names(X) <- gsub("k", "h", names(X))
#' lg(X, "-h")
#'
#' # If not same length as columns in X, then it doesn't know how to lag
#' \donttest{#lg(X, 1:2)}
#'
#' \dontshow{
#' if(!class(lg(data.frame(k1=1:10), 2)) == "data.frame"){stop("Trying to lag data.frame with 1 column, but return is not class data.frame")}
#' if(!all(dim(lg(data.frame(k1=1:10), "+k")) == c(10,1))){stop("Trying to lag data.frame with 1 column, but return is not class data.frame")}
#' }
#'
#' @export
lg.data.frame <- function(x, lagseq) {
    X <- x
    nms <- nams(X)
    if (length(lagseq) == 1) {
        if (lagseq %in% c("+k","+h")) {
            lagseq <- rep(0, length(nms))
            ## lagseq according to the k value of the columnnames
            i <- grep("^[k|h][[:digit:]]+$", nms)
            lagseq[i] <- as.integer(sapply(strsplit(nms[i], "[k|h]"), function(x){ x[length(x)] }))
        } else if (lagseq %in% c("-k","-h")) {
            lagseq <- rep(0, length(nms))
            ## lagseq according to the negative k value of the columnnames
            i <- grep("^[k|h][[:digit:]]+$", nms)
            lagseq[i] <- -as.integer(sapply(strsplit(nms[i], "[k|h]"), function(x){ x[length(x)] }))
        }
    }
    if (length(lagseq) > 1) {
        if(length(lagseq) != ncol(X)){
            stop(pst("Must have same columns as length of lagseq: data.frame has ",ncol(X)," columns and laqseq is of length ",length(lagseq)))
        }else{
            ## lagseq has length equal to the number of columns in X
            X <- as.data.frame(sapply(1:length(lagseq), function(i) {
                lag_vector(X[, i], lagseq[i])
            }))
            nams(X) <- nms
         }
    } else {
        ## X is a data.frame, but lag is a factor, so lag all
        lag <- lagseq
        ## If only one row in X given, then X it is a not a data.frame anymore (code above has changed it)
        if(is.vector(X)){
          X <- as.data.frame(lag_vector(X, lag))
          nams(X) <- nms
        } else {
            if (lag > 0) {
                X[(lag + 1):nrow(X), ] <- X[1:(nrow(X) - lag), ]
                X[1:lag, ] <- NA
            } else if (lag < 0) {
                lag <- -lag
                X[1:(nrow(X) - lag), ] <- X[(lag + 1):nrow(X), ]
                X[(nrow(X) - lag + 1):nrow(X), ] <- NA
            }
        }
     }
    return(X)
}

#' @export
lg.matrix <- function(x, lagseq){
    lg.data.frame(x, lagseq)
}

## ## Test
## x <- data.frame(k1=1:5,k2=6:10)
## ##
## lg(x, lagseq=1)
## source("nams.R")
## lg(as.matrix(x), lagseq=c(1,2))
## ##
## lg(x, lagseq="+k")
## lg(x, "+k")
## lg(x, "-k")

## lg.data.table <- function(x, nms, lagseq, per_reference = FALSE) {
##     DT <- x
##     if (!per_reference) {
##         ## Don't do it per reference
##         X <- DT[, ..nms]
##         for (i in 1:length(lagseq)) {
##             if (lagseq[i] > 0) {
##                 X[, `:=`(c(nams(X)[i]), shift(.SD, lagseq[i], NA, "lag")), .SDcols = c(nams(X)[i])]
##             } else if (lagseq[i] < 0) {
##                 X[, `:=`(c(nams(X)[i]), shift(.SD, -lagseq[i], NA, "lead")), .SDcols = c(nams(X)[i])]
##             }
##         }
##         return(X)
##     } else {
##         ## Here also names of the columns to be shifted should be given Do it per
##         ## reference
##         for (i in 1:length(lagseq)) {
##             if (lagseq[i] > 0) {
##                 DT[, `:=`(c(nms[i]), shift(.SD, lagseq[i], NA, "lag")), .SDcols = c(nms[i])]
##             } else if (lagseq[i] < 0) {
##                 DT[, `:=`(c(nms[i]), shift(.SD, -lagseq[i], NA, "lead")), .SDcols = c(nms[i])]
##             }
##         }
##         invisible(NULL)
##     }
## }

