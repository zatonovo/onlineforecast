# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?anlt
#?anlt.default

#' The argument is converted into POSIXlt with tz="GMT".
#'
#' 
#' 
#' @title Convertion to POSIXlt
#' @param object The character, POSIXct, POSIClt, or numeric which is converted to POSIXct.
#' @param tz Timezone. If set, then the time zone will be changed of the object.
#' @param ... Arguments to be passed to methods.
#' @return An object of class POSIXlt
#' @section Methods:
#' #' @examples
#' 
#' # Create a POSIXlt with tz="GMT"
#' anlt("2019-01-01")
#' class(anlt("2019-01-01"))
#' anlt("2019-01-01 01:00:05")
#'
#' # Convert between time zones
#' x <- anlt("2019-01-01", tz="CET")
#' anlt(x,tz="GMT")
#'
#' # To seconds and back again
#' anlt(as.numeric(x, units="sec"))
#' 
#' @export
anlt <- function(object, ...){
    UseMethod("anlt")
}

#' @rdname anlt
#' @section Methods:
#'     - anlt.character: Simply a wrapper for \code{as.POSIXlt}
#' @export
anlt.character <- function(object, tz = "GMT", ...){
    as.POSIXlt(object, tz = tz, ...)
}

#' @rdname anlt
#' @section Methods:
#'     - anlt.POSIXct: Converts to POSIXct.
#' @export
anlt.POSIXct <- function(object, tz = NA, ...){
    if(!is.na(tz)){
        attr(object, "tzone") <- tz
    }
    as.POSIXlt(object, ...)
}

#' @rdname anlt
#' @section Methods:
#'     - anlt.POSIXlt: Changes the time zone of the object if tz is given.
#' @export
anlt.POSIXlt <- function(object, tz = NA, ...){
    if(!is.na(tz)){
        attr(object, "tzone") <- tz
    }
    return(object)
}

#' @rdname anlt
#' @section Methods:
#'     - anlt.numeric: Converts from UNIX time in seconds to POSIXlt.
#' @export
anlt.numeric <- function(object, ...){
    as.POSIXlt(ISOdate(1970, 1, 1, 0, ...) + object)
}
