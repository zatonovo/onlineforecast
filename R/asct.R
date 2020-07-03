# Do this in a separate file to see the generated help:
#library(devtools)
#document()
#load_all(as.package("../../onlineforecast"))
#?asct
#?asct.default

#' The object is converted into POSIXct with tz="GMT".
#'
#' A simple helper, which wraps \code{\link{as.POSIXct}}` and sets the time zone to "GMT" per default.
#' 
#' @title Convertion to POSIXct
#' @param object The object to convert can be: character, numeric, POSIXct or POSIXlt
#' @param tz Timezone. If set, then the time zone will be changed of the object.
#' @param ... 
#' @return An object of class POSIXct
#' @section Methods:
#' @examples
#'
#'
#' # Create a POSIXct with tz="GMT"
#' asct("2019-01-01")
#' class(asct("2019-01-01"))
#' asct("2019-01-01 01:00:05")


#' # Convert to POSIXct
#' class(asct(as.POSIXlt(x)))
 
#' # To seconds and back again
#' asct(as.numeric(x, units="sec"))


#' # --------
#' # Convert character of time which has summer time leaps
#' # Example from CET (with CEST which is winter time)
#' # 
#' # The point of shifting to and from summer time:
#' # DST Start (Clock Forward)	DST End (Clock Backward)
#' # Sunday, March 31, 02:00	Sunday, October 27, 03:00

#' # --------
#' # From to winter time to summer time
#' txt <- c("2019-03-31 01:00",
#'          "2019-03-31 01:30",
#'          "2019-03-31 03:00",
#'          "2019-03-31 03:30")
#' x <- asct(txt, tz="CET")
#' x
#' asct(x, tz="GMT")

#' # BE AWARE of this conversion of the 02:00: to 02:59:59 (exact time of shift) will lead to a wrong conversion
#' txt <- c("2019-03-31 01:30",
#'          "2019-03-31 02:00",
#'          "2019-03-31 03:30")
#' x <- asct(txt, tz="CET")
#' x
#' asct(x, tz="GMT")
#' # Which a diff on the time can detect, since all steps are not equal
#' plot(diff(asct(x, tz="GMT")))
 
#' # --------
#' # Shift to winter time is more problematic
#' # It works like this 
#' txt <- c("2019-10-27 01:30",
#'          "2019-10-27 02:00",
#'          "2019-10-27 02:30",
#'          "2019-10-27 03:00",
#'          "2019-10-27 03:30")
#' x <- asct(txt, tz="CET")
#' x
#' asct(x, tz="GMT")

#' # however, timestamps can be given like this
#' txt <- c("2019-10-27 01:30",
#'          "2019-10-27 02:00",
#'          "2019-10-27 02:30",
#'          "2019-10-27 02:00",
#'          "2019-10-27 02:30",
#'          "2019-10-27 03:00",
#'          "2019-10-27 03:30")
#' x <- asct(txt, tz="CET")
#' x
#' asct(x, tz="GMT")
#' # Again can be detected, since all steps are not equal
#' plot(diff(asct(x, tz="GMT")))
#' # This can be fixed by (note that it can go wrong, e.g. with gaps around convertion etc.)
#' asct(x, tz="GMT", duplicatedadd=3600)
#'
#' @export

asct <- function(object, tz, ...){
    UseMethod("asct")
}


#' @rdname asct
#' @section Methods:
#'     - asct.character: Simply a wrapper for \code{as.POSIXct} with default \code{tz}
#' @export
asct.character <- function(object, tz = "GMT", ...){
    as.POSIXct(object, tz=tz, ...)
}

#' @rdname asct
#' @section Methods:
#'     - asct.POSIXct: Changes the time zone of the object if \code{tz} is given.
#' @export
asct.POSIXct <- function(object, tz = NA, duplicatedadd = NA){
    if(!is.na(tz)){
        attr(object, "tzone") <- tz
    }
    if(!is.na(duplicatedadd)){
        # To mitigate the problem of duplicated timestamps at the shift to winter time
        # then shift the time of duplicated time stamps with the seconds given
        object[which(duplicated(object))] <- object[which(duplicated(object))] + duplicatedadd
    }
    return(object)
}

#' @rdname asct
#' @section Methods:
#'     - asct.POSIXlt: Converts to POSIXct.
#' @export
asct.POSIXlt <- function(object, tz = NA, duplicatedadd = NA){
    as.POSIXct(asct.POSIXct(object, tz, duplicatedadd))
}

#' @rdname asct
#' @section Methods:
#'     - asct.numeric: Converts from UNIX time in seconds to POSIXct with \code{tz} as GMT.
#' @export
asct.numeric <- function(object){
    ISOdate(1970, 1, 1, 0) + object
}
