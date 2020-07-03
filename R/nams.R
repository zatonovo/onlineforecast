#' Return the column names of a dataframe or a matrix.
#'
#' Simply to have a single function for returning the column names, instead of
#' \code{colnames()} for a \code{matrix} and \code{names()} for a \code{data.frame}).
#' 
#' @title Return the column names
#' @param x 
#' @examples
#' 
#' X <- matrix(1, nrow=2, ncol=3)
#' colnames(X) <- c("c1","c2","c3")
#' D <- as.data.frame(X)
#' 
#' # Annoyingly this fails
#' \dontrun{names(X)}
#' # Could use this everywhere
#' colnames(D)
#' # but this is shorter
#' nams(X)
#' nams(D)
#'
#' # Also for assignment
#' nams(D) <- c("x1","x2","x3")
#' nams(D)
#'
#' @export
nams <- function(x) {
    if(is.matrix(x)){
        colnames(x)
    } else {
        names(x)
    }
}


#' @export
`nams<-` <- function(x, value) {
    if(is.matrix(x)){
        colnames(x) <- value
    } else {
        names(x) <- value
    }
  x
}
