## TEMPLATE FOR MAKING DOCUMENTATION OF R-FUNCTIONS
## by Linde :)
## see for more info : https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html

## put the below before your function and fill in
## the ?example will only work after running "document()". Then create .Rd file in folder /man
## So "document()" should be run in make file I think?

#' Write a short description of the function here (this will be title of help page, and "description")
#'
#' @param x Describe the parameter
#' @param y And maybe there are more
#' @return Write what the function returns, for example: The sum of \code{x} and \code{y}
#' @examples
#' example()
#' example()

example <- function() {
    blabla <- 0
    return ( bla )
}

#?example
