#' Depth of a list
#'
#' Returns the depth of a list
#' @title Depth of a list
#' @param this list
#' @return integer
depth <- function(this) ifelse(is.list(this), 1L + max(sapply(this, depth)), 0L)
