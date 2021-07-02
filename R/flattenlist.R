#' Flattens list in a single list of data.frames
#'
#' Flattens list. Can maybe be made better. It might end up copying data in
#' memory!? It might change the order of the elements.
#' @title Flattens list
#' @param x List to flatten.
#' @return A flatten list
flattenlist <- function(x){
    (n <- depth(x))
    if(n == 2){
        # Its fine
        return(x)
    }else if(n ==3){
        unlist(x, recursive=FALSE)
    }else{
        morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
        out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
        if(sum(morelists)){ 
            Recall(out)
        }else{
            return(out)
        }
    }
}
