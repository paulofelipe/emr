#' Create an array from a list of an indexes
#'
#' @param value A numeric vector with the value(s) to fill the array
#' @param indexes A named list with the indexes of the array
#'
#' @return An array with the dimensions equal to the length of each index.
#' @export
#' @examples
#' create_array(value = 1, indexes = list(i = c("a", "b"), j = c("c", "d")))
#'

create_array <- function(value = 1, indexes){
  set_sizes <- sapply(indexes, function(x) length(x))
  a <- array(value, dim = set_sizes, dimnames = indexes)

  if(length(dim(a)) == 1){
    a <- as.vector(a)
    names(a) <- indexes[[1]]
  }

  if(length(dim(a)) == 2){
    a <- as.matrix(a)
  }
  a
}
