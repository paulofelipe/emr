#' Convert a data.frame to array format.
#'
#' @param df A data.frame with the indexes columns and a value column.
#' @param indexes A names list with the indexes for the array.
#'
#' @return An array with the indexes dimension.
#' @export
#' @examples
#' library(emr)
#'
#' set1 <- c("A", "B")
#' set2 <- c("C", "D")
#' set3 <- c("imp", "exp")
#' sets <- list(
#'  set1 = set1,
#'  set2 = set2,
#'  set3 = set3
#' )
#' x <- expand.grid(
#'   set1 = set1,
#'   set2 = set2,
#'   set3 = set3,
#'   stringsAsFactors = TRUE
#' )
#' x$value <- 1:8
#' df_to_array(x, sets[(c('set1', 'set2', 'set3'))])

df_to_array <- function(df, indexes){

  indexes_names <- names(indexes)

  if(ncol(df) != length(indexes) + 1)
    stop('df should have ', length(indexes) + 1, ' columns\n',
         call. = FALSE)


  for(i in indexes_names){
    df[,i] <- factor(as.character(df[[i]]), levels = indexes[[i]])
  }

  set_sizes <- sapply(indexes, length)

  names_num <- names(df)[sapply(df, is.numeric)]

  df <- df[do.call("order", df[rev(indexes_names)]), ]
  a <- array(
    df[[names_num]],
    dim = set_sizes,
    dimnames = indexes
  )

  if(length(dim(a)) == 1){
    a <- as.vector(a)
    names(a) <- indexes[[1]]
  }

  if(length(dim(a)) == 2){
    a <- as.matrix(a)
  }
  a

}
