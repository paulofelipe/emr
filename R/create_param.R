#' Create a parameter
#'
#' @param value a numeric vector with the value(s) to fill the array.
#' @param indexes a named list with the indexes of the array.
#' @param desc variable description.
#'
#' @return A named list with the values and type for the variable.
#' @export
#' @examples
#'
#'C <- c("sec1", "sec2", "sec3")
#'
#'create_param(
#'  value = 1,
#'  indexes = list(C = C),
#'  desc = "Parameter description"
#')
#'


create_param <- function(value = 1, indexes, desc){

  if(is.data.frame(value)){
    value <- df_to_array(value, indexes)
  } else{
    value <- create_array(value = value,
                          indexes = indexes)
  }

  list(
    value = value,
    desc = desc
  )

}
