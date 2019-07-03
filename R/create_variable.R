#' Create a variable
#'
#' @param value a numeric vector with the value(s) to fill the array.
#' @param indexes a named list with the indexes of the array.
#' @param type the variable type: "defined" or "undefined".
#' @param desc variable description.
#'
#' @return A named list with the values and type for the variable.
#' @export
#' @examples
#'
#'C <- c("sec1", "sec2", "sec3")
#'
#'create_variable(
#'  value = 1,
#'  indexes = list(C = C),
#'  type = "defined",
#'  desc = "variable description"
#')
#'

create_variable <- function(value = 1, indexes, type = c("defined", "undefined"),
                            desc){

  type <- match.arg(type, c("defined", "undefined"))

  if(typeof(indexes) == "character" & length(indexes) == 1){
    name_index <- indexes
    indexes <- list(indexes)
    names(indexes) <- name_index
  }

  if(is.data.frame(value)){
    value <- df_to_array(value, indexes)
  } else{
    value <- create_array(value = value,
                          indexes = indexes)
  }

  list(
    value = value,
    type = type,
    desc = desc
  )

}
