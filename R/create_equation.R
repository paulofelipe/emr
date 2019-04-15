#' Create a model equation
#'
#' @param equation a string with the equation expression.
#' @param indexes a vector with index and sets specifications. Example: "c in COM".
#' @param type the equation type: "defining" or "mcc".
#' @param desc equation description.
#'
#' @return A named list with the values and type for the variable.
#' @export
#' @examples
#'
#'create_equation(
#'  equation = "x[c] = alpha * M / p[c]",
#'  indexes = "c in COM",
#'  type = "defining",
#'  desc = "Commodity demand"
#')
#'

create_equation <- function(equation, indexes = NULL,
                            type = c("defining", "mcc"),
                            desc){

  type <- match.arg(type, c("defining", "mcc"))

  list(
    equation = equation,
    indexes = indexes,
    type = type,
    desc = desc
  )

}
