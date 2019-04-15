#' Create and parse a model equation
#'
#' @param equation A text defining the equation.
#' @param envir A named list with the indexes for the array.
#'
#' @return An expression with the code to evaluate the equation.
#' @export
#' @importFrom glue glue glue_collapse
#' @examples
#'equation <- list(
#' equation = "y[c] = x[c]^2",
#' indexes = "c in C"
#')
#'
#'C <- c("sec1", "sec2", "sec3")
#'
#'parse_equation(equation, envir = globalenv())
#'

parse_equation <- function(equation, envir){

  eq <- equation$equation
  sets <- equation$indexes

  if(is.null(sets)){
    new_eq <- eq
  }

  if(!is.null(sets)){
    sets_names <- strsplit(sets, " in ")
    sets_names <- sapply(sets_names, function(x) x[2])

    index <- strsplit(sets, " in ")
    index <- sapply(index, function(x) x[1])
    index <- glue_collapse(index, ",")

    res_size <- sapply(sets_names, function(x) length(get(x, envir = envir)))
    res_size <- prod(res_size)

    sets_values <- lapply(sets_names, function(x) get(x, envir = envir))
    sets_sizes <- sapply(sets_values, length)

    curly_braces <-  rep("}", length(sets))
    curly_braces <- glue_collapse(curly_braces, "")

    if(grepl("=", eq)){
      new_eq <- glue('for({sets}){{')
      new_eq <- glue_collapse(new_eq, sep = " \n ")
      new_eq <- glue('{text_for} {eq} {curly_braces}',
                     text_for = new_eq,
                     curly_braces = curly_braces
      )

    } else {
      res <- array(0, dim = sets_sizes,
                 dimnames = sets_values)

      res_name <- paste0("res", paste0(sample(letters, 15, replace = TRUE),
                                       collapse = ""))
      assign(res_name, res, envir = envir)

      new_eq <- glue('for({sets}){{')
      new_eq <- glue_collapse(new_eq, sep = " \n ")
      new_eq <- glue('{text_for} {res_name}[{index}] <- {eq} {curly_braces}; {res_name}',
                     text_for = new_eq,
                     curly_braces = curly_braces,
                     res_name = res_name
      )
    }
  }
  return(parse(text = new_eq))
}

