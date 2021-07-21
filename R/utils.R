#' Create a safe envinroment
safe_env <- function() {
  .safe_env <- new.env(parent = emptyenv())

  safe_f <- c(
    methods::getGroupMembers("Math"),
    methods::getGroupMembers("Arith"),
    methods::getGroupMembers("Compare"),
    "<-", "{", "(", "min", "max", "pmin", "pmax", "mean", "sum", "prod", "for",
    "vector", "seq", ":", "seq.default", "seq.int", "[", "[[", "[<-", "$", "$<-",
    "c", "::", "~", "function", "=", "if", "%in%"
  )

  for (f in safe_f) {
    .safe_env[[f]] <- get(f, "package:base")
  }
  .safe_env[["concatenate_function"]] <- get("c", "package:base")
  .safe_env
}