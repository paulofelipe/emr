#' Solve the model
#'
#' @param model a named list with params, variables and equations.
#' @param start a vector with start values for undefined variables. If NULL, the currente value will be used.
#' @param method the solver to be used: BB or nleqslv
#' @param ... the other arguments to be passed into the solver control argument.
#'
#'
#' @return Returns data and information about the model and its solution
#' @export
#' @examples
#'
#'library(emr)
#'
#'sa_model <- simple_armington(
#' v0 = c(60, 30, 10),
#' eps = c(1, 10, 10),
#' eta = -1,
#' sigma = 4,
#' regions = c("dom", "sub", "nsub")
#')
#'
#'# tau is the power tariff change
#'# Increase the tariff in 10 percent for the sub region
#'sa_model$params$tau$value['sub'] <- 1.1
#'
#'sol <- solve_emr(sa_model)
#'
#'sol$variables

solve_emr <- function(model, start = NULL,
                      method = "BB", ...){

  method <- match.arg(method, c("BB", "nleqslv", "rootSolve", "dfsane"))

  sets <- model$sets
  params <- model$params
  variables <- model$variables
  equations <- model$equations

  env_model <- safe_env()

  undefined_variables <- variables[
    sapply(variables, function(x) x$type == "undefined")
    ]
  v <- lapply(undefined_variables, function(x) x[["value"]])
  idx <- sapply(v, function(x) length(c(x)))

  defined_variables <- variables[
    sapply(variables, function(x) x$type == "defined")
    ]


  mcc_equations <- equations[
    sapply(equations, function(x) x$type == "mcc")
    ]

  defining_equations <- equations[
    sapply(equations, function(x) x$type == "defining")
    ]


  for(i in names(params))
    assign(i, params[[i]][['value']], envir = env_model)

  for(i in names(variables))
    assign(i, variables[[i]][['value']], envir = env_model)

  for(i in names(sets))
    assign(i, sets[[i]], envir = env_model)


  defining_equations_p <- lapply(defining_equations, parse_equation,
                                 envir = env_model)
  mcc_equations_p <- lapply(mcc_equations, parse_equation,
                            envir = env_model)


  model_fn <- function(start, params, defining_equations,
                       mcc_equations){

    idx_0 <- 1
    for(i in 1:length(v)){
      v[[i]][] <- start[idx_0:(idx_0 + idx[i] - 1)]
      idx_0 <- idx_0 + idx[i]
    }

    for(i in names(v))
      assign(i, v[[i]], envir = env_model)

    x <- lapply(defining_equations_p, eval, envir = env_model)

    r <- lapply(mcc_equations_p, eval, envir = env_model)
    r <- unlist(r)
    r

  }

  if(is.null(start)){
    start <- lapply(undefined_variables, function(x) x[[1]])
    start <- unlist(start)
  }

  names_var <- stringr::str_extract(names(start), "[^\\.]+$")
  names(start) <- names_var

  if(method == "BB"){
    sol <- BB::BBsolve(start, model_fn,
                       params = params,
                       defining_equations = defining_equations_p,
                       mcc_equations = mcc_equations_p,
                       quiet = TRUE,
                       method = c(3,2,1),
                       control = list(...)
    )

    start <- sol$par
    names(start) <- names_var
  }

  if(method == "dfsane"){
    sol <- BB::dfsane(start, model_fn,
                       params = params,
                       defining_equations = defining_equations_p,
                       mcc_equations = mcc_equations_p,
                       quiet = TRUE,
                       control = list(...)
    )

    start <- sol$par
    names(start) <- names_var
  }

  if(method == "nleqslv"){
    sol <- nleqslv::nleqslv(start, model_fn,
                            params = params,
                            defining_equations = defining_equations_p,
                            mcc_equations = mcc_equations_p,
                            control = list(...))

    start <- sol$x
  }

  if(method == "rootSolve"){
    sol <- rootSolve::multiroot(model_fn, start,
                                params = params,
                                verbose = TRUE,
                                defining_equations = defining_equations_p,
                                mcc_equations = mcc_equations_p)
    start <- sol$root
  }

  x <- lapply(defining_equations_p, eval, envir = env_model)

  #r <- lapply(mcc_equations_p, eval, envir = env_model)

  idx_0 <- 1
  for(i in 1:length(v)){
    v[[i]][] <- start[idx_0:(idx_0 + idx[i] - 1)]
    idx_0 <- idx_0 + idx[i]
  }

  for(i in names(v))
    assign(i, v[[i]], envir = env_model)

  for(i in names(defined_variables)){
    if(is.vector(defined_variables[[i]][["value"]])){
      names(env_model[[i]]) <- names(defined_variables[[i]][["value"]])
    }
  }

  for(i in names(undefined_variables)){
    if(is.vector(undefined_variables[[i]][["value"]])){
      names(env_model[[i]]) <- names(undefined_variables[[i]][["value"]])
    }
  }

  results_tmp <- as.list.environment(env_model)
  results_tmp <- results_tmp[c(names(v),
                               names(defined_variables),
                               names(params))]

  results <- list()
  results[["params"]] <- results_tmp[names(params)]
  results[["variables"]] <- results_tmp[names(variables)]

  update_equations <- lapply(model$update_equations, parse_equation,
                             envir = env_model)

  x <- lapply(update_equations, eval, envir = env_model)

  updated_data <- list()
  for(i in names(update_equations)){
    updated_data[[i]] <- env_model[[i]]
  }

  variables_descriptions <- data.frame(
    variable = names(variables),
    description = sapply(variables, function(x) x$desc),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  params_descriptions <- data.frame(
    params = names(params),
    description = sapply(params, function(x) x$desc)
  )

  return(list(sol = sol,
              params = results$params,
              variables = results$variables,
              updated_data = updated_data,
              variables_descriptions = variables_descriptions,
              params_descriptions = params_descriptions))

}
