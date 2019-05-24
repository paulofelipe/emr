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

solve_emr_block <- function(model, scale_alpha = 1, triter = 100, trace = FALSE,
                            maxit = 1e5, tol = 1e-7){

  #method <- match.arg(method, c("BB", "nleqslv", "rootSolve"))

  sets <- model$sets
  params <- model$params
  variables <- model$variables
  equations <- model$equations

  env_model <- new.env()

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


  F <- list()
  F_old <- list()
  v_old <- list()

  for(i in names(v)){
    F[[i]] <- rep(0, length(c(v[[i]])))
    F_old[[i]] <- rep(0, length(c(v[[i]])))
    v_old[[i]] <- v[[i]]
  }

  eps <- 1e-10

  residuals <- rep(0, length(v))

  for(iter in 0:maxit){

    x <- lapply(defining_equations_p, eval, envir = env_model)

    for(i in 1:length(v)){

      name <- names(v)[i]
      F[[name]] <- unlist(lapply(mcc_equations_p[i], eval, envir = env_model))

      normF <- sqrt(sum(F[[name]]^2))

      if (iter == 0) {
        alpha <- min(1/normF, 1)
      }

      # if(iter > 0){
      #
      #   s <- v[[name]] - v_old[[name]]
      #   y <- F[[name]] - F_old[[name]]
      #   alpha <- sum(s*y)/sum(y*y)
      #
      #   if (is.nan(alpha))
      #     alpha <- eps
      #
      #   if ((abs(alpha) <= eps) | (abs(alpha) >= 1/eps))
      #     alpha <- if (normF > 1)
      #       1
      #   else if (normF >= 1e-05 & normF <= 1)
      #     1/normF
      #   else if (normF < 1e-05)
      #     1e+05
      # }

      if(iter > 0) alpha <- scale_alpha[i]

      res <- normF/sqrt(sum(c(v[[name]])^2))
      residuals[i] <- res

      if(iter%%triter == 0 & trace == TRUE)
        cat("Iteration: ", iter, " ",name,
            " ||F(x)||: ", res, "\n")
      v_old[[name]] <- v[[name]]
      v[[name]][] <- c(v[[name]]) + alpha * (-F[[name]])
      F_old[[name]] <- c(F[[name]])

    }

    for(name in names(v)){
      assign(name, v[[name]], envir = env_model)
    }

    max_F <- max(residuals)

    if(max_F < tol) break

  }


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

  return(list(params = results$params,
              variables = results$variables,
              updated_data = updated_data,
              variables_descriptions = variables_descriptions,
              params_descriptions = params_descriptions))

}
