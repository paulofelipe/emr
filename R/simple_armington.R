#' Create a simple armington model
#'
#' @description
#' The model is writen using the "exact hat algebra".
#'
#' For example, \eqn{\hat{q}_s = q_s'/q_s}, where q_s' is the value in the new equilibrium and q_s is the value in the old equilibrium.
#'
#' @param v0 a vector with the initial demand values.
#' @param eps a vector with supply elasticities.
#' @param eta the total demand price elasticity.
#' @param t a vector with the tariff initial values
#' @param sigma the armington elasticity.
#' @param regions a vector with the regions names.
#'
#' @return Returns a list with the model components.
#' @export
#' @examples
#'
#'library(emr)
#'
#'sa_model <- simple_armington(
#'  v0 = c(60, 30, 10),
#'  eps = c(1, 10, 10),
#'  t = c(0, 0, 0),
#'  eta = -1,
#'  sigma = 4,
#'  regions = c("dom", "sub", "nsub")
#')
#'

simple_armington <- function(
  v0, # initial values
  eps, # supply elasticities
  eta, # total demand elasticity
  t, # tariffs
  sigma, # armington elastiticy
  regions # regions names vector
){
  # Sets --------------------------------------------------------------------

  REG <- regions

  sets <- list(
    REG = REG
  )

  # Parameters --------------------------------------------------------------

  params <- list()

  params[['v0']] <- create_param(
    value = v0,
    indexes = sets[c('REG')],
    desc = 'Initial demand by region'
  )

  params[['eps']] <- create_param(
    value = eps,
    indexes = sets['REG'],
    desc = 'Supply elasticity'
  )

  params[['eta']] <- create_param(
    value = eta,
    indexes = list(eta = "eta"),
    desc = "Total demand price elasticity"
  )

  params[["sigma"]] <- create_param(
    value = sigma,
    indexes = list(sigma = "sigma"),
    desc = "Elasticity of substitution"
  )

  params[["tau"]] <- create_param(
    value = rep(1, length(REG)),
    indexes = sets['REG'],
    desc = "Change in the tariff power"
  )

  params[["pi"]] <- create_param(
    value = (params$v0$value * (1+t))/sum(params$v0$value * (1+t)),
    indexes = sets['REG'],
    desc = "shares consumption"
  )

  params[["k_s"]] <- create_param(
    value = 1,
    indexes = sets['REG'],
    desc = "shift in supply"
  )

  params[["alpha"]] <- create_param(
    value = 1,
    indexes = sets['REG'],
    desc = "shift in demand preferences"
  )

  params[["k_d"]] <- create_param(
    value = 1,
    indexes = list(k_d = "k_d"),
    desc = "shift in total demand"
  )

  # Variables -------------------------------------------------

  variables <- list()

  variables[['p']] <- create_variable(
    value = 1,
    indexes = sets['REG'],
    type = "undefined",
    desc = "Price by region (including tariff)"
  )

  variables[['q_d']] <- create_variable(
    value = 1,
    indexes = sets['REG'],
    type = "defined",
    desc = "Demand by region"
  )

  variables[['q_s']] <- create_variable(
    value = 1,
    indexes = sets['REG'],
    type = "defined",
    desc = "Supply by region"
  )

  variables[['P']] <- create_variable(
    value = 1,
    indexes = list(P = "P"),
    type = "defined",
    desc = "Price index"
  )

  variables[['Q']] <- create_variable(
    value = 1,
    indexes = list(Q = "Q"),
    type = "defined",
    desc = "Total demand"
  )

  # Equations ---------------------------------------------------------------

  equations <- list()

  equations[["e_P"]] <- create_equation(
    "P = sum(pi * (alpha * p)^(1-sigma))^(1/(1-sigma))",
    type = "defining",
    desc = "Price index"
  )

  equations[["e_Q"]] <- create_equation(
    "Q = k_d * P^eta",
    type = "defining",
    desc = "Total demand"
  )

  equations[['e_q_d']] <- create_equation(
    "q_d[r] = alpha[r]^(1-sigma) *( p[r]/P)^(-sigma) * Q",
    indexes = 'r in REG',
    type = "defining",
    desc = "Demand for each region product"
  )

  equations[['e_q_s']] <- create_equation(
    "q_s[r] = k_s[r] * (p[r]/tau[r])^eps[r]",
    indexes = "r in REG",
    type = "defining",
    desc = "Supply by region"
  )

  equations[['e_p']] <- create_equation(
    "q_s[r] - q_d[r]",
    indexes = "r in REG",
    type = "mcc",
    desc = "market clearing condition for each product r"
  )


  # Update data -------------------------------------------------------------

  update_equations <- list()

  update_equations[['v0']] <- create_equation(
    'v0[r] = v0[r] * p[r] * q_s[r]',
    indexes = 'r in REG',
    desc = "Value of demand by source region"
  )

  list(
    sets = sets,
    params = params,
    variables = variables,
    equations = equations,
    update_equations = update_equations
  )
}
