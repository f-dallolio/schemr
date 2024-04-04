#' Get/set quosures expressions and environments
#'
#' @param x quosures.
#' @param exprs list of expressions.
#' @param env an environment.
#'
#' @name quos_helpers
NULL
#'
#' @rdname name quos_helpers
#' @export
quos_get_expr <- function(x){
  assertthat::assert_that(is_quosures(x))
  lapply(x, rlang::quo_get_expr)
}
#'
#' @rdname quos_helpers
#' @export
quos_set_expr <- function(x, exprs){
  assertthat::assert_that(is_quosures(x))
  mapply(rlang::quo_set_expr, x, exprs)
}
#'
#' @rdname quos_helpers
#' @export
quos_get_env <- function(x){
  assertthat::assert_that(is_quosures(x))
  lapply(x, rlang::quo_get_env)
}
#'
#' @rdname quos_helpers
#' @export
quos_set_env <- function(x, env){
  assertthat::assert_that(is_quosures(x))
  mapply(rlang::quo_set_env, x, env)
}




