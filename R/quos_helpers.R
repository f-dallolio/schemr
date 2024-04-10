#' Get/set quosures expressions and environments
#'
#' @param x quosures.
#' @param exprs list of expressions.
#' @param env an environment.
#'
#' @name quos_helpers
NULL
#'
#' @rdname quos_helpers
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
#'
#' @rdname quos_helpers
#' @export
expr_to_string <- function(x){
  # x <- enexpr(x)
  if(is_call(x)){
    x <- eval(x)
  }
  if(is_symbol(x)){
    expr_text(x)
  } else {
    x
  }
}
#'
#' @rdname quos_helpers
#' @export
quo_to_string <- function(x){
  check_quosure(x)
  if(quo_is_call(x)){
    x <- eval_tidy(x)
  }
  if(quo_is_symbol(x)){
    quo_text(x)
  } else {
    quo_get_expr(x)
  }
}

setNames(sapply(quos("a", "b") , quo_to_string), sapply(quos("a", "b") , quo_to_string))
