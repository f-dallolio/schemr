#' Test fr callables
#'
#' @name is_callable
NULL
#'
#' @rdname is_callable
#' @export
is_callable <- function(x){
  rlang:::is_callable(x)
}
#' @rdname is_callable
#' @export
is_call <- function(x, name = NULL, n = NULL, ns = NULL){
  rlang:::is_call(x = x, name = name, n = n, ns = ns)
}
#' @rdname is_callable
#' @export
is_simple_call <- function(x, ns = NULL){
  rlang:::is_call_simple(x, ns = ns)
}
#' @rdname is_callable
#' @export
is_ns_call <- function(x, name = "::", n = NULL, ns = NULL){
  rlang:::is_call(x = x, name = name, n = n, ns = ns)
}
#' @rdname is_callable
#' @export
is_ns_internal_call <- function(x, name = ":::", n = NULL, ns = NULL){
  rlang:::is_call(x = x, name = ":::", n = n, ns = ns)
}
#' @rdname is_callable
#' @export
is_ns_symbol <- function(x, ns = NULL, private = NULL){
  rlang:::is_namespaced_symbol(x, ns = ns, private = private)
}
#' @rdname is_callable
#' @export
is_symbol <- function(x, name = NULL){
  rlang:::is_symbol(x = x, name = name)
}
#' @rdname is_callable
#' @export
is_symbolic <- function(x){
  rlang:::is_symbolic(x = x)
}
#' @rdname is_callable
#' @export
is_function <- function(x){
  rlang:::is_function(x = x)
}
#' @rdname is_callable
#' @export
is_closure <- function(x){
  rlang:::is_closure(x = x)
}
#' @rdname is_callable
#' @export
is_formula <- function(x, scoped = NULL, lhs = NULL){
  rlang:::is_formula(x = x, scoped = scoped, lhs = lhs)
}
#' @rdname is_callable
#' @export
is_environment <- function(x){
  rlang:::is_environment(x = x)
}
