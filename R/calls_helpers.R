#' Call Helpers
#'
#' @param x a call.
#' @param calls a list of calls.
#'
#' @name calls_helpers
NULL
#'
#' @rdname calls_helpers
#' @export
encall <- function(x) {
  x <- get_expr(x)
  if (is_string(x)) {
    str2lang(x)
  } else {
    x
  }
}
#'
#' @rdname calls_helpers
#' @export
call_add_ns <- function(.call, .ns){
  check_call_simple(.call)
  ns <- encall(enexpr(.ns))
  fn <- sym(call_name(.call))
  args <- call_args(.call)
  call2(as_string(fn), splice(args), .ns = as_string(ns))
}
