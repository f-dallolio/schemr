#' Split calls into call name, args, and namespace
#'
#' @param x a call.
#' @param ... calls.
#'
#' @name split_call
NULL
#'
#' @rdname split_call
#' @export
split_call <- function(x){
  list(name = rlang::call_name(x),
       args = rlang::call_args(x),
       ns = rlang::call_ns(x))
}
#' @rdname split_call
#' @export
split_calls <- function(...){
  x <- list2(...)
  check_objs_are_call(!!!x)
  nms <- names(x)
  setNames(lapply(x, split_call), nms)
}
