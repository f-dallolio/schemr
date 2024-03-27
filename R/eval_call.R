#' TEvaluate calls
#'
#' @param x a list of calls.
#' @param ...
#'
#' @name eval_call
NULL
#'
#' @export
#' @rdname eval_call
eval_call <- function(x, env = parent.env()){
  UseMethod("eval_call")
}
#' @export
#' @rdname eval_call
eval_call.default <- function(x, env = parent.env()){
    cli::cli_abort("Cannot find {.fn eval_call} for class {.cls {class(x)[[1]]}}")
}
#' @export
#' @rdname eval_call
eval_call.vcall <- function(x){
  lapply(field(x, "call"), rlang::eval_tidy, env = attr(x, "env"))
}
