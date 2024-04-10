#' Find Namespaces
#'
#' @param x .
#' @param full_out .
#'
#'
#' @name find_ns
NULL
#'
#' @rdname find_ns
#' @export
.auto_ns <- function(x, full_out = TRUE){
  check_call(x)
  ns <- call_ns(x)
  if(is.null(ns)){
    fn <- try(get(call_name(x)))
    if(is_function(fn)){
      ns <- ns_env_name(fn)
    } else {
      ns <- NULL
    }
  }
  if(full_out){
    call2(call_name(x), !!!call_args(x), .ns = ns)
  } else {
    ns
  }
}
#' @rdname find_ns
#' @export
auto_ns <- function(..., ns_only = FALSE){
  sapply(list2(...), .auto_ns)
}

add_ns <- function(x, ns){

}
