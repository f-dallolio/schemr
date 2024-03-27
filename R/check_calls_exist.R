#' Check that call names exist
#'
#' @param .call a call.
#' @param .calls a list of call.
#' @param envir an environment.
#' @param verbose logical. Defaults to FALSE. If TRUE it prints the function in the form `ns::fn()`.
#'
#' @name check_calls_exist
NULL
#'
#' @rdname check_calls_exist
#' @export
check_call_exists <- function(.call, envir = rlang::caller_env(), verbose = FALSE){
  if(!is_call_simple(.call)){ cli::cli_abort("{.var call} must be a call.") }
  x <- split_call(.call)
  xnm <- x$name
  if( !is.null(x$ns) ){
    envir = asNamespace(x$ns)
  }
  out <- exists(x$name, envir = envir, mode = "function")
  if(out){
    ns_out <- ns_env_name(get(xnm, envir = envir))
  } else {
    cli::cli_abort("Cannot find the function {.var {xnm}}.")
  }
  if(verbose){
    cli::cli_text("{.fn {paste0(ns_env_name(get(xnm)), '::', xnm)}}")
  }
  invisible(.call)
}

#' @rdname check_calls_exist
#' @export
check_calls_exist <- function(.calls, envir = rlang::caller_env(), verbose = FALSE){
  lapply(.calls, check_call_exists, envir = envir, verbose = verbose)
  invisible(.calls)
}

#' @rdname check_calls_exist
#' @export
check_arrow_fn <- function(x){
  UseMethod("check_arrow_fn")
}
#' @rdname check_calls_exist
#' @export
check_arrow_fn.default <- function(x){
  id_ok <- vapply(calls_nms(x), exists, logical(1), envir = asNamespace("arrow"))
  id_not_ok <- which(!id_ok)
  if( !is_empty(id_not_ok) ){
    cli::cli_abort("{vec_size(id_not_ok)} element{?s} not in {.cls namespace:arrow}:
                   {str_oxford(id_not_ok)}")
  }
  invisible(x)
}
#' @rdname check_calls_exist
#' @export
check_arrow_fn.vcall <- function(x){
  x <- field(x, "call")
  id_ok <- vapply(calls_nms(x), exists, logical(1), envir = asNamespace("arrow"))
  id_not_ok <- which(!id_ok)
  if( !is_empty(id_not_ok) ){
    cli::cli_abort(c("{vec_size(id_not_ok)} element{?s} not in {.cls namespace:arrow}.",
                     "i" = "{str_oxford(id_not_ok)}"))
  }
  invisible(x)
}
