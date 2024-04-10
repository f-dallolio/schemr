#' Inputs as calls
#'
#' @param x expression or character vector to test as call or transform to call.
#'
#' @name objs_to_calls
NULL
#'
#' @rdname objs_to_calls
#' @export
obj_is_call <- function(x){
  x <- rlang::enexpr(x)
  if( rlang::is_string(x) ) {
    rlang::is_call_simple(str2lang(x))
  } else {
    rlang::is_call_simple(x)
  }
}
#'
#' @rdname objs_to_calls
#' @export
check_obj_is_call <- function(x){
  x <- enexpr(x)
  if(obj_is_call(!!x)){
    invisible(x)
  } else {
    cli::cli_abort("{.var x} must be a call.")
  }
}
#'
#' @rdname objs_to_calls
#' @export
objs_are_call <- function(..., all = TRUE){
  exprs <- rlang::enexprs(...)
  out <- vapply(exprs, obj_is_call, logical(1))
  if(all){
    all(out)
  } else {
    out
  }
}
#'
#' @rdname objs_to_calls
#' @export
check_objs_are_call <- function(...){
  exprs <- exprs(...)
  out <- objs_are_call(!!!exprs, all = FALSE)
  not_call <- which(!out)
  if( !rlang::is_empty(not_call) ){
    cli::cli_abort("Element{?s} {not_call} {? is/are} not call{?s}")
  }
  invisible(exprs)
}


.obj2call <- function(x){
  # x <- enquo(x)
  x_expr <- enexpr(x)
  if(is.character(x_expr)){
    x_expr <- str2lang(x_expr)
  }
  if(rlang::is_call(x_expr)){
    if(rlang::is_call(x_expr, name = c("::", ":::"))){
      cll_nm <- as_string(x_expr[[3]])
      cll_ns <- as_string(x_expr[[2]])
      return(call2(cll_nm, .ns = cll_ns))
    } else if (is_call_simple(x_expr)){
      return(x_expr)
    } else {
      cli::cli_abort("Cannot create a call out of {.arg x}")
    }
  } else {
    cll <- as.call(list(x_expr))
    return(cll)
  }
}
#'
#' @rdname objs_to_calls
#' @export
obj_as_call <- function(x){
  .obj2call(!!enexpr(x))
}
#'
#' @rdname objs_to_calls
#' @export
objs_as_call <- function(..., .named = FALSE){#, .auto_ns = FALSE){
  x <- enexprs(..., .named = .named)
  out <- lapply(x, obj_as_call)
  out
}
