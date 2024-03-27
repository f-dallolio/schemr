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

#'
#' @rdname objs_to_calls
#' @export
obj_as_call <- function(x = NULL) {
  x <- rlang::enexpr(x)
  if( is.null({{ x }}) ) { return(NULL) }
  if( rlang::is_string({{ x }}) ){
    x_str <- str2lang({{ x }})
    if( !rlang::is_call(x_str) ){
      rlang::call2(x)
    } else {
      x_str
    }
  } else {
    if( !rlang::is_call({{ x }}) ) {
      as.call(list({{ x }}))
    } else {
      x
    }
  }
}
#'
#' @rdname objs_to_calls
#' @export
objs_as_call <- function(...){new
  exprs <- rlang::enexprs(...)
  nms <- names(exprs)
  setNames(lapply(exprs, obj_as_call), nms)
}

