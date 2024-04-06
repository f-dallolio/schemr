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
obj_as_call <- function(x, ..., .find_ns = TRUE) {
  x <- enexpr(x)
  if( is_string(x) ){
    x_str <- str2lang(x)
    if( is_call(x_str, c("::", ":::")) ){
      fn <- quo_text(x_str[[3]])
      ns <- quo_text(x_str[[2]])
      x_str <- call2(fn, .ns = ns)
    }
    if( !rlang::is_call(x_str) ){
      out <- rlang::call2(x_str)
    } else {
      out <- x_str
    }
  } else {
    if( is_call(x, c("::", ":::")) ){
      fn <- quo_text(x[[3]])
      ns <- quo_text(x[[2]])
      x <- call2(fn, .ns = ns)
    }
    if( is_call_simple(x)){
      out <- x
    } else {
      out <- call2(expr_text(x))
    }
  }
  if(.find_ns){
    null_ns <- is.null(call_ns(out))
    .auto_ns(out)
  } else {
    out
  }
}

#'
#' @rdname objs_to_calls
#' @export
objs_as_call <- function(..., .find_ns = TRUE, .named = FALSE){
  x <- enexprs(...)
  nms <- names(x)
  if(all(nms == "")){
    names(x) <- NULL
  }
  out <- lapply(x, obj_as_call, .find_ns = .find_ns)
  if(.named){
    nms_id <- which(nms == "")
    nms[nms_id] <- vapply(out[nms_id], call_name, character(1))
  }
  setNames(out, nms)
}
