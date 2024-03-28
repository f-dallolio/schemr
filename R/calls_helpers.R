#' Get names/args/ns (and set ns) of calls lists.
#'
#' @param x a call.
#' @param calls a list of calls.
#'
#' @name calls_helpers
NULL
#'
#' @rdname calls_helpers
#' @export
call_name2 <- function(calls){
  if(!is.list(calls)){
    calls = list(calls)
  }
  sapply(calls, rlang::call_name)
}
#' @rdname calls_helpers
#' @export
calls_nms <- function(calls) {call_name2(calls)}

#' @rdname calls_helpers
#' @export
call_args2 <- function(calls){
  if(!is.list(calls)){
    calls = list(calls)
  }
  vapply(calls, rlang::call_args, list())
}
#' @rdname calls_helpers
#' @export
calls_args <- function(calls) {call_args2(calls)}

#' @rdname calls_helpers
#' @export
call_ns2 <- function(calls){
  if(!is.list(calls)){
    calls = list(calls)
  }
  sapply(calls, rlang::call_ns)
}
#' @rdname calls_helpers
#' @export
calls_ns <- function(calls) {call_ns2(calls)}

#' @rdname calls_helpers
#' @export
call_match2 <- function(calls, env = parent.env(), ...){
  if(!is.list(calls)){
    calls = list(calls)
  }
  check_calls_name_exist(calls, envir = env)
  fn_nms <- call_name2(calls)
  fns <- lapply(fn_nms, get, envir = env)
  mapply(rlang::call_match, call = calls, fn = fns, defaults = TRUE)
}
#' @rdname calls_helpers
#' @export
calls_match <- function(calls, env = parent.env(), ...) {call_match2(calls, env, ...)}

#' @rdname calls_helpers
#' @export
call_set_ns <- function(x, ns){
  assertthat::assert_that(rlang::is_call_simple(x))
  rlang::call2(rlang::call_name(x), !!!rlang::call_args(x), .ns = ns)
}

#' @rdname calls_helpers
#' @export
calls_set_ns <- function(calls, ns){
  lapply(x, call_set_ns)
}

#' @rdname calls_helpers
#' @export
call_text <- function(x, unname = TRUE, shrink = TRUE){
    UseMethod("call_text")
}

#' @rdname calls_helpers
#' @export
call_text.default <- function(x, unname = TRUE, shrink = TRUE){
  assertthat::assert_that(is_call(x))
  nm <- rlang::call_name(x)
  arg <- lapply(rlang::call_args(x),
                function(x) if(is.numeric(x)) {
                  as.numeric(x)
                } else {
                  sprintf("%s", x)
                })
  if(unname){
    arg <- unname(arg)
  }
  out <- rlang::expr_text(rlang::call2(nm, !!!arg))
  if(shrink){
    out <- gsub(" ", "", out)
  }
  gsub('\"',"\'",out)
}

#' @rdname calls_helpers
#' @export
call_text.vcall <- function(x, unname = TRUE, shrink = TRUE){
  x <- field(x, "call")
  out <- vapply(x, call_text.default, character(1), unname = unname, shrink = shrink)
  if( all(names(x) == "")) {
    names(out) <- NULL
  }
  out
}
