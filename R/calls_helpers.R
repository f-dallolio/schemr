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
  out <- sapply(calls, rlang::call_name)
  if(all(names(out) == "")){
    names(out) <- NULL
  }
  out
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
  out <- vapply(calls, rlang::call_args, list())
  if(all(names(out) == "")){
    names(out) <- NULL
  }
  out
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
  out <- sapply(calls, rlang::call_ns)
  if(all(names(out) == "")){
    names(out) <- NULL
  }
  out
}
#' @rdname calls_helpers
#' @export
calls_ns <- function(calls) {call_ns2(calls)}

#' @rdname calls_helpers
#' @export
call_match2 <- function(calls, env = rlang::caller_env(), ...){
  if(!is.list(calls)){
    calls = list(calls)
  }
  check_calls_exist(calls, envir = env)
  fn_nms <- call_name2(calls)
  fns <- lapply(fn_nms, get, envir = env)
  mapply(rlang::call_match, call = calls, fn = fns, dots_expand = TRUE, defaults = TRUE)
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
call_self_match <- function(call, ...,
                            defaults = !is.null(call_ns(call)),
                            dots_env = NULL, dots_expand = TRUE){
  check_call_simple(call)
  check_dots_empty0(...)
  nm <- call_name(call)
  ns <- call_ns(call)
  if(is.null(ns)){
    fn_ok <- exists(nm, mode = "function")
  } else {
    fn_ok <- exists(nm, envir = asNamespace(ns), mode = "function")
  }

  if(fn_ok){
    fn <- get(nm, mode = "function")
  } else {
    c_arg <- caller_arg(call)
    cli::cli_abort("Cannot find the function {.arg {c_arg}}")
  }
  rlang::call_match(call = call, fn = fn,
                    defaults = defaults,
                    dots_env = dots_env,
                    dots_expand = dots_expand)
}

#' @rdname calls_helpers
#' @export
call_set_name <- function(x, value){
  check_call(x)
  value <- enquo(value)
  if(quo_is_call(value)){
    value <- as_quosure(eval_tidy(value))
  }
  if (quo_is_symbolic(value)){
    val <- quo_text(value)
  } else {
    val <- quo_get_expr(value)
  }
  call2(val, splice(call_args(x)), .ns = call_ns(x))
}
#'
#' @rdname calls_helpers
#' @export
call_set_ns <- function(x, value){
  check_call(x)
  value <- enquo(value)
  if(quo_is_call(value)){
    value <- as_quosure(eval_tidy(value))
  }
  if (quo_is_symbolic(value)){
    val <- quo_text(value)
  } else {
    val <- quo_get_expr(value)
  }
  call2(call_name(x), splice(call_args(x)), .ns = val)
}
#'
#' @rdname calls_helpers
#' @export
call_no_ns <- function(x){
  check_call_simple(x)
  is.null(call_ns(x))
}
#'
#' @rdname calls_helpers
#' @export
call_has_ns <- function(x){
  !call_no_ns(x)
}
#'
call_expand_scalar <- function(x){
  check_call_simple(x)
  fn <- call_name(x)
  args <- call_args(x)
  ns <- call_ns(x)
  foo <- get0(fn, mode = "function", ifnotfound = NULL)
  if(call_has_ns(x)){
    foo <- get0(fn, envir =  asNamespace(ns), mode = "function", ifnotfound = NULL)
  }
  if(is.null(ns) && is_function(foo)){
    ns <- ns_env_name(foo)
  }
  call_new <- call2(fn, splice(args), .ns = ns)
  if(is_function(foo)){
    call_match(call_new, foo, defaults = TRUE, dots_expand = TRUE)
  } else {
    call_new
  }
}
#'
#' @rdname calls_helpers
#' @export
call_expand <- function(...){
  x <- list2(...)
  lapply(x, call_expand_scalar)
}
#'
#' @rdname calls_helpers
#' @export
call_base <- function(name, ns = NULL){
  .fn <- expr_to_string(enexpr(name))
  .ns <- expr_to_string(enexpr(ns))
  call2(.fn = .fn, .ns = .ns)
}
