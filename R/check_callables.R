#' Checks for calls and callables.
#'
#' @name check_callable
NULL
#' @rdname check_callable
#' @export
check_callable <- function(x,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (is_callable(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a {.cls callable}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_call <- function(x,
                       ...,
                       allow_null = FALSE,
                       arg = caller_arg(x),
                       call = caller_env()) {
  if (!missing(x)) {
    if (is_call(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused {.cls call}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_simple_call <- function(x,
                              ...,
                              allow_null = FALSE,
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (!missing(x)) {
    if (is_call_simple(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused {.cls call_simple}.",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_ns_call <- function(x,
                                  ...,
                                  allow_null = FALSE,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
  if (!missing(x)) {
    if (rlang::is_call(x, name = "::")) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused {.cls namespaced_call} (::).",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_ns_intetnal_call <- function(x,
                                       ...,
                                       allow_null = FALSE,
                                       arg = caller_arg(x),
                                       call = caller_env()) {
  if (!missing(x)) {
    if (rlang::is_call(x, name = ":::")) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a defused {.cls namespaced_call} (:::).",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_ns_symbol <- function(x,
                                    ...,
                                    allow_null = FALSE,
                                    arg = caller_arg(x),
                                    call = caller_env()) {
  if (!missing(x)) {
    if (rlang:::is_namespaced_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a {.cls namespaced_symbol}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_symbol <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    if (is_symbol(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a {.cls symbol}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_symbolic <- function(x,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (is_symbolic(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a {.cls symbolic}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_function <- function(x,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (is_function(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a {.cls function}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_closure <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_closure(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an R {.cls function}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_formula <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  if (!missing(x)) {
    if (is_formula(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a {.cls formula}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
#' @rdname check_callable
#' @export
check_environment <- function(x,
                              ...,
                              allow_null = FALSE,
                              arg = caller_arg(x),
                              call = caller_env()) {
  if (!missing(x)) {
    if (is_environment(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an {.cls environment}",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

