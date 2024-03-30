#' Vector checks
#'
#' @param x input object.
#'
#' @return an error if the condition is not satisfied. The function returns `x` invisibly otherwise.
#'
#' @name checks
NULL
#'
#' @examples
#' input <- 2L
#' check_scalar_integer(input)
#' check_character(input)
#'
#' @rdname checks
#' @export
check_call_simple <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_call_simple(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls call} (simple), not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_call <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_call(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls call}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_function <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_function(x)) {
    cli::cli_abort("{.arg {arg}} must be a {.cls function}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_lambda_function <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_lambda(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls rlang_lambda_function}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#'
#' @rdname checks
#' @export
check_scalar_character <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_scalar_character(x) ) {
    cli::cli_abort("{.arg {arg}} must be a scalar {.cls character}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_scalar_double <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_scalar_double(x) ) {
    cli::cli_abort("{.arg {arg}} must be a scalar {.cls double}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_scalar_integer <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_scalar_integer(x) ) {
    cli::cli_abort("{.arg {arg}} must be a scalar {.cls integer}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_scalar_list <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_scalar_list(x) ) {
    cli::cli_abort("{.arg {arg}} must be a scalar {.cls list}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_scalar_logical <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_scalar_logical(x) ) {
    cli::cli_abort("{.arg {arg}} must be a scalar {.cls logical}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_scalar_vector <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_scalar_vector(x) ) {
    cli::cli_abort("{.arg {arg}} must be a scalar {.cls vector}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#'
#' @rdname checks
#' @export
check_bool <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_bool(x) ) {
    cli::cli_abort("{.arg {arg}} must be {.cls logical} of size 1, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_string <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_string(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls string}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#'
#' @rdname checks
#' @export
check_character <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_character(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls character}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_double <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_double(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls double}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_integer <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_integer(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls integer}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_list <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_list(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls list}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_logical <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_logical(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls logical}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}
#'
#' @rdname checks
#' @export
check_vector <- function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env()){
  if( !rlang::is_vector(x) ) {
    cli::cli_abort("{.arg {arg}} must be a {.cls vector}, not a {.cls {class(x)}}.")
  }
  invisible(x)
}

