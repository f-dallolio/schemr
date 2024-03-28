#' Names helpers
#'
#' @param x a vector or data frame.
#' @param .fn a function. Same rules as for `purrr::map`.
#' @param ... arguments passed to `.fn`.
#' @param .prefix defaults to "x". Prefix to use in case `x` has no names.
#' @param .pad defaults to "_". Padding to use in case `x` has no names .
#'
#' @examples
#' iris_nonames <- unname(iris)
#' names(re_name(iris))
#' names(re_name2(iris_nonames))
#'
#' @name names_helpers
NULL
#'
#' @rdname names_helpers
#' @export
re_name <- function(x, .fn = snakecase::to_snake_case, ...){
  if(rlang::is_named(x)){
    fn <- rlang::as_function(.fn, ...)
    names(x) <- fn(names(x))
  }
  x
}
#' @rdname names_helpers
#' @export
re_name2 <- function(x, .fn = ~ .x, ..., .prefix = "x", .pad = "_"){
  # if(is.null(.fn)){ fn <- rlang::as_function(~ .x) }
  if (!rlang::is_named(x)) {
    nms <- seq_len(length(x))
    names(x) <- paste0(.prefix, str_numpad(nms, pad = .pad))
  }
  re_name(x, .fn = .fn, ...)
}
