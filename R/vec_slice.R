#' Vector Slicing
#'
#' @param x .
#' @param i .
#' @param n .
#' @param ... .
#' @param error_call .
#'
#' @name vec_slice
NULL
#'
#' @rdname vec_slice
#' @export
vec_slice <- function(x, i, ..., error_call = rlang::current_env()){
  vctrs::vec_slice(x = x, i = i, error_call = error_call)
}
#' @rdname vec_slice
#' @export
vec_head <- function(x, n){
  len <- length(x)
  pos <- seq_len(n)
  x[pos]
}
#' @rdname vec_slice
#' @export
vec_tail <- function(x, n){
  len <- length(x)
  offset <- len - n
  pos <- seq_len(n) + offset
  x[pos]
}
