#' Check if argument is numeric
#'
#' @param x a vector.
#'
#' @return `x` invisibly if is numeric, an error otherwise.
#' @export
#'
#' @examples
#' check_numeric(letters)
#' check_numeric(1:10)
check_numeric <- function(x){
  if (!is.numeric(x)) {
    cli::cli_abort("{.var x} must be a numeric vector")
  }
  invisible(x)
}
