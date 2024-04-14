#' Create Delimited Call
#'
#' @export
delim_call <- function(lhs, rhs, ..., .op = "::"){
  out <- pairlist2(ensym(.op), ensym(lhs), ensym(rhs))
  as.call(out)
}
