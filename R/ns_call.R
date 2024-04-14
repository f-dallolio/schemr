#' Create Namespaced Call
#'
#' @export
ns_call <- function(.ns = NULL, .fn = NULL, ...,
                    .private = FALSE, .strict = FALSE){
  lhs <- encall(enexpr(.ns))
  rhs <- encall(enexpr(.fn))
  if(is.null(lhs)) return(rhs)
  if(.private){
    .op = ":::"
  } else {
    .op = "::"
  }
  call2(.op, lhs, rhs)
}
