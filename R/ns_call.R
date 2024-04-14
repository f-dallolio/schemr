#' Create Namespaced Call
#'
#'@name namespaced-call
NULL
#'
#' @rdname namespaced-call
#' @export
ns_call <- function(.ns = NULL, .fn = NULL, ...,
                    .private = FALSE, .strict = FALSE){
  check_dots_empty()
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
#'
#' @export
namespaced_call <- function(.ns = NULL, .fn = NULL, ...,
                    .private = FALSE, .strict = FALSE){
  ns_call <- function(.ns = .ns, .fn = .fn, ...,
                      .private = .private, .strict = .strict)
}
