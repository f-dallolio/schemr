#' Create Named (standard) Call
#'
#'@name named-call
NULL
#'
#' @rdname named-call
#' @export
named_call <- function(.fn, ..., .args = NULL, .ns = NULL, .private = FALSE){
  car <- ns_call(.ns = {{ .ns }},
                 .fn = {{ .fn }},
                 .private = .private,
                 .strict = TRUE)
  if(!is.null(.args)){
    check_dots_empty()
  } else {
    stopifnot(is.null(.args))
    .args <- enexprs(...)
  }
  new_call(car, as.pairlist(.args))
}
#'
#' @rdname named-call
#' @export
named_call0 <- function(.fn, .args = NULL){
  check_callable(.fn)
  new_call(.fn, as.pairlist(.args))
}
#'
#' @rdname named-call
#' @export
as_named_call <- function(x){
  out <- encall(enexpr(x))
  # if(is_string(out)) out <- str2lang(out)
  if(is_symbol(out) || is_call(out, name = c("::", ":::"))){
    new_call(out)
  } else {
    out
  }
}
#'
#' @rdname named-call
#' @export
as_named_call_list <- function(..., .named = FALSE){
  out <- lapply(enexprs(...), as_named_call)
  if(.named){
    nms <- vctrs::vec_as_names(rep_len("call", length(out)), repair = "universal",
                               quiet = TRUE)
    setNames(out, nms)
  } else {
    out
  }
}
