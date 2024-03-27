new_vcall <- function(x = list(), env = NULL) {
  calls_check <- vapply(x, rlang::is_call_simple, logical(1))

  if( !is.null(env) ) {
    ns <- unique(unlist(lapply(x, call_ns)))
    assertthat::assert_that(length(ns) <= 1)
    if(length(ns) > 0){
      env <- asNamespace(ns)
    }
  }

  if( any(!calls_check) ) {
    id <- which(any(!calls_check))
    cli::cli_abort(message = paste0("objs ",
                                    stringr::str_flatten_comma(id, last = ", and "),
                                    " is/are not calls."))
  }

  fields <- list(call = x)
  vctrs::new_rcrd(fields = fields, env = env, class = "vcall")
}


# for compatibility with the S4 system
methods::setOldClass(c("vcall", "vctrs_vctr"))

#' `vcall` vector
#'
#' This creates a vector of calls that prints anc can be manipulate as a normal S3 vector.
#'
#' @param ... expressions to turn into calls.
#' @return An S3 vector of class `vcall`.
#'
#' @export
#' @examples
#' vcall(mean, "mean", mean(), "mean()")
vcall <- function(..., ns = NULL){
  x <- objs_as_call(...)
  if(!is.null(ns)){
    env <- asNamespace(ns)
  } else {
    env <- NULL
  }
  new_vcall(x, env = env)
}

#' @export
#' @rdname vcall
is_vcall <- function(x){
  inherits(x, "vcall")
}
#' @export
#' @rdname vcall
format.vcall <- function(x){
  if( !rlang::is_empty(x) ){
    call <- vctrs::vec_data(x)[["call"]]
    vapply(call, rlang::as_label, character(1))
  }
}
#' @export
#' @rdname vcall
vec_ptype_full.vcall <- function(x){
  "vctr_of<calls>"
}
#' @export
#' @rdname vcall
vec_ptype_abbr.vcall <- function(x){
  env <- attr(x, "env")
  if(is.null(env)){
    "calls"
  } else {
    paste0("ns:", rlang::ns_env_name(env))
  }
}
#' @export
#' @rdname vcall
obj_print_data.vcall <- function(x){
  nms <- names(x)
  if( all(nms == "") ){ nms = NULL }
  out <- vapply(field(x, "call"), call_text, character(1))
  print(setNames(out, nms), quote = FALSE)
}
#' @export
#' @rdname vcall
obj_print_footer.vcall <- function(x){
  env <- attr(x, "env")
  if(!is.null(env)){
    cli::cat_line(paste0("ns: <", rlang::ns_env_name(env), ">"))
  }
  invisible(x)
}
#' @export
#' @rdname vcall
names.vcall <- function(x){ names(field(x, "call")) }
#' @export
#' @rdname vcall
`names<-.vcall` <- function(x, value){
  names(field(x, "call")) <- value
  x
}
#' @export
#' @rdname vcall
`[<-.vcall` <- function(x, ..., values){
  pos <- tidyselect::eval_select(expr(c(...)), data = field(x, "call"))
  field(x, "call")[pos] <- values
  x
}
#' @export
#' @rdname vcall
`[[.vcall` <- function(x, i){
  pos <- vec_as_location(i, n = vec_size(x), names = names(x))
  field(x, "call")[[pos]]
}
#' @export
#' @rdname vcall
`[[<-.vcall` <- function(x, i, value){
  id <- vec_as_location(i, n = vec_size(x), names = names(x))
  field(x, "call")[[id]] <- value
  x
}

