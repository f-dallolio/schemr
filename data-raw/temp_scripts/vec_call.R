
library(rlang)
library(vctrs)
library(schemr)

as_call <- function(x, ...){
  UseMethod("as_call")
}

as_call.default <- function(x, ...){
  as.call(x)
}

vec_call <- function(...){
  x <- objs_as_call(...)
  new_vec_call(x)
}
new_vec_call <- function(x = list()){
  if(is_empty(x)){
    fields <- list(name = character(),
                   args = list(),
                   ns = list())
  } else {
    out <- split_calls(splice(x))
    nms <- names(out)
    if(all(nms == "")){ nms <- NULL }
    names(out) <- NULL
    fields <- purrr::list_transpose(out)
  }

  vctrs::new_rcrd(fields, rcrd_names = nms, class = "vec_call")
}

is_vec_call <- function(x){
  rlang::in
}

names.vec_call <- function(x){
  attr(x, "rcrd_names")
}
`names<-.vec_call` <- function(x, value) {
  attr(x, "rcrd_names") <- value
  x
}


format.vec_call <- function(x){
  x <- vctrs::vec_data(x)
  x_args <- lapply(x$args, rlang::splice)
  out <- mapply(rlang::call2, .fn = x$name, x_args, .ns = x$ns, USE.NAMES = FALSE)
  vapply(out, as_label, character(1))
}

as_call.vec_call <- function(x, use.names = TRUE){
  x <- vec_data(x)
  nms <- attr(x, "rcrd_names")
  out <- mapply(rlang::call2, .fn = x$name, x_args, .ns = x$ns, USE.NAMES = FALSE)
  names(out) <- nms
  out
}

vec_ptype.vec_call.vec_call <- function(x, y, ...){
  out <- c(as_call(x), as_call(y))
  vec_call(!!!out)
}
cast.vec_call.vec_call <- function(x, to, ...){
  x
}

call_get_names <- function(x, ...){
  UseMethod("call_get_names")
}
call_get_names.default <- function(x, ...){
  i <- c(...)
  if(is_empty(i)){i <- seq_along(x)}
  pos <- vec_as_location(i, n = length(x), names = names(x))
  out <- vapply(x[pos], call_name, character(1))
  if(all(names(out) == "")){ names(out) <- NULL}
  out
}
call_get_names.vec_call <- function(x, ...){
  i <- c(...)
  if(is_empty(i)){i <- seq_along(x)}
  pos <- vec_as_location(i, n = length(x), names = names(x))
  vctrs::field(x, "name")[pos]
}

call_get_args <- function(x, ...){
  UseMethod("call_get_args")
}
call_get_args.default <- function(x, ...){
  i <- c(...)
  if(is_empty(i)){i <- seq_along(x)}
  pos <- vec_as_location(i, n = length(x), names = names(x))
  out <- sapply(x[pos], call_args)
  if(all(names(out) == "")){ names(out) <- NULL}
  out
}
call_get_args.vec_call <- function(x, ...){
  i <- c(...)
  if(is_empty(i)){i <- seq_along(x)}
  pos <- vec_as_location(i, n = length(x), names = names(x))
  vctrs::field(x, "args")[pos]
}

call_get_ns <- function(x, ...){
  UseMethod("call_get_ns")
}
call_get_ns.default <- function(x, ...){
  i <- c(...)
  if(is_empty(i)){i <- seq_along(x)}
  pos <- vec_as_location(i, n = length(x), names = names(x))
  out <- sapply(x[pos], call_ns)
  if(all(names(out) == "")){ names(out) <- NULL}
  out
}
call_get_ns.vec_call <- function(x, ...){
  i <- c(...)
  if(is_empty(i)){i <- seq_along(x)}
  pos <- vec_as_location(i, n = length(x), names = names(x))
  vctrs::field(x, "ns")[pos]
}
