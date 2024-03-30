#' Get/set quosures expressions and environments
#'
#' @param x quosures.
#' @param exprs list of expressions.
#' @param env an environment.
#'
#' @name quos_helpers
NULL
#'
#' @name quos_helpers
#' @export
quos_get_expr <- function(x){
  assertthat::assert_that(is_quosures(x))
  lapply(x, rlang::quo_get_expr)
}
#'
#' @name quos_helpers
#' @export
quos_set_expr <- function(x, exprs){
  assertthat::assert_that(is_quosures(x))
  mapply(rlang::quo_set_expr, x, exprs)
}
#'
#' @name quos_helpers
#' @export
quos_get_env <- function(x){
  assertthat::assert_that(is_quosures(x))
  lapply(x, rlang::quo_get_env)
}
#'
#' @name quos_helpers
#' @export
quos_set_env <- function(x, env){
  assertthat::assert_that(is_quosures(x))
  mapply(rlang::quo_set_env, x, env)
}

lcheck <- function(x, fn){
  check_list(x)
  arg <- rlang::caller_arg(x)
  fname0 <- rlang::caller_arg(fn)
  fn <- purrr::as_mapper(eval(enexpr(fn)))
  if(rlang::is_lambda(fn)){
    fn_as_list <- as.list(fn_body(fn))
    fname = gsub("~", "", rlang::as_label(fn_as_list[[length(fn_as_list)]]))
  } else {
    fname1 <- paste0(rlang::ns_env_name(get(fname0)), '::', fname0)
    fname <- paste0("{.fn ",fname1, "}")
  }
  out <- vapply(x, fn, logical(1))
  id_f <- which(!out)
  if(length(id_f) > 0){
    # cli::cli_abort(c(paste0("All elements in {.arg {arg}} must return TRUE for ", fname, "."),
    cli::cli_abort(c(paste0("All elements in {.arg {arg}} must return TRUE for {.fn", fname, "."),
                   "i" = "Elements returning FALSE: {id_f}"))
  }
  invisible(x)
}
library(rlang)
lcheck(x_lst, fn = ~ length(.x) == 1)

arg="x"
fname0 = "is.integer"

x_lst <- list(2L, 3)
xx <- lcheck(x_lst, fn = is_scalar_integer)
xx
check_scalar_integer(2L)

new_function(args = pairlist2(), body = expr()) |> class()

fn_list<- function(...){
  fns0 <- exprs(..., .named = TRUE)
  lcheck()
  fns <- lapply(fns0, function(x) purrr::as_mapper(eval(x)))
  new_list_of(fns, class = "fn_list")
}
checks_obj(is_character, ~ length(.x) == 1)


.call = quote(log(mean(1:10)))

check_call(.call)
id <- which(Vectorize(is_call)(as.list(.call)))
id

which(Vectorize(is_call)(as.list(.call)[[id]]))
as_label(as.list(quote(length(1) == 1))[[1]])




