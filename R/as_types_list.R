#' Lists of data types
#'
#' @export
#' @name as_types_list
NULL
#'
#' @rdname as_types_list
#' @export
as_types_list <- function(x, .fn = NULL, ...){
  UseMethod("as_types_list")
}
#' @rdname as_types_list
#' @export
as_types_list.default <- function(x, .fn = NULL, ...){
  fn <- "schemr::as_types_list"
  cls <- class(x)[[1]]
  cli::cli_abort("No {.fn {fn}} method for class {.cls {cls}}" )
}
#' @rdname as_types_list
#' @export
as_types_list.arrow_ptype <- function(x, .fn = NULL, ...){
  x0 <- field(x, "types")
  nms <- stringr::str_squish(names(x))
  if ( !is.null(.fn) ) {
    nms <- rlang::as_function(.fn)(nms)
  }
  setNames(x0, nms)
}
#' @rdname as_types_list
#' @export
as_types_list.Schema <- function(x, .fn = NULL, ...){
  x0 <- as.list(x)
  nms <- stringr::str_squish(names(x))
  if ( !is.null(.fn) ) {
    nms <- rlang::as_function(.fn)(nms)
  }
  lapply(setNames(x0, nms), arrow::as_data_type)
}
#' @rdname as_types_list
#' @export
as_types_list.data.frame <- function(x, .fn = NULL, ...){
  x0 <- as.list(arrow::schema(x))
  nms <- stringr::str_squish(names(x))
  if ( !is.null(.fn) ) {
    nms <- rlang::as_function(.fn)(nms)
  }
  lapply(setNames(x0, nms), arrow::as_data_type)
}
#' @rdname as_types_list
#' @export
as_types_list.FileSystemDataset <- function(x, .fn = NULL, ...){
  x0 <- as.list(arrow::schema(x))
  nms <- stringr::str_squish(names(x))
  if ( !is.null(.fn) ) {
    nms <- rlang::as_function(.fn)(nms)
  }
  lapply(setNames(x0, nms), arrow::as_data_type)
}
#' @rdname as_types_list
#' @export
as_types_list.Table <- function(x, .fn = NULL, ...){
  x0 <- as.list(arrow::schema(x))
  nms <- stringr::str_squish(names(x))
  if ( !is.null(.fn) ) {
    nms <- rlang::as_function(.fn)(nms)
  }
  lapply(setNames(x0, nms), arrow::as_data_type)
}
#' @rdname as_types_list
#' @export
as_types_list.StructArray <- function(x, .fn = NULL, ...){
  x0 <- as.list(arrow::schema(x))
  nms <- stringr::str_squish(names(x))
  if ( !is.null(.fn) ) {
    nms <- rlang::as_function(.fn)(nms)
  }
  lapply(setNames(x0, nms), arrow::as_data_type)
}



#
#
#
# schema(mtcars)
# as_types_call <- function(x){
#   x0 <- field(x, "types")
#   f <-
#   lapply(x0, f)
# }
# x <- as_arrow_ptype(mtcars)
# function(x) {
#   gsub('\"',"\'",format(x))
#
#   paste0(xlst[[12]][[1]], "(", paste(xlst[[12]][[2]], collapse = ",") , ")")
#   paste(xlst)
# }
