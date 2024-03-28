#' As arrow ptype
#'
#' @export
#' @name as_arrow_ptype
NULL
#'
#' @rdname as_arrow_ptype
#' @export
as_arrow_ptype <- function(x, .fn = NULL, tbl_name = NULL){
  UseMethod("as_arrow_ptype")
}
#' @rdname as_arrow_ptype
#' @export
as_arrow_ptype.Schema <- function(x, .fn = NULL, tbl_name = NULL){
  f <- function(x) x$code()
  x <- lapply(as_types_list(x, .fn = .fn), f)
  arrow_ptype(!!!x, tbl_name = tbl_name)
}
#' @rdname as_arrow_ptype
#' @export
as_arrow_ptype.data.frame <- function(x, .fn = NULL, tbl_name = NULL){
  x0 <- schema(x)
  f <- function(x) x$code()
  x <- lapply(as_types_list(x0, .fn = .fn), f)
  arrow_ptype(!!!x, tbl_name = tbl_name)
}
#' @rdname as_arrow_ptype
#' @export
as_arrow_ptype.FileSystemDataset <- function(x, .fn = NULL, tbl_name = NULL){
  x0 <- schema(x)
  f <- function(x) x$code()
  x <- lapply(as_types_list(x0, .fn = .fn), f)
  arrow_ptype(!!!x, tbl_name = tbl_name)
}
#' @rdname as_arrow_ptype
#' @export
as_arrow_ptype.Table <- function(x, .fn = NULL, tbl_name = NULL){
  x0 <- schema(x)
  f <- function(x) x$code()
  x <- lapply(as_types_list(x0, .fn = .fn), f)
  arrow_ptype(!!!x, tbl_name = tbl_name)
}
#' @rdname as_arrow_ptype
#' @export
as_arrow_ptype.StructArray <- function(x, .fn = NULL, tbl_name = NULL){
  x0 <- schema(x)
  f <- function(x) x$code()
  x <- lapply(as_types_list(x0, .fn = .fn), f)
  arrow_ptype(!!!x, tbl_name = tbl_name)
}


