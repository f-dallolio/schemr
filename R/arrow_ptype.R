new_arrow_ptype <- function(x = list(), tbl_name = NULL){
  if(!is.null(tbl_name)) {assertthat::assert_that(is.character(tbl_name))}
  vctrs::new_rcrd(fields = list(types = x), tbl = tbl_name, class = "arrow_ptype")
}
## for compatibility with the S4 system
methods::setOldClass(c("arrow_ptype", "vctrs_vctr"))

## Vector class: `arrow_ptype` ----------
#'
#' `arrow_ptype` vector
#'
#' This creates a vector of calls that prints anc can be manipulate as a normal S3 vector.
#'
#' @param ... expressions to turn into arrow data types.
#' @return An S3 vector of class `arrow_ptypes`.
#'
#' @export
#' @examples
#' arrow_ptype(int32, decimal(1,1))
arrow_ptype <- function(..., tbl_name = NULL){
  x <- vcall(..., ns = "arrow")
  check_arrow_fn(x)
  nms0_id <- which(names(x) == "")
  v_nms <- v_names(x)
  names(x)[nms0_id] <- v_nms[nms0_id]
  out <- eval_call(x)
  new_arrow_ptype(out, tbl_name = tbl_name)
}
#' @export
is_arrow_ptype <- function(x) { inherits(x, "arrow_ptype") }
#' @export
names.arrow_ptype <- function(x) {names(field(x, "types"))}
#'
#' @export
vec_ptype_abbr.arrow_ptype <- function(x){
  "arw_ptype"
}
#' @export
format.arrow_ptype <- function(x){
  x0 <- vctrs::vec_data(x)
  types <- x0$types
  nms <- names(types)
  out <- vapply(types, function(x) call_text(x$code()), character(1))
  out <- gsub('\"',"\'",out)
  setNames(out, nms)
}

## Print ---------
rich_print <- function(x = arrow_ptype(), width = 20){
  x_df <- sapply(as.data.frame(x), function(x) paste0("<", vec_ptype_abbr(x), ">"))
  x_arw <- format(x)
  n_df <- max(str_width(x_df))
  n_arw <- max(str_width(x_arw))

  nms0 <- stringr::str_trunc(names(x), width = width, side = "right")
  n_nms0 <- max(nchar(nms0)) + 1
  nms <- stringr::str_pad(paste0(nms0, " "), n_nms0, side = "right", pad = "-")
  # rws <- str_numpad(seq_along(nms))
  rws <- seq_along(nms)
  n_rws <- max(nchar(rws))

  body <- sprintf(glue::glue("%{n_rws}.0f - %-s--- %-{n_df}s | %{-n_arw}s"),
                  rws, nms, x_df, x_arw)
  full_args <- full_args(x)
  cli::cli_h1(paste0("Table: ", tbl))
  cli::cat_line()
  cat(body, sep = "\n")
  if(!is.null(full_args)){
    cli::cat_rule()
    cli::cat_line(full_args(x))
  }
}
full_args <- function(x){
  xx <- calls_nms(field(vcall(!!!format(x)), "call"))
  xx0 <- unique(xx)
  xx1 <- lapply(xx0, function(x) rlang:::fn_fmls_names(get(x)))
  xxx <- purrr::compact(setNames(xx1, xx0))
  if(!is_empty(xxx)){
    out <- paste0(names(xxx), "[",paste(sQuote(unlist(xxx)), collapse = ","), "]")
    out <- gsub('\"', "\'", out)
    return(out)
  }
  NULL
}
#' @export
obj_print_header.arrow_ptype <- function(x){
  pt_full <- paste0("<", vec_ptype_full(x), "[", vec_size(x), "]>")
  tbl <- attr(x, "tbl")
  if(is.null(tbl)){
    cli::cat_line(pt_full)
  } else {
    cat_line(pt_full, " [ Table: ", tbl, " ]")
  }
  invisible(x)
}
#' @export
obj_print_data.arrow_ptype <- function(x){
  print(format(x), quote = FALSE)
  invisible(x)
}
#' @export
print.arrow_ptype <- function(x, rich = TRUE){
  if(rich){
    rich_print(x)
  } else {
    obj_print(x)
  }
}
#'
## Coercion ----------
#' @export
vec_ptype2.arrow_ptype.arrow_ptype <- function(x, y, ...){
  x1 <- lapply(vctrs::field(x, "types"), function(x) x$code())
  y1 <- lapply(vctrs::field(y, "types"), function(x) x$code())
  new_arrow_ptype(!!!append(x1, y1))
}
#' @export
vec_ptype2.arrow_ptype.list <- function(x, y,...){
  append(as.list(x), y)
}
#' @export
vec_ptype2.list.arrow_ptype <- function(x, y,...){
  append(x, as.list(y))
}
#' @export
vec_ptype2.arrow_ptype.vcall <- function(x, y,...){
  out <- append(lapply(vctrs::field(x, "types"), \(x) x$code()),
                vctrs::field(y, "call"))
  vcall(!!!out)
}
#' @export
vec_ptype2.vcall.arrow_ptype <- function(x, y,...){
  new_vcall(append(vctrs::field(x, "call")),
            lapply(vctrs::field(y, "types"), \(x) x$code()))
}
#'
## Casting ----------
#' @export
vec_cast.arrow_ptype.arrow_ptype <- function(x, to, ...){ x }
#' @export
vec_cast.list.arrow_ptype <- function(x, to, ...){ vctrs::field(x, "types") }
#' @export
vec_cast.arrow_ptype.vcall <- function(x, to,...){
  new_arrow_ptype(!!!vctrs::field(x, "call"))
}
#' @export
vec_cast.vcall.arrow_ptype <- function(x, to,...){
  new_vcall(lapply(vctrs::field(x, "types"), \(x) x$code()))
}
#'
#
#' Methods ----------
#' @export
infer_schema.arrow_ptype <- function(x) { arrow::schema(vctrs::field(x, "types")) }
#' @export
as.data.frame.arrow_ptype <- function(x) { as.data.frame(arrow::schema(x)) }
#' @export
as_df_list <- function(x){ vctrs::df_list(!!!x) }
#' @export
as_tibble.arrow_ptype <- function(x) dplyr::as_tibble(as.data.frame(x))
#'
#'
## Manipulations ----------
#' @export
select.arrow_ptype <- function(.data, ...){
  x <- vec_cast(.data, list())
  pos <- tidyselect::eval_select(expr = expr(c(...)),
                                 data = x)
  new_arrow_ptype(setNames(x[pos], names(pos)))
}
#' @export
relocate.arrow_ptype <- function(.data, ..., .before = NULL, .after = NULL){
  x <- vec_cast(.data, list())
  pos <- tidyselect::eval_relocate(expr = expr(c(...)),
                                   data = x,
                                   before = enquo(.before),
                                   after = enquo(.after))
  new_arrow_ptype(setNames(x[pos], names(pos)))
}
#' @export
rename.arrow_ptype <- function(.data, ...){
  x <- vec_cast(.data, list())
  pos <- tidyselect::eval_select(expr = expr(c(...)),
                                 data = x)
  names(x)[pos] <- names(pos)
  new_arrow_ptype(x)
}
#' @export
mutate.arrow_ptype <- function(.data, ..., .before = NULL, .after = NULL){
  x <- arrow_ptype(...)
  x_nms <- names(x)
  pos_old <- tidyselect::eval_select(dplyr::any_of(x_nms), data = .data)
  .data[pos_old] <- x[names(pos_old)]
  x_new <- x[! x_nms %in% names(.data)]
  out <- append(field(.data, "types"), field(x_new, "types"))
  if( !is.null(.before) || !is.null(.after)){
    rel_pos <- tidyselect::eval_relocate(
      dplyr::all_of(x_nms), data = out, before = enquo(.before), after = enquo(.after)
    )
    out <- out[rel_pos]
  }
  arrow_ptype(!!!lapply(out, function(x) x$code()))
}


#' @export
rich_print <- function(x = arrow_ptype(), width = 20){

  # x_df <- sapply(as.data.frame(x), function(x) paste0("<", class(x)[[1]], ">"))
  x_df <- sapply(as.data.frame(x), function(x) paste0("<", vec_ptype_abbr(x), ">"))
  x_arw <- format(x)
  n_df <- max(str_width(x_df))
  n_arw <- max(str_width(x_arw))

  nms0 <- stringr::str_trunc(names(x), width = width, side = "right")
  n_nms0 <- max(nchar(nms0)) + 1
  nms <- stringr::str_pad(paste0(nms0, " "), n_nms0, side = "right", pad = "-")
  # rws <- str_numpad(seq_along(nms))
  rws <- seq_along(nms)
  n_rws <- max(nchar(rws))

  body <- sprintf(glue::glue("%{n_rws}.0f - %-s--- %-{n_df}s | %{-n_arw}s"),
                  rws, nms, x_df, x_arw)
  full_args <- full_args(x)
  tbl <- attr(x, "tbl")
  if(is.null(tbl)){ tbl <- "..." }

  cli::cli_h1(paste0("Table:   ", tbl, "   "))
  cat(body, sep = "\n")
  if(!is.null(full_args)){
    cli::cat_rule()
    cli::cat_line(full_args(x))
  }
}
#' @export
full_args <- function(x){
  xx <- calls_nms(field(vcall(!!!format(x)), "call"))
  xx0 <- unique(xx)
  xx1 <- lapply(xx0, function(x) rlang:::fn_fmls_names(get(x)))
  xxx <- purrr::compact(setNames(xx1, xx0))
  if(!is_empty(xxx)){
    out <- paste0(names(xxx), "[",paste(sQuote(unlist(xxx)), collapse = ","), "]")
    out <- gsub('\"', "\'", out)
    return(out)
  }
  NULL
}
