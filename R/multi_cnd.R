#' `multi_cnd` vector
#'
#' This creates a vector of conditions re. a vector.
#'
#' @param x a vector.
#' @param ... conditions.
#' @return An S3 vector of class `multi_cnd`.
#'
#' @export
#' @examples
#' multi_cnd(letters, is.numeric, ~ is.character(.x))
multi_cnd <- function(x, ...){
  arg <- rlang::caller_arg(x)
  fn2 <- rlang::enquos(..., .named = TRUE)
  out_fn2 <- vector("list", length = length(fn2))
  for(i in seq_along(fn2)){
    fn_flag <- rlang::is_function(rlang::eval_tidy(fn2[[i]]))
    fml_flag <- rlang::is_formula(rlang::eval_tidy(fn2[[i]]))
    if(fml_flag) {
      fn_out <- rlang::as_function(rlang::eval_tidy(fn2[[i]]))(x)
      if(isFALSE(fn_out)){
        fn_text <- rlang::f_text(rlang::eval_tidy(fn2[[i]]))
        fn_text <- gsub(".x", arg, fn_text)
        fn_call <- rlang::call2("validate_that", str2lang(fn_text), .ns = "assertthat")
        fn_out <- eval(fn_call)
      }
    } else if (fn_flag) {
      fn_out <- rlang::as_function(rlang::eval_tidy(fn2[[i]]))(x)
      if(isFALSE(fn_out)){
        fn_text <- strsplit(names(fn2)[[i]], "::")[[1]]
        fn_text[length(fn_text)]
        fn_text <- rlang::as_label(rlang::call2(fn_text, str2lang(arg)))
        fn_call <- rlang::call2("validate_that", str2lang(fn_text), .ns = "assertthat")
        fn_out <- eval(fn_call)
      }
    } else {
      cli::cli_abort("{.arg {fn2}} must be function or a formula.")
    }
    out_nms <- if(is.character(fn_out)) "x" else "v"
    out_res <- if(is.character(fn_out)) FALSE else TRUE
    out_res_msg <- if(is.character(fn_out)) fn_out else ""
    out_fn2[[i]] <- data.frame(out = out_res,
                               is_fml = fml_flag,
                               fn = names(fn2)[[i]],
                               msg = out_res_msg,
                               nms = out_nms)
  }
  res <- vctrs::df_list(do.call("rbind", out_fn2))
  vctrs::new_rcrd(res, input_name = arg, class = "multi_cnd")
}
# for compatibility with the S4 system
methods::setOldClass(c("multi_cnd", "vctrs_vctr"))
#' @export
#' @rdname multi_cnd
vec_ptype_abbr.multi_cnd <- function(x){"cnds"}
#' @export
#' @rdname multi_cnd
vec_ptype_full.multi_cnd <- function(x){
  paste0(attr(x, "input_name"), "_cnds")
}
#' @export
#' @rdname multi_cnd
names.multi_cnd <- function(x){
  vctrs::field(x, "fn")
}
#' @export
#' @rdname multi_cnd
`names<-.multi_cnd` <- function(x, value){
  `names<-`(vctrs::field(x, "fn"), value)
  x
}
#' @export
#' @rdname multi_cnd
format.multi_cnd <- function(x){
  setNames(vctrs::field(x, "out"), vctrs::field(x, "fn"))
}
#' @export
#' @rdname multi_cnd
obj_print_header.multi_cnd <- function(x){
  msg <- paste0("<", "cnds[", vctrs::vec_size(x),"]>   Variable:" ,"{.emph {.strong {attr(x, 'input_name')}}}")
  cli::cli_inform(msg)
  invisible(x)
}
#' @export
#' @rdname multi_cnd
obj_print_data.multi_cnd <- function(x){
  df <- vctrs::vec_data(x)
  df$fn[!df$is_fml] <- paste0("{.fn ", df$fn[!df$is_fml], "}")
  df$fn[df$is_fml] <- paste0("`", df$fn[df$is_fml], "`")
  df$fn <- paste0("..", str_numpad(seq_along(df$fn)), " ", df$fn)
  out_msg <- setNames(df$fn, df$nms)
  cli::cli_h3("Returns:")
  cli::cli_inform(out_msg)
  invisible(x)
  ##----
  # df$fn[!df$is_fml] <- paste0("{.fn ", df$fn[!df$is_fml], "}")
  # df$fn[df$is_fml] <- paste0("`", df$fn[df$is_fml], "`")
  # df$fn <- paste0("..", str_numpad(seq_along(df$fn)), " ", df$fn)
  # x_true <- subset(df, out)
  # x_false <- subset(df, !out)
  # if(NROW(x_true) > 0){
  #   out_msg <- setNames(x_true$fn, x_true$nms)
  #   cli::cli_h3("Returns TRUE")
  #   cli::cli_inform(out_msg)
  # }
  # if(NROW(x_false) > 0){
  #   out_msg <- paste0(x_false$fn, " - ", x_false$msg)
  #   out_msg <- setNames(out_msg, x_false$nms)
  #
  #   cli::cli_h3("Returns FALSE")
  #   cli::cli_inform(out_msg)
  # }
  ## ----
}
#' @export
#' @rdname multi_cnd
vec_math.multi_cnd <- function(.fn, x, ...) {
  switch (.fn,
          all = all(vctrs::field(x, "out")),
          any = any(vctrs::field(x, "out")),
          and_c = all(vctrs::field(x, "out")),
          or_c = any(vctrs::field(x, "out")),
          vec_math_base(.fn, x, ...)
  )
}


#' @export
#' @rdname multi_cnd
and_c <- function(x, ...) {
  UseMethod("and_c")
}
#' @export
#' @rdname multi_cnd
and_c.default <- function(...){
  all(..., na.rm = TRUE)
}
#' @export
#' @rdname multi_cnd
and_c.multi_cnd <- function(x, ..., verbose = TRUE){
  out <- all(field(x, "out"), na.rm = TRUE)
  obj_print_header(x)
  if(out){
    cli::cat_line()
    cli::cli_alert_success("TRUE")
  } else {
    cli::cat_line()
    cli::cli_alert_danger("FALSE")
  }
  if(verbose){
    obj_print_data(x)
  }
  invisible(out)
}
#' @export
#' @rdname multi_cnd
or_c <- function(x, ...) {
  UseMethod("or_c")
}
#' @export
#' @rdname multi_cnd
or_c.default <- function(...){
  any(..., na.rm = TRUE)
}
#' @export
#' @rdname multi_cnd
or_c.multi_cnd <- function(x, ..., verbose = TRUE){
  out <- any(field(x, "out"), na.rm = TRUE)
  obj_print_header(x)
  if(out){
    cli::cat_line()
    cli::cli_alert_success("TRUE")
  } else {
    cli::cat_line()
    cli::cli_alert_danger("FALSE")
  }
  if(verbose){
    obj_print_data(x)
  }
  invisible(out)
}

