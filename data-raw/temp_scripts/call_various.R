# f <- function(x){
#   check_call(x)
#   fn <- x[[1]]
#   args <- as.list(x)[-1]
#
#   if(is_call(fn, c("::", ":::"))){
#     ns <- asNamespace(as_string(fn[[2]]))
#     name <- fn[[3]]
#   } else {
#     ns <- NULL
#     name <- as_string(fn)
#   }
#
#   fn_exists <- exists(name, mode = "function")
#   if(is.null(ns) %% fn_exists){
#     ns <- ns_env_name(get(name, mode = "function"))
#   }
#
#   list(name = name, args = args, ns)
# }
# x0 <- map(list("base::mean(1:10)", "mean(1:10)"), str2lang)
# x <- x0[[1]]
# f(x)
# ns_env("rlang")
call_list <- function(x){
  out <- list(name = call_name(x),
              args = call_args(x))
  ns <- call_ns(x) %||% ""
  structure(out, ns = ns, class = "call_list")
}
call_list(x=call2("x"))

call_match_self <- function(x, ...){
  check_call(x)
  fn <- try(eval(x[[1]]), silent = TRUE)
  if(is_function(fn)){
    call_match(x, fn, defaults = TRUE, ...)
  } else {
    cli::cli_abort("Cannot find the function {.val {as_string(x[[1]])}}")
  }
}

vec_call <- function(...){
  x <- objs_as_call(...)
  nms <- names(x)
  if(all(nms == "")){
    names(x) <- NULL
  }
  new_rcrd(list(call = x), class = "vec_call")
}
names.vec_call <- function(x){
  names(field(x, "call"))
}
`names<-.vec_call` <- function(x, value){
  `names<-`(field(x, "call"), value)
  x
}
format.vec_call <- function(x){
  x0 <- vec_data(x)
  vapply(field(x0, "call"), as_label, character(1))
}

x[[1]]

split_calls(!!!xx)

x <- vec_call(a = int32, b = decimal(1,1))
attr(x, "calls")

vec_call <- function(...){
  x <- objs_as_call(...)
  new_vec_call(x)
}
new_vec_call <- function(x = list()){
  check_objs_are_call(splice(x))
  out <- lapply(x, as_label)
  new_vctr(out, calls = x, class = "vec_call")
}

get_calls <- function(x){
  attr(x, "calls")
}

call_get_name <- function(x){
  UseMethod("call_get_name")
}
call_get_name.call <- function(x){
  call_name(x)
}
call_get_name.vec_call <- function(x){
  x <- attr(x, "calls")
  sapply(x, call_name)
}

call_get_args <- function(x){
  UseMethod("call_get_args")
}
call_get_args.call <- function(x){
  call_name(x)
}
call_get_args.vec_call <- function(x){
  x <- attr(x, "calls")
  lapply(x, call_args)
}

call_get_ns <- function(x){
  UseMethod("call_get_ns")
}
call_get_ns.call <- function(x){
  call_ns(x)
}
call_get_ns.vec_call <- function(x){
  x <- attr(x, "calls")
  sapply(x, call_ns)
}

has_ns <- function(x){
  check_call(x)
  head <- x[[1]]
  is_call(head, c("::", ":::"))
}
x <- quo_call

f <- function(x){
  quo <- enquo(x)
  if(quo_is_call(quo)){
    quo_call <- quo_get_expr(quo)
    if(has_ns(quo_call)){
      call_fn <- quo_call[[3]]
      call_ns <- quo_call[[2]]
      if(is_call_simple(call_fn)){
        call_fn
      } else {
        call2(as_label_x, .ns = call_ns)
      }
    }
  }
}
quo_call <- has_ns(quote(arrow::int32))
quo <- quo(arrow::int32)

call_get_args(x)

call_get_ns.vec_call(x)


fn_exists <- function(x){
  x0 <- enexpr(x)
  if(!is_string(x0)){
    out <- get(expr_text(x0), mode = "function")
  } else {
    out <- get(x, mode = "function")
  }
  if(is_function(out)){
    out
  } else {
    NULL
  }
}

is_function(eval(quote(mn)))
