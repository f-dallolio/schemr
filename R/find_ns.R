#
# .auto_ns <- function(x, full_out = TRUE){
#   check_call(x)
#   ns <- call_ns(x)
#   if(is.null(ns)){
#     fn <- try(get(call_name(x)))
#     if(is_function(fn)){
#       ns <- ns_env_name(fn)
#     } else {
#       ns <- NULL
#     }
#   }
#   if(full_out){
#     call2(call_name(x), !!!call_args(x), .ns = ns)
#   } else {
#     ns
#   }
# }
# auto_ns <- function(..., ns_only = FALSE){
#   sapply(list2(...), .auto_ns)
# }
#
# x <- auto_ns(expr(int32()))
# objs_as_call(arroe::int32)
