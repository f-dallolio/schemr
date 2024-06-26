% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{checks}
\alias{checks}
\alias{check_call_simple}
\alias{check_call}
\alias{check_function}
\alias{check_lambda_function}
\alias{check_scalar_character}
\alias{check_scalar_double}
\alias{check_scalar_integer}
\alias{check_scalar_list}
\alias{check_scalar_logical}
\alias{check_scalar_vector}
\alias{check_bool}
\alias{check_string}
\alias{check_character}
\alias{check_double}
\alias{check_integer}
\alias{check_list}
\alias{check_logical}
\alias{check_vector}
\title{Vector checks}
\usage{
check_call_simple(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_call(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())

check_function(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())

check_lambda_function(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_scalar_character(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_scalar_double(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_scalar_integer(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_scalar_list(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_scalar_logical(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_scalar_vector(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_bool(
  x,
  ...,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_string(
  x,
  ...,
  allow_empty = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_character(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_double(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())

check_integer(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())

check_list(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())

check_logical(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

check_vector(x, ..., arg = rlang::caller_arg(x), call = rlang::caller_env())
}
\arguments{
\item{x}{input object.}
}
\value{
an error if the condition is not satisfied. The function returns \code{x} invisibly otherwise.
}
\description{
Vector checks
}
\examples{
input <- 2L
check_scalar_integer(input)
check_character(input)

}
