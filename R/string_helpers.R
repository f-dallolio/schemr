#' Pad number strings
#'
#' @param x a numeric vector.
#'
#' @return a character vector of numbers padded left with '0'.
#' @export
#'
#' @examples
#' str_numpad(1:15)
str_numpad <- function(x){
  check_numeric(x)
  x <- as.character(x)
  nmax <- max(stringr::str_width(x))
  stringr::str_pad(x, width = nmax, side = "left", pad = "0", use_width = TRUE)
}
#' @export
str_oxford <- function(x){
  n <- vec_size(x)
  if(n == 1){
    return(as.character(x))
  }
  x1 <- paste(x[-n], collapse = ", ")
  x2 <- x[n]
  if (n == 2) {
    paste0(x1, " and ", x2)
  } else {
    paste0(x1, ", and ", x2)
  }
}

## ----------
#' Vector of integer characters padded with `V`
#'
#' @param x a vector.
#'
#' @return a character vector of numbers padded left with 'V' or 'V0'.
#' @export
#'
#' @examples
#' v_names(letters)
v_names <- function(x){
  paste0("V", str_numpad(seq_len(vec_size(x))))
}
