adintel_path_i <- function(x){
  path <- fs::path_split(x)[[1]]
  res <- setNames(vec_tail(path, 3), c("year", "tbl_type", "tbl_name"))
  res["year"] <- as.integer(res["year"])

  if( res["tbl_type"] %in% c("impressions", "market_breaks") ) {
    tbl <- gsub("imp_", "imp__", res[["tbl_name"]])
  } else if ( res["tbl_type"] == "occurrences" ) {
    tbl <- paste0("occ__", res[["tbl_name"]])
  } else if ( res["tbl_type"] == "references" ) {
    tbl <- paste0("ref__", res[["tbl_name"]])
  } else if ( res["tbl_type"] == "universe_estimates" ) {
    tbl <- gsub("ue_", "ue__", res[["tbl_name"]])
  }
  tbl <- gsub("network_tv", "national_tv", tbl)
  tbl <- gsub("spot_tv", "local_tv", tbl)
  ptypes <- as_arrow_ptype(adintel_ptypes[[tbl]], tbl)
  tibble::as_tibble(append(as.list(res), list(tbl = tbl, ptypes = list(ptypes))))
}
#' Split adintel paths
#'
#' @param x.
#'
#' @export

adintel_paths <- function(x){
  paths <- fs::dir_ls(x, recurse = TRUE, type = "file")
  paths <- fs::path_dir(paths)
  common_path <- fs::path_common(paths)
  path_splits <- lapply(paths, adintel_path_i)
  res_paths <- gsub(common_path, "", paths)
  res <- do.call(rbind, unname(path_splits))
  res <- tibble::as_tibble(res) |>
    dplyr::mutate(path = res_paths, .before = 1)
  attr(res, "common_path") <- common_path
  res
}

