#' read_raw_data
#'
#' @param
#'
#' @return df
#' @export
#'
#' @examples
read_raw_data <- function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE, fill = false) {

  df <- list(...)
  df_combined <- do.call(rbind, df)
  return(df_combined)

}
