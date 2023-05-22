#' Compile Basic Summaries for Recent Timeframe
#'
#' Bind together metal and organic basic data summaries for the same recent
#' timeframe.
#'
#' @param my_basic_summary_recent_metals obtained from
#'   \code{\link{summarize_basic_recent_metals}}.
#' @param my_basic_summary_recent_other obtained from
#'   \code{\link{summarize_basic_recent}}.
#'
#' @return dataframe with same columns as inputs.
#' @export
#'
#' @examples
#' compile_basic_recent(my_basic_summary_10yr_metals, my_basic_summary_10yr_other)

compile_basic_recent <- function(my_basic_summary_recent_metals, my_basic_summary_recent_other) {


  df_compiled <- my_basic_summary_recent_metals %>%
    dplyr::bind_rows( my_basic_summary_recent_other)

  return(df_compiled)

}
