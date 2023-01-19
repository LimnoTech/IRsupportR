#' compile_basic_recent
#'
#' @param my_basic_summary_metals
#' @param my_basic_summary_pah
#' @param my_basic_summary_other
#'
#' @return
#' @export
#'
#' @examples
compile_basic_recent <- function(my_basic_summary_recent_metals,  my_basic_summary_recent_pah, my_basic_summary_recent_other) {


  df_compiled <- my_basic_summary_recent_metals %>%
    dplyr::bind_rows(my_basic_summary_recent_pah, my_basic_summary_recent_other)

  return(df_compiled)

}
