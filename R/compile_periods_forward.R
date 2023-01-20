#' compile_periods_forward
#'
#' @param my_basic_summary_metals
#' @param my_basic_summary_pah
#' @param my_basic_summary_other
#'
#' @return
#' @export
#'
#' @examples
compile_periods_forward <- function(my_period_summary_forward_metals,  my_period_summary_forward_pah, my_period_summary_forward) {


  df_compiled <- my_period_summary_forward_metals %>%
    dplyr::bind_rows(my_period_summary_forward_pah, my_period_summary_forward)

  return(df_compiled)

}
