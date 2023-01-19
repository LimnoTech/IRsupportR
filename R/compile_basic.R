#' compile_basic
#'
#' @param my_basic_summary_metals
#' @param my_basic_summary_pah
#' @param my_basic_summary_other
#'
#' @return
#' @export
#'
#' @examples
compile_basic <- function(my_basic_summary_other,  my_basic_summary_pah, my_basic_summary_metals) {


  df_compiled <- my_basic_summary_other %>%
    dplyr::bind_rows(my_basic_summary_pah, my_basic_summary_metals)

  return(df_compiled)

}
