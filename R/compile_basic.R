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
compile_basic <- function(my_basic_summary_metals, my_basic_summary_pah, my_basic_summary_other) {


  df_compiled <- my_basic_summary_metals %>%
    dplyr::bind_rows(my_basic_summary_pah, my_basic_summary_other)

  return(df_compiled)

}
