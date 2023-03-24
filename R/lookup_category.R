#' lookup_category
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
lookup_category <- function(df) {


  # Join categories to main dataframe
  df <- df %>%
    dplyr::left_join(ir_categories,
                     by = c("waterbody_segment", "pollutant_name"))

}
