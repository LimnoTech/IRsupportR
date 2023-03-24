#' lookup_criteria_ratio
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
lookup_criteria_ratio <- function(df) {


  # Join categories to main dataframe
  df <- df %>%
    dplyr::left_join(criteria_ratios,
                     by = "pollutant_name")

}
