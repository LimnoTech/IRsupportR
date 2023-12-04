#' format_period_summary
#'
#' @param period_summary
#'
#' @return
#' @export
#'
#' @examples
format_period_summary <- function(period_summary,
                                  period_end_year) {

  period_end_year <- lubridate::year(as.Date(paste(period_end_year, 1, 1, sep = "-")))


  # Limit range so that it does not go beyond analysis period
  df <- period_summary %>%
    dplyr::mutate(adjusted_start_year = dplyr::case_when(end_year > period_end_year ~ period_end_year - 3,
                                                         TRUE ~ start_year),
                  adjusted_end_year = dplyr::case_when(end_year > period_end_year ~ period_end_year,
                                              TRUE ~ end_year))

  # Find periods with more than one exceedance
  df_summary_ccc <- df %>%
    dplyr::filter(exceedance_ccc_3yr_sum > 1) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::summarize(most_recent_period_with_multiple_ccc_exceedances = paste0(max(adjusted_start_year), " - ", max(adjusted_end_year)))


  df_summary_cmc <- df %>%
    dplyr::filter(exceedance_cmc_3yr_sum > 1) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::summarize(most_recent_period_with_multiple_cmc_exceedances = paste0(max(adjusted_start_year), " - ", max(adjusted_end_year)))


  df_summary_d <- df %>%
    dplyr::filter(exceedance_d_3yr_sum > 1) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::summarize(most_recent_period_with_multiple_d_exceedances = paste0(max(adjusted_start_year), " - ", max(adjusted_end_year)))


  df_summary <- dplyr::full_join(df_summary_ccc, df_summary_cmc)

  df_summary <- dplyr::full_join(df_summary, df_summary_d)

}
