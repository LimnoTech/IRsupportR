#' format_period_summary
#'
#' @param period_summary
#'
#' @return
#' @export
#'
#' @examples
format_period_summary <- function(period_summary) {

  df <- period_summary


  df_summary_ccc <- df %>%
    dplyr::mutate(start_year = lubridate::year(start_date),
                  end_year = lubridate::year(end_date)) %>%
    dplyr::filter(exceedance_ccc_3yr_sum > 1) %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(most_recent_period_with_multiple_ccc_exceedances = paste0(max(end_year)-3, " - ", max(end_year)))


  df_summary_cmc <- df %>%
    dplyr::mutate(start_year = lubridate::year(start_date),
                  end_year = lubridate::year(end_date)) %>%
    dplyr::filter(exceedance_cmc_3yr_sum > 1) %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(most_recent_period_with_multiple_cmc_exceedances = paste0(max(end_year)-3, " - ", max(end_year)))


  df_summary_d <- df %>%
    dplyr::mutate(start_year = lubridate::year(start_date),
                  end_year = lubridate::year(end_date)) %>%
    dplyr::filter(exceedance_d_3yr_sum > 1) %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(most_recent_period_with_multiple_d_exceedances = paste0(max(end_year)-3, " - ", max(end_year)))


  df_summary <- dplyr::full_join(df_summary_ccc, df_summary_cmc)

  df_summary <- dplyr::full_join(df_summary, df_summary_d)

}
