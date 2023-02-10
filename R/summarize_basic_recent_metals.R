#' summarize_basic_recent_metals
#'
#' @param df
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
summarize_basic_recent_metals <- function(criteria_results,
                                   start_date,
                                   end_date) {



  df <- criteria_results %>%
    dplyr::filter(pollutant_group %in% basic_metals)

  start_date <- as.Date(start_date, format = "%m/%d/%Y")
  end_date <- as.Date(end_date, format = "%m/%d/%Y")


  df_summary <- df %>%
    dplyr::filter(dplyr::between(sample_date, start_date, end_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::summarize(n_samples_recent = dplyr::n(),
                     most_recent_sample_recent = max(year),
                     n_detects_recent = sum(processed_detect_status == "D"),
                     n_ccc_exceedance_recent = sum(exceedance_ccc),
                     n_cmc_exceedance_recent = sum(exceedance_cmc),
                     n_d_exceedance_recent = sum(exceedance_d))



  df_summary <- df %>%
    dplyr::filter(dplyr::between(sample_date, start_date, end_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::filter(processed_detect_status == "D") %>%
    dplyr::summarize(most_recent_detect_recent = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::mutate(most_recent_detect_recent = tidyr::replace_na(most_recent_detect_recent, "Never")) %>%
    dplyr::relocate(waterbody_segment, pollutant_group, n_samples_recent, most_recent_sample_recent, n_detects_recent)


  return(df_summary)


}
