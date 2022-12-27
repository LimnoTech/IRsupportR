#' Title
#'
#' @param df
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
summarize_basic_recent <- function(criteria_results,
                                   start_date,
                                   end_date) {




  ######### Group Data ##############

  df <- criteria_results

  start_date <- as.Date(start_date, format = "%m/%d/%Y")
  end_date <- as.Date(end_date, format = "%m/%d/%Y")


  df_summary <- df %>%
    dplyr::filter(between(date, start_date, end_date)) %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::summarize(n_samples_recent = n(),
                     most_recent_sample_recent = max(year),
                     n_detects_recent = sum(detection == "d"),
                     n_suspected_detects_recent = sum(detection == "d" & suspected_nd == FALSE),
                     n_ccc_exceedance_recent = sum(evidence_based_exceedance_ccc),
                     n_cmc_exceedance_recent = sum(evidence_based_exceedance_cmc),
                     n_d_exceedance_recent = sum(evidence_based_exceedance_d))



  df_summary <- df %>%
    dplyr::filter(between(date, start_date, end_date)) %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::filter(detection == "d" & suspected_nd == FALSE) %>%
    dplyr::summarize(most_recent_detect_recent = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::mutate(most_recent_detect_recent = tidyr::replace_na(most_recent_detect_recent, "never")) %>%
    dplyr::relocate(site_summary_segment, group_lower, n_samples_recent, most_recent_sample_recent, n_detects_recent)


  return(df_summary)


}
