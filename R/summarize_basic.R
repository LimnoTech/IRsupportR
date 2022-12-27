#' summarize_basic
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
summarize_basic <- function(criteria_results) {


  ######### Group Data ##############

  df <- criteria_results

  # very basic summary

  df_summary <- df %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::summarize(n_samples = n(),
              most_recent_sample = max(year),
              n_detects = sum(detection == "d" & suspected_nd == FALSE), #consider whether we think the detection flag is incorrect
              n_ccc_exceedance = sum(evidence_based_exceedance_ccc),
              n_cmc_exceedance = sum(evidence_based_exceedance_cmc),
              n_d_exceedance = sum(evidence_based_exceedance_d)) %>%
    dplyr::mutate(n_exc_ccc_or_cmc = paste0(n_ccc_exceedance, " (CCC) ", n_cmc_exceedance, " (CMC)"))

  # most recent detect

  df_summary <- df %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::filter(detection == "d" & suspected_nd == FALSE) %>%
    dplyr::summarize(most_recent_detect = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::mutate(most_recent_detect = tidyr::replace_na(most_recent_detect, "never")) %>%
    dplyr::relocate(site_summary_segment, group_lower, n_samples, most_recent_sample, n_detects)

  # most recent CCC

  df_summary <- df %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::filter(evidence_based_exceedance_ccc > 0) %>%
    dplyr::summarize(most_recent_ccc_exceedance_date = max(date),
                     most_recent_ccc_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_ccc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_ccc_exceedance_year, .after = dplyr::last_col())

  # most recent CMC

  df_summary <- df %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::filter(evidence_based_exceedance_cmc > 0) %>%
    dplyr::summarize(most_recent_cmc_exceedance_date = max(date),
                     most_recent_cmc_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_cmc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_cmc_exceedance_year, .after = dplyr::last_col())

  # most recent D

  df_summary <- df %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::filter(evidence_based_exceedance_d > 0) %>%
    dplyr::summarize(most_recent_d_exceedance_date = max(date),
                     most_recent_d_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_d_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_d_exceedance_year, .after = dplyr::last_col())


  # report formatting most recent ccc / cmc / d:
  df_summary <- df_summary %>%
    dplyr::mutate(most_recent_class_c_exceedance = paste0(most_recent_ccc_exceedance_year, " (CCC) ", most_recent_cmc_exceedance_year, " (CMC)")) %>%
    dplyr::mutate(most_recent_class_d_exceedance = most_recent_d_exceedance_year)


  # n samples since last ccc / cmc / d
  df_summary <- df %>%
    dplyr::left_join(df_summary %>% select(site_summary_segment, group_lower, most_recent_ccc_exceedance_date, most_recent_cmc_exceedance_date, most_recent_d_exceedance_date)) %>%
    dplyr::group_by(site_summary_segment, group_lower) %>%
    dplyr::summarize(n_since_most_recent_ccc_exceedance = sum(date > most_recent_ccc_exceedance_date),
                     n_since_most_recent_cmc_exceedance = sum(date > most_recent_cmc_exceedance_date),
                     n_since_most_recent_d_exceedance = sum(date > most_recent_d_exceedance_date)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(n_since_most_recent_ccc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_cmc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_d_exceedance, .after = dplyr::last_col())




  return(df_summary)


}
