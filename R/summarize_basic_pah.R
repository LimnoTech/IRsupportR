#' summarize_basic_pah
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
summarize_basic_pah <- function(criteria_results) {



  df <- criteria_results %>%
    dplyr::filter(pollutant_group %in% basic_pahs)

  # very basic summary

  df_summary_pah_samples <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::summarize(n_results = dplyr::n(),
                     most_recent_result_date = max(sample_date),
                     most_recent_result_year = max(year),
                     n_detects = sum(processed_detect_status == "D"),
                     n_ccc_exceedance = sum(exceedance_ccc),
                     n_cmc_exceedance = sum(exceedance_cmc),
                     n_d_exceedance = sum(exceedance_d))
  # most recent detect

  df_summary_pah_samples <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::filter(processed_detect_status == "D") %>%
    dplyr::summarize(most_recent_detect_year = max(year)) %>%
    dplyr::right_join(df_summary_pah_samples) %>%
    dplyr::relocate(waterbody_segment, pollutant_group, sample_id, n_results, most_recent_result_date, most_recent_result_year, n_detects)

  # most recent CCC

  df_summary_pah_samples <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::filter(exceedance_ccc > 0) %>%
    dplyr::summarize(most_recent_ccc_exceedance_date = max(sample_date),
                     most_recent_ccc_exceedance_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary_pah_samples) %>%
    dplyr::relocate(most_recent_ccc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_ccc_exceedance_year, .after = dplyr::last_col())

  # most recent CMC

  df_summary_pah_samples <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::filter(exceedance_cmc > 0) %>%
    dplyr::summarize(most_recent_cmc_exceedance_date = max(sample_date),
                     most_recent_cmc_exceedance_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary_pah_samples) %>%
    dplyr::relocate(most_recent_cmc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_cmc_exceedance_year, .after = dplyr::last_col())

  # most recent D

  df_summary_pah_samples <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::filter(exceedance_d > 0) %>%
    dplyr::summarize(most_recent_d_exceedance_date = max(sample_date),
                     most_recent_d_exceedance_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary_pah_samples) %>%
    dplyr::relocate(most_recent_d_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_d_exceedance_year, .after = dplyr::last_col())



  # Roll up to Group Level
  df_summary_pah <- df_summary_pah_samples %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(n_samples = dplyr::n(),
                     most_recent_sample_year = max(most_recent_result_year),
                     n_detects = sum(n_detects > 0),
                     most_recent_detect_year = as.character(max(most_recent_detect_year, na.rm = TRUE)),
                     n_ccc_exceedance = sum(n_ccc_exceedance > 0),
                     n_cmc_exceedance = sum(n_cmc_exceedance > 0),
                     n_d_exceedance = sum(n_d_exceedance > 0),
                     most_recent_ccc_exceedance_date = max(most_recent_ccc_exceedance_date, na.rm = TRUE),
                     most_recent_ccc_exceedance_year = max(most_recent_ccc_exceedance_year, na.rm = TRUE),
                     most_recent_cmc_exceedance_date = max(most_recent_cmc_exceedance_date, na.rm = TRUE),
                     most_recent_cmc_exceedance_year = max(most_recent_cmc_exceedance_year, na.rm = TRUE),
                     most_recent_d_exceedance_date = max(most_recent_d_exceedance_date, na.rm = TRUE),
                     most_recent_d_exceedance_year = max(most_recent_d_exceedance_year, na.rm = TRUE),
                     n_since_most_recent_ccc_exceedance = sum(most_recent_result_date > max(most_recent_ccc_exceedance_date)),
                     n_since_most_recent_cmc_exceedance = sum(most_recent_result_date > max(most_recent_cmc_exceedance_date)),
                     n_since_most_recent_d_exceedance = sum(most_recent_result_date > max(most_recent_d_exceedance_date))) %>%
    dplyr::mutate(n_since_most_recent_ccc_exceedance = tidyr::replace_na(n_since_most_recent_ccc_exceedance, 0),
                  n_since_most_recent_cmc_exceedance = tidyr::replace_na(n_since_most_recent_cmc_exceedance, 0),
                  n_since_most_recent_d_exceedance = tidyr::replace_na(n_since_most_recent_d_exceedance, 0))

  # Correct counts of samples since most recent exceedance (should be zero) when there are no exceedances
  df_summary_pah <- df_summary_pah %>%
    dplyr::mutate(n_since_most_recent_ccc_exceedance = dplyr::case_when(n_ccc_exceedance == 0 ~ 0,
                                                                        TRUE ~ as.numeric(n_since_most_recent_ccc_exceedance)),
                  n_since_most_recent_cmc_exceedance = dplyr::case_when(n_cmc_exceedance == 0 ~ 0,
                                                                        TRUE ~ as.numeric(n_since_most_recent_cmc_exceedance)),
                  n_since_most_recent_d_exceedance = dplyr::case_when(n_d_exceedance == 0 ~ 0,
                                                                        TRUE ~ as.numeric(n_since_most_recent_d_exceedance)),)



  # Make formats consistent with other basic summary tables
  df_summary_pah <- df_summary_pah %>%
    dplyr::na_if(-Inf) %>%
    dplyr::mutate(most_recent_detect_year = tidyr::replace_na(most_recent_detect_year, "Never"),
                  most_recent_ccc_exceedance_year = tidyr::replace_na(most_recent_ccc_exceedance_year, "Never"),
                  most_recent_cmc_exceedance_year = tidyr::replace_na(most_recent_cmc_exceedance_year, "Never"),
                  most_recent_d_exceedance_year = tidyr::replace_na(most_recent_d_exceedance_year, "Never"),
                  n_since_most_recent_ccc_exceedance = as.integer(n_since_most_recent_ccc_exceedance),
                  n_since_most_recent_cmc_exceedance = as.integer(n_since_most_recent_cmc_exceedance),
                  n_since_most_recent_d_exceedance = as.integer(n_since_most_recent_d_exceedance),
                  n_ccc_exceedance = as.numeric(n_ccc_exceedance),
                  n_cmc_exceedance = as.numeric(n_cmc_exceedance),
                  n_d_exceedance = as.numeric(n_d_exceedance))





  return(df_summary_pah)


}
