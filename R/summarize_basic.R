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

  df <- criteria_results %>%
    dplyr::filter(pollutant_name %in% basic_other)

  # very basic summary

  df_summary <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(n_samples = dplyr::n(),
              most_recent_sample_year = max(year),
              n_detects = sum(processed_detect_status == "D"),
              n_ccc_exceedance = sum(exceedance_ccc),
              n_cmc_exceedance = sum(exceedance_cmc),
              n_d_exceedance = sum(exceedance_d)) %>%
    dplyr::mutate(n_exc_ccc_or_cmc = paste0(n_ccc_exceedance, " (CCC) ", n_cmc_exceedance, " (CMC)"))

  # most recent detect

  df_summary <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::filter(processed_detect_status == "D") %>%
    dplyr::summarize(most_recent_detect_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::mutate(most_recent_detect_year = tidyr::replace_na(most_recent_detect_year, "Never")) %>%
    dplyr::relocate(waterbody_segment, pollutant_group, n_samples, most_recent_sample_year, n_detects)

  # most recent CCC

  df_summary <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::filter(exceedance_ccc > 0) %>%
    dplyr::summarize(most_recent_ccc_exceedance_date = max(sample_date),
                     most_recent_ccc_exceedance_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_ccc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_ccc_exceedance_year, .after = dplyr::last_col())

  # most recent CMC

  df_summary <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::filter(exceedance_cmc > 0) %>%
    dplyr::summarize(most_recent_cmc_exceedance_date = max(sample_date),
                     most_recent_cmc_exceedance_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_cmc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_cmc_exceedance_year, .after = dplyr::last_col())

  # most recent D

  df_summary <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::filter(exceedance_d > 0) %>%
    dplyr::summarize(most_recent_d_exceedance_date = max(sample_date),
                     most_recent_d_exceedance_year = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_d_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_d_exceedance_year, .after = dplyr::last_col())


  # report formatting most recent ccc / cmc / d:
  df_summary <- df_summary %>%
    dplyr::mutate(most_recent_ccc_exceedance_year = tidyr::replace_na(most_recent_ccc_exceedance_year, "Never"),
                  most_recent_cmc_exceedance_year = tidyr::replace_na(most_recent_cmc_exceedance_year, "Never"),
                  most_recent_d_exceedance_year = tidyr::replace_na(most_recent_d_exceedance_year, "Never")) %>%
    dplyr::mutate(most_recent_class_c_exceedance = paste0(most_recent_ccc_exceedance_year, " (CCC) ", most_recent_cmc_exceedance_year, " (CMC)")) %>%
    dplyr::mutate(most_recent_class_d_exceedance = paste0(most_recent_d_exceedance_year, " (D)"))


  # n samples since last ccc / cmc / d
  df_summary <- df %>%
    dplyr::left_join(df_summary %>% dplyr::select(waterbody_segment, pollutant_group, most_recent_ccc_exceedance_date, most_recent_cmc_exceedance_date, most_recent_d_exceedance_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(n_since_most_recent_ccc_exceedance = sum(sample_date > most_recent_ccc_exceedance_date),
                     n_since_most_recent_cmc_exceedance = sum(sample_date > most_recent_cmc_exceedance_date),
                     n_since_most_recent_d_exceedance = sum(sample_date > most_recent_d_exceedance_date)) %>%
    dplyr::mutate(n_since_most_recent_ccc_exceedance = tidyr::replace_na(n_since_most_recent_ccc_exceedance, 0),
                  n_since_most_recent_cmc_exceedance = tidyr::replace_na(n_since_most_recent_cmc_exceedance, 0),
                  n_since_most_recent_d_exceedance = tidyr::replace_na(n_since_most_recent_d_exceedance, 0)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(n_since_most_recent_ccc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_cmc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_d_exceedance, .after = dplyr::last_col())

  # # Add empty column for test_fraction
  # df_summary <- df_summary %>%
  #   dplyr::mutate(test_fraction = as.character(NA))




  return(df_summary)


}
