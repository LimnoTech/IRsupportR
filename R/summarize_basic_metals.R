#' summarize_basic
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
summarize_basic_metals <- function(criteria_results) {


  ######### Group Data ##############

  df <- criteria_results %>%
    dplyr::filter(pollutant_group %in% metals)

  # very basic summary

  df_summary_metals <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::summarize(n_samples = dplyr::n(),
                     most_recent_sample = max(year),
                     n_detects = sum(processed_detect_status == "D"),
                     n_ccc_exceedance = sum(exceedance_ccc),
                     n_cmc_exceedance = sum(exceedance_cmc),
                     n_d_exceedance = sum(exceedance_d)) %>%
    dplyr::mutate(n_exc_ccc_or_cmc = paste0(n_ccc_exceedance, " (CCC) ", n_cmc_exceedance, " (CMC)"))

  # most recent detect

  df_summary_metals <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::filter(processed_detect_status == "D") %>%
    dplyr::summarize(most_recent_detect = as.character(max(year))) %>%
    dplyr::right_join(df_summary_metals) %>%
    dplyr::mutate(most_recent_detect = tidyr::replace_na(most_recent_detect, "never")) %>%
    dplyr::relocate(waterbody_segment, pollutant_group, n_samples, most_recent_sample, n_detects)

  # most recent CCC

  df_summary_metals <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::filter(exceedance_ccc > 0) %>%
    dplyr::summarize(most_recent_ccc_exceedance_date = max(sample_date),
                     most_recent_ccc_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary_metals) %>%
    dplyr::relocate(most_recent_ccc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_ccc_exceedance_year, .after = dplyr::last_col())

  # most recent CMC

  df_summary_metals <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::filter(exceedance_cmc > 0) %>%
    dplyr::summarize(most_recent_cmc_exceedance_date = max(sample_date),
                     most_recent_cmc_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary_metals) %>%
    dplyr::relocate(most_recent_cmc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_cmc_exceedance_year, .after = dplyr::last_col())

  # most recent D

  df_summary_metals <- df %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::filter(exceedance_d > 0) %>%
    dplyr::summarize(most_recent_d_exceedance_date = max(sample_date),
                     most_recent_d_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary_metals) %>%
    dplyr::relocate(most_recent_d_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_d_exceedance_year, .after = dplyr::last_col())


  # report formatting most recent ccc / cmc / d:
  df_summary_metals <- df_summary_metals %>%
    dplyr::mutate(most_recent_class_c_exceedance = paste0(most_recent_ccc_exceedance_year, " (CCC) ", most_recent_cmc_exceedance_year, " (CMC)")) %>%
    dplyr::mutate(most_recent_class_d_exceedance = most_recent_d_exceedance_year)


  # n samples since last ccc / cmc / d
  df_summary_metals <- df %>%
    dplyr::left_join(df_summary_metals %>% dplyr::select(waterbody_segment, pollutant_group, test_fraction, most_recent_ccc_exceedance_date, most_recent_cmc_exceedance_date, most_recent_d_exceedance_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, test_fraction) %>%
    dplyr::summarize(n_since_most_recent_ccc_exceedance = sum(sample_date > most_recent_ccc_exceedance_date),
                     n_since_most_recent_cmc_exceedance = sum(sample_date > most_recent_cmc_exceedance_date),
                     n_since_most_recent_d_exceedance = sum(sample_date > most_recent_d_exceedance_date)) %>%
    dplyr::right_join(df_summary_metals) %>%
    dplyr::relocate(n_since_most_recent_ccc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_cmc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_d_exceedance, .after = dplyr::last_col())




  return(df_summary_metals)


}
