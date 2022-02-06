#' summarize_basic
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
summarize_basic <- function(criteria_results, ...) {


  ######### Capture expressions #######

  dots <- ensyms(...)

  ######### Group Data ##############

  df <- criteria_results

  # very basic summary

  df_summary <- df %>%
    dplyr::group_by_at(vars(!!!dots)) %>%
    dplyr::summarize(n_samples = n(),
              most_recent_sample = max(year),
              n_detects = sum(detection == "d"),
              n_suspected_detects = sum(detection == "d" & suspected_nd == FALSE),
              n_ccc_exceedance = sum(evidence_based_exceedance_ccc),
              n_cmc_exceedance = sum(evidence_based_exceedance_cmc),
              n_d_exceedance = sum(evidence_based_exceedance_d)) %>%
    dplyr::mutate(n_exc_ccc_or_cmc = paste0(n_ccc_exceedance, " (CCC) ", n_cmc_exceedance, " (CMC)"))

  # most recent detect

  df_summary <- df %>%
    dplyr::group_by_at(vars(!!!dots)) %>%
    dplyr::filter(detection == "d" & suspected_nd == FALSE) %>%
    dplyr::summarize(most_recent_detect = max(year)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::mutate(most_recent_detect = tidyr::replace_na(most_recent_detect, "never")) %>%
    dplyr::relocate(site_summary_segment, group_lower, n_samples, most_recent_sample, n_detects)

  # most recent CCC

  df_summary <- df %>%
    dplyr::group_by_at(vars(!!!dots)) %>%
    dplyr::filter(evidence_based_exceedance_ccc > 0) %>%
    dplyr::summarize(most_recent_ccc_exceedance_date = max(date),
                     most_recent_ccc_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_ccc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_ccc_exceedance_year, .after = dplyr::last_col())

  # most recent CMC

  df_summary <- df %>%
    dplyr::group_by_at(vars(!!!dots)) %>%
    dplyr::filter(evidence_based_exceedance_cmc > 0) %>%
    dplyr::summarize(most_recent_cmc_exceedance_date = max(date),
                     most_recent_cmc_exceedance_year = max(year)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(most_recent_cmc_exceedance_date, .after = dplyr::last_col()) %>%
    dplyr::relocate(most_recent_cmc_exceedance_year, .after = dplyr::last_col())


  # report formatting most recent ccc and cmc:
  df_summary <- df_summary %>%
    dplyr::mutate(most_recent_class_c_exceedance = paste0(most_recent_ccc_exceedance_year, " (CCC) ", most_recent_cmc_exceedance_year, " (CMC)"))


  # n samples since last ccc
  df_summary <- df %>%
    dplyr::left_join(df_summary %>% select(site_summary_segment, group_lower, most_recent_ccc_exceedance_date, most_recent_cmc_exceedance_date)) %>%
    dplyr::group_by_at(vars(!!!dots)) %>%
    dplyr::summarize(n_since_most_recent_ccc_exceedance = sum(date > most_recent_ccc_exceedance_date),
                     n_since_most_recent_cmc_exceedance = sum(date > most_recent_cmc_exceedance_date)) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::relocate(n_since_most_recent_ccc_exceedance, .after = dplyr::last_col()) %>%
    dplyr::relocate(n_since_most_recent_cmc_exceedance, .after = dplyr::last_col())


  return(df_summary)


}
