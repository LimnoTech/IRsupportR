#' summarize_basic_recent_pah
#'
#' @param df
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
summarize_basic_recent_pah <- function(criteria_results,
                                   start_date,
                                   end_date) {




  ######### Group Data ##############

  df <- criteria_results %>%
    dplyr::filter(pollutant_group %in% baSic_pahs)

  start_date <- as.Date(start_date, format = "%m/%d/%Y")
  end_date <- as.Date(end_date, format = "%m/%d/%Y")


  df_summary_pah_samples <- df %>%
    dplyr::filter(dplyr::between(sample_date, start_date, end_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::summarize(n_samples_recent = dplyr::n(),
                     most_recent_sample_recent = max(year),
                     n_detects_recent = sum(processed_detect_status == "D"),
                     n_ccc_exceedance_recent = sum(exceedance_ccc),
                     n_cmc_exceedance_recent = sum(exceedance_cmc),
                     n_d_exceedance_recent = sum(exceedance_d))



  df_summary_pah_samples <- df %>%
    dplyr::filter(dplyr::between(sample_date, start_date, end_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
    dplyr::filter(processed_detect_status == "D") %>%
    dplyr::summarize(most_recent_detect_recent = max(year)) %>%
    dplyr::right_join(df_summary_pah_samples) %>%
    dplyr::relocate(waterbody_segment, pollutant_group, n_samples_recent, most_recent_sample_recent, n_detects_recent)



  # Roll up to Group Level

  df_summary_pah <- df_summary_pah_samples %>%
    dplyr::group_by(waterbody_segment, pollutant_group) %>%
    dplyr::summarize(n_samples_recent = sum(n_samples_recent > 0),
                     most_recent_sample_recent = max(most_recent_sample_recent, na.rm = TRUE),
                     n_detects_recent = sum(n_detects_recent > 0),
                     most_recent_detect_recent = as.character(max(most_recent_detect_recent, na.rm = TRUE)),
                     n_ccc_exceedance_recent = as.numeric(sum(n_ccc_exceedance_recent > 0)),
                     n_cmc_exceedance_recent = as.numeric(sum(n_cmc_exceedance_recent > 0)),
                     n_d_exceedance_recent = as.numeric(sum(n_d_exceedance_recent > 0))) %>%
    dplyr::mutate(most_recent_detect_recent = stringr::str_replace_all(most_recent_detect_recent, "-Inf", "Never"))




  return(df_summary_pah)


}
