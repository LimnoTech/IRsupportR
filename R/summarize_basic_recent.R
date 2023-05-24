#' Basic Summary of Organic Parameters for Recent Timeframe
#'
#' Create basic summary statistics for all organic parameters within specified
#' timeframe.
#'
#' @param criteria_results obtained from \code{\link{evaluate_criteria}}.
#' @param start_date beginning of date range of interest in "mm/dd/yyyy" format.
#' @param end_date end of date range of interest in "mm/dd/yyyy" format.
#'
#' @return dataframe with added columns based on specified timeframe.
#' @export
#'
#' @examples
#' try( summarize_basic_recent(criteria_results = criteria_results,
#'                             start_date = "07/01/2011",
#'                             end_date = "06/30/2021") )

summarize_basic_recent <- function(criteria_results,
                                   start_date,
                                   end_date) {



  df <- criteria_results %>%
    dplyr::filter(pollutant_group %in% organics)

  start_date <- as.POSIXct(start_date, format = "%m/%d/%Y")
  end_date <- as.POSIXct(end_date, format = "%m/%d/%Y")


  df_summary <- df %>%
    dplyr::filter(dplyr::between(sample_date, start_date, end_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_name , pollutant_group) %>%
    dplyr::summarize(n_samples_recent = dplyr::n(),
                     most_recent_sample_recent = max(year),
                     n_detects_recent = sum(processed_detect_status == "D"),
                     n_ccc_exceedance_recent = sum(exceedance_ccc),
                     n_cmc_exceedance_recent = sum(exceedance_cmc),
                     n_d_exceedance_recent = sum(exceedance_d))



  df_summary <- df %>%
    dplyr::filter(dplyr::between(sample_date, start_date, end_date)) %>%
    dplyr::group_by(waterbody_segment, pollutant_name , pollutant_group) %>%
    dplyr::filter(processed_detect_status == "D") %>%
    dplyr::summarize(most_recent_detect_recent = as.character(max(year))) %>%
    dplyr::right_join(df_summary) %>%
    dplyr::mutate(most_recent_detect_recent = tidyr::replace_na(most_recent_detect_recent, "Never")) %>%
    dplyr::relocate(waterbody_segment, pollutant_name , pollutant_group, n_samples_recent, most_recent_sample_recent, n_detects_recent)



  # Rename variables with particular years
  start_year = format(start_date, "%Y")
  end_year = format(end_date, "%Y")


  names(df_summary)[names(df_summary) == 'n_samples_recent'] <- paste0("n_samples_", start_year, "_to_", end_year)
  names(df_summary)[names(df_summary) == 'most_recent_sample_recent'] <- paste0("most_recent_sample_", start_year, "_to_", end_year)
  names(df_summary)[names(df_summary) == 'n_detects_recent'] <- paste0("n_detects_", start_year, "_to_", end_year)
  names(df_summary)[names(df_summary) == 'n_ccc_exceedance_recent'] <- paste0("n_ccc_exceedance_", start_year, "_to_", end_year)
  names(df_summary)[names(df_summary) == 'n_cmc_exceedance_recent'] <- paste0("n_cmc_exceedance_", start_year, "_to_", end_year)
  names(df_summary)[names(df_summary) == 'n_d_exceedance_recent'] <- paste0("n_d_exceedance_", start_year, "_to_", end_year)
  names(df_summary)[names(df_summary) == 'most_recent_detect_recent'] <- paste0("most_recent_detect_", start_year, "_to_", end_year)




  return(df_summary)


}
