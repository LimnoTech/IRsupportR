#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
create_ir_appendix_b_class_d <- function(my_decision_logic, five_year_start_date) {


  five_year_start_date <- as.POSIXct(five_year_start_date, format = "%m/%d/%Y")


  df <- my_decision_logic %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "TOTAL") %>% # Keep only total metals. Remove metals where test fraction is DISSOLVED or NA
    dplyr::left_join(decisions, by = c("current_category", "decision_case_number")) %>%
    dplyr::left_join(tidal_type, by = "waterbody_segment")


  df <- df %>%
    dplyr::mutate(was_last_d_exceedance_within_5_year_period = dplyr::case_when(most_recent_d_exceedance_date >= five_year_start_date ~ "Yes",
                                                                                most_recent_d_exceedance_date < five_year_start_date ~ "No")) %>%
    dplyr::mutate(n_d_exceedance = tidyr::replace_na(n_d_exceedance, 0),
                  n_since_most_recent_d_exceedance = tidyr::replace_na(n_since_most_recent_d_exceedance, 0)) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(most_recent_d_exceedance_year = tidyr::replace_na(most_recent_d_exceedance_year, "Never")) %>%
    dplyr::mutate(number_of_unique_days_sampled = n_sample_dates,
                  number_of_samples_1990_to_2021 = n_samples,
                  number_of_samples_2011_to_2021 = n_samples_2011_to_2021,
                  number_of_samples_2016_to_2021 = n_samples_2016_to_2021,
                  number_of_detects_1990_to_2021 = n_detects,
                  n_d_exceedance_1990_to_2021 = paste0(n_d_exceedance, " (D)"),
                  number_of_samples_since_last_class_d_exceedance = paste0(n_since_most_recent_d_exceedance, " (D)")) %>%
    dplyr::mutate(number_of_samples_1990_to_2021 = tidyr::replace_na(number_of_samples_1990_to_2021, "0"),
                  number_of_samples_2011_to_2021 = tidyr::replace_na(number_of_samples_2011_to_2021, "0"),
                  number_of_samples_2016_to_2021 = tidyr::replace_na(number_of_samples_2016_to_2021, "0"),
                  number_of_detects_1990_to_2021 = tidyr::replace_na(number_of_detects_1990_to_2021, "0"),
                  n_d_exceedance_1990_to_2021 = tidyr::replace_na(n_d_exceedance_1990_to_2021, "0 (D)"),
                  number_of_samples_since_last_class_d_exceedance = tidyr::replace_na(number_of_samples_since_last_class_d_exceedance, "0 (D)")) %>%
    dplyr::select(waterbody_segment,
                  pollutant_name,
                  pollutant_group,
                  test_fraction,
                  current_category,
                  number_of_unique_days_sampled,
                  number_of_samples_1990_to_2021,
                  number_of_detects_1990_to_2021,
                  n_d_exceedance_1990_to_2021,
                  number_of_samples_2011_to_2021,
                  n_d_exceedance_2011_to_2021,
                  number_of_samples_2016_to_2021,
                  n_d_exceedance_2016_to_2021,
                  number_of_samples_since_last_class_d_exceedance,
                  d_criterion,
                  d_dl_ratio_range,
                  was_last_d_exceedance_within_5_year_period,
                  tidal_type,
                  decision_description,
                  decision_case_number)

  return(df)

}
