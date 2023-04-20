#' create_ir_appendix_b_class_d
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
    dplyr::left_join(tidal_type, by = "waterbody_segment")


  df <- df %>%
    dplyr::mutate(was_last_d_exceedance_within_5_year_period = dplyr::case_when(most_recent_d_exceedance_date >= five_year_start_date ~ "Yes",
                                                                                most_recent_d_exceedance_date < five_year_start_date ~ "No")) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(number_of_unique_days_sampled = n_sample_dates,
                  number_of_samples_1990_to_2021 = n_samples,
                  number_of_samples_2011_to_2021 = n_samples_2011_to_2021,
                  number_of_samples_2016_to_2021 = n_samples_2016_to_2021,
                  number_of_detects_1990_to_2021 = n_detects,
                  n_d_exceedance_1990_to_2021 = n_d_exceedance,
                  number_of_samples_since_last_class_d_exceedance = n_since_most_recent_d_exceedance) %>%
    dplyr::mutate(number_of_samples_1990_to_2021 = tidyr::replace_na(number_of_samples_1990_to_2021, "0"),
                  number_of_samples_2011_to_2021 = tidyr::replace_na(number_of_samples_2011_to_2021, "0"),
                  number_of_samples_2016_to_2021 = tidyr::replace_na(number_of_samples_2016_to_2021, "0"),
                  number_of_detects_1990_to_2021 = tidyr::replace_na(number_of_detects_1990_to_2021, "0"),
                  number_of_unique_days_sampled = tidyr::replace_na(number_of_unique_days_sampled, "0"),
                  n_d_exceedance_1990_to_2021 = tidyr::replace_na(n_d_exceedance_1990_to_2021, "0"),
                  number_of_samples_since_last_class_d_exceedance = tidyr::replace_na(number_of_samples_since_last_class_d_exceedance, "No exceedance")) %>%
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
                  number_of_samples_since_last_class_d_exceedance,
                  d_criterion,
                  d_dl_ratio_range,
                  was_last_d_exceedance_within_5_year_period,
                  fish_tissue_results_class_d,
                  d_decision_description,
                  d_decision_case_number,
                  updated_tmdl)


  df <- df %>%
    dplyr::rename("Waterbody" = waterbody_segment,
                  "Pollutant" = pollutant_name,
                  "Pollutant Group" = pollutant_group,
                  "Test Fraction " = test_fraction,
                  "Current Categorization in 2020 IR" = current_category,
                  "Impaired Use Class in 2020 IR" = number_of_unique_days_sampled,
                  "# Samples 1990-2021" = number_of_samples_1990_to_2021,
                  "# Detects 1990-2021" = number_of_detects_1990_to_2021,
                  "# Exceedances 1990-2021" = n_d_exceedance_1990_to_2021,
                  "# of Samples Within Last 10 Years (2011-2021)" = number_of_samples_2011_to_2021,
                  "# of Exceedances Within Last 10 Years (2011 - 2021)" = n_d_exceedance_2011_to_2021,
                  "# of Samples Within Last 5 Years (2016 - 2021)" = number_of_samples_2016_to_2021,
                  "# Samples Since Last Exceed-ance" = number_of_samples_since_last_class_d_exceedance,
                  "Criterion (ug/L)" = d_criterion,
                  "Range of Ratios Between Detection Limit and Criterion" = d_dl_ratio_range,
                  "Was Last Exceedance Within Current 5-year Assessment Period (2016 - 2021)?" = was_last_d_exceedance_within_5_year_period,
                  "Fish Tissue Results" = fish_tissue_results_class_d,
                  "Reevaluation Categorization Decision for Class D" = d_decision_description,
                  "Decision Logic Case #" = d_decision_case_number,
                  "Updated TMDL?" = updated_tmdl)

  return(df)

}
