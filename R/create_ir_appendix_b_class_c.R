#' Create Class C Results Table
#'
#' Format table for inclusion in report appendices.
#'
#' @param my_decision_logic obtained from
#'   \code{\link{create_decision_logic_class_c}}.
#' @param five_year_start_date beginning of date range for last five year
#'  analysis in "mm/dd/yyyy" format.
#'
#' @return dataframe with formatting adjusted and columns renamed.
#' @export
#'
#' @examples
#' try( create_ir_appendix_b_class_c(my_decision_logic_class_c,
#'                                   five_year_start_date = "07/01/2016") )

create_ir_appendix_b_class_c <- function (my_decision_logic, five_year_start_date) {


  five_year_start_date <- as.POSIXct(five_year_start_date, format = "%m/%d/%Y")


  df <- my_decision_logic %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "DISSOLVED") %>% # Keep only dissolved metals. Remove metals where test fraction is TOTAL or NA
    dplyr::left_join(tidal_type, by = "waterbody_segment")


  df <- df %>%
    dplyr::mutate(was_last_c_exceedance_within_5_year_period = dplyr::case_when(most_recent_c_exceedance_date >= five_year_start_date ~ "Yes",
                                                                                most_recent_c_exceedance_date < five_year_start_date ~ "No")) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(number_of_unique_days_sampled = n_sample_dates,
                  number_of_samples_1990_to_2021 = n_samples,
                  number_of_samples_2011_to_2021 = n_samples_2011_to_2021,
                  number_of_samples_2016_to_2021 = n_samples_2016_to_2021,
                  number_of_detects_1990_to_2021 = n_detects,
                  n_c_exceedance_1990_to_2021 = n_c_exceedance,
                  number_of_samples_since_last_class_c_exceedance = n_since_most_recent_c_exceedance) %>%
    dplyr::mutate(number_of_samples_1990_to_2021 = tidyr::replace_na(number_of_samples_1990_to_2021, "0"),
                  number_of_samples_2011_to_2021 = tidyr::replace_na(number_of_samples_2011_to_2021, "0"),
                  number_of_samples_2016_to_2021 = tidyr::replace_na(number_of_samples_2016_to_2021, "0"),
                  number_of_detects_1990_to_2021 = tidyr::replace_na(number_of_detects_1990_to_2021, "0"),
                  number_of_unique_days_sampled = tidyr::replace_na(number_of_unique_days_sampled, "0"),
                  n_c_exceedance_1990_to_2021 = tidyr::replace_na(n_c_exceedance_1990_to_2021, "0"),
                  number_of_samples_since_last_class_c_exceedance = tidyr::replace_na(number_of_samples_since_last_class_c_exceedance, "No exceedance"),
                  was_last_c_exceedance_within_5_year_period = tidyr::replace_na(was_last_c_exceedance_within_5_year_period, "No exceedance")) %>%
    dplyr::mutate(n_c_exceedance_1990_to_2021 = dplyr::case_when(ccc_criterion == "no criteria" & cmc_criterion == "no criteria" ~ "N/A - no Class C WQ criteria",
                                                                 TRUE ~ n_c_exceedance_1990_to_2021),
                  n_c_exceedance_2011_to_2021 = dplyr::case_when(ccc_criterion == "no criteria" & cmc_criterion == "no criteria" ~ "N/A - no Class C WQ criteria",
                                                                 TRUE ~ n_c_exceedance_2011_to_2021),) %>%
    dplyr::select(waterbody_segment,
                  pollutant_name,
                  pollutant_group,
                  test_fraction,
                  current_category,
                  number_of_unique_days_sampled,
                  number_of_samples_1990_to_2021,
                  number_of_detects_1990_to_2021,
                  n_c_exceedance_1990_to_2021,
                  number_of_samples_2011_to_2021,
                  n_c_exceedance_2011_to_2021,
                  number_of_samples_2016_to_2021,
                  number_of_samples_since_last_class_c_exceedance,
                  ccc_criterion,
                  ccc_dl_ratio_range,
                  cmc_criterion,
                  cmc_dl_ratio_range,
                  was_last_c_exceedance_within_5_year_period,
                  c_decision_description,
                  c_decision_case_number)

  df <- df %>%
    dplyr::rename("Waterbody" = waterbody_segment,
                  "Pollutant" = pollutant_name,
                  "Pollutant Group" = pollutant_group,
                  "Test Fraction " = test_fraction,
                  "Current Categorization in 2020 IR" = current_category,
                  "Impaired Use Class in 2020 IR" = number_of_unique_days_sampled,
                  "# Samples 1990-2021" = number_of_samples_1990_to_2021,
                  "# Detects 1990-2021" = number_of_detects_1990_to_2021,
                  "# Exceedances 1990-2021" = n_c_exceedance_1990_to_2021,
                  "# of Samples Within Last 10 Years (2011-2021)" = number_of_samples_2011_to_2021,
                  "# of Exceedances Within Last 10 Years (2011 - 2021)" = n_c_exceedance_2011_to_2021,
                  "# of Samples Within Last 5 Years (2016 - 2021)" = number_of_samples_2016_to_2021,
                  "# Samples Since Last Exceedance" = number_of_samples_since_last_class_c_exceedance,
                  "Class C CCC Criterion (ug/L)" = ccc_criterion,
                  "Range of Ratios Between Detection Limit and Criterion for CCC" = ccc_dl_ratio_range,
                  "Class C CMC Criterion (ug/L)" = cmc_criterion,
                  "Range of Ratios Between Detection Limit and Criterion for CMC" = cmc_dl_ratio_range,
                  "Was Last Exceedance Within Current 5-year Assessment Period (2016 - 2021)?" = was_last_c_exceedance_within_5_year_period,
                  "Reevaluation Categorization Decision for Class C" = c_decision_description,
                  "Decision Logic Case #" = c_decision_case_number)





  return(df)


}
