#' Create Class D Results Table
#'
#' Format table for inclusion in report appendices.
#'
#' @param my_decision_logic obtained from
#'   \code{\link{create_decision_logic_class_d}}.
#' @param five_year_start_date beginning of date range for last five year
#'  analysis in "mm/dd/yyyy" format.
#'
#' @return dataframe with formatting adjusted and columns renamed.
#' @export
#'
#' @examples
#' try( create_results_class_c(my_decision_logic_class_d,
#'                             five_year_start_date = "07/01/2018",
#'                             five_year_end_date <- "06/30/2023",
#'                             ten_year_start_date <- "07/01/2013",
#'                             ten_year_end_date <- "06/30/2023")

create_results_class_d <- function(my_decision_logic,
                                   five_year_start_date,
                                   five_year_end_date,
                                   ten_year_start_date,
                                   ten_year_end_date) {


  five_year_start_date <- as.POSIXct(five_year_start_date, format = "%m/%d/%Y")

  five_year_start_year <- format(as.Date(five_year_start_date, format="%m/%d/%Y"), "%Y")
  five_year_end_year <- format(as.Date(five_year_end_date, format="%m/%d/%Y"), "%Y")
  ten_year_start_year <- format(as.Date(ten_year_start_date, format="%m/%d/%Y"), "%Y")
  ten_year_end_year <- format(as.Date(ten_year_end_date, format="%m/%d/%Y"), "%Y")

  range_10yr <- paste0(ten_year_start_year, "-", ten_year_end_year)
  range_5yr <- paste0(five_year_start_year, "-", five_year_end_year)
  n_d_exceedance_10yr <- paste0("n_d_exceedance_", ten_year_start_year, "_to_", ten_year_end_year)
  n_d_exceedance_5yr <- paste0("n_d_exceedance_", five_year_start_year, "_to_", five_year_end_year)
  n_samples_10yr <- paste0("n_samples_", ten_year_start_year, "_to_", ten_year_end_year)
  n_samples_5yr <- paste0("n_samples_", five_year_start_year, "_to_", five_year_end_year)


  df <- my_decision_logic %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "TOTAL") %>% # Keep only total metals. Remove metals where test fraction is DISSOLVED or NA
    dplyr::left_join(tidal_type, by = "waterbody_segment")


  df <- df %>%
    dplyr::mutate(was_last_d_exceedance_within_5_year_period = dplyr::case_when(most_recent_d_exceedance_date >= five_year_start_date ~ "Yes",
                                                                                most_recent_d_exceedance_date < five_year_start_date ~ "No")) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(n_samples = tidyr::replace_na(n_samples, "0"),
                  # n_samples_2011_to_2021 = tidyr::replace_na(n_samples_2011_to_2021, "0"),
                  # n_samples_2016_to_2021 = tidyr::replace_na(n_samples_2016_to_2021, "0"),
                  n_detects = tidyr::replace_na(n_detects, "0"),
                  n_sample_dates = tidyr::replace_na(n_sample_dates, "0"),
                  n_d_exceedance = tidyr::replace_na(n_d_exceedance, "0"),
                  n_since_most_recent_d_exceedance = tidyr::replace_na(n_since_most_recent_d_exceedance, "No exceedance"),
                  was_last_d_exceedance_within_5_year_period = tidyr::replace_na(was_last_d_exceedance_within_5_year_period, "No exceedance")) %>%
    dplyr::mutate(n_d_exceedance = dplyr::case_when(d_criterion == "no criteria" ~ "N/A - no Class D WQ criteria",
                                                                 TRUE ~ n_d_exceedance))

  for (i in 1:nrow(df)) {
    if (df$d_criterion[i] == "no criteria"){
      df[i, n_d_exceedance_10yr] = "N/A - no Class D WQ criteria"
    }
  }


  cols <- c('waterbody_segment',
            'pollutant_name',
            'pollutant_group',
            'test_fraction',
            'current_category',
            'n_sample_dates',
            'n_samples',
            'n_detects',
            'n_d_exceedance',
            n_samples_10yr,
            n_d_exceedance_10yr,
            n_samples_5yr,
            'n_since_most_recent_d_exceedance',
            'd_criterion',
            'd_dl_ratio_range',
            'was_last_d_exceedance_within_5_year_period',
            'fish_tissue_results_class_d',
            'd_decision_description',
            'd_decision_case_number')

  df <- df %>%
    dplyr::select(dplyr::one_of(cols))


  rename_n_samples_10yr <- paste0("# of Samples Within Last 10 Years (", range_10yr, ")")
  rename_n_d_exceedance_10yr <- paste0("# of Exceedances Within Last 10 Years (", range_10yr, ")")
  rename_n_samples_5yr <- paste0("# of Samples Within Last 5 Years (", range_5yr, ")")


  df <- df %>%
    dplyr::rename("Waterbody" = waterbody_segment,
                  "Pollutant" = pollutant_name,
                  "Pollutant Group" = pollutant_group,
                  "Test Fraction " = test_fraction,
                  "Current Categorization in 2020 IR" = current_category,
                  "Impaired Use Class in 2020 IR" = n_sample_dates,
                  "# Samples" = n_samples,
                  "# Detects" = n_detects,
                  "# Exceedances" = n_d_exceedance,
                  !!rename_n_samples_10yr := n_samples_10yr,
                  !!rename_n_d_exceedance_10yr := n_d_exceedance_10yr,
                  !!rename_n_samples_5yr := n_samples_5yr,
                  "# Samples Since Last Exceedance" = n_since_most_recent_d_exceedance,
                  "Criterion (ug/L)" = d_criterion,
                  "Range of Ratios Between Detection Limit and Criterion" = d_dl_ratio_range,
                  "Was Last Exceedance Within Current 5-year Assessment Period?" = was_last_d_exceedance_within_5_year_period,
                  "Fish Tissue Results" = fish_tissue_results_class_d,
                  "Reevaluation Categorization Decision for Class D" = d_decision_description,
                  "Decision Logic Case #" = d_decision_case_number)

  return(df)

}
