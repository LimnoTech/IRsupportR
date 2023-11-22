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
#' try( create_results_class_c(my_decision_logic_class_c,
#'                             five_year_start_date = "07/01/2016") )

create_results_class_c <- function (my_decision_logic,
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
  n_c_exceedance_10yr <- paste0("n_c_exceedance_", ten_year_start_year, "_to_", ten_year_end_year)
  n_c_exceedance_5yr <- paste0("n_c_exceedance_", five_year_start_year, "_to_", five_year_end_year)
  n_samples_10yr <- paste0("n_samples_", ten_year_start_year, "_to_", ten_year_end_year)
  n_samples_5yr <- paste0("n_samples_", five_year_start_year, "_to_", five_year_end_year)


  df <- my_decision_logic %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "DISSOLVED") %>% # Keep only dissolved metals. Remove metals where test fraction is TOTAL or NA
    dplyr::left_join(tidal_type, by = "waterbody_segment")


  df <- df %>%
    dplyr::mutate(was_last_c_exceedance_within_5_year_period = dplyr::case_when(most_recent_c_exceedance_date >= five_year_start_date ~ "Yes",
                                                                                most_recent_c_exceedance_date < five_year_start_date ~ "No")) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(n_samples = tidyr::replace_na(n_samples, "0"),
                  # n_samples_2011_to_2021 = tidyr::replace_na(n_samples_2011_to_2021, "0"),
                  # n_samples_2016_to_2021 = tidyr::replace_na(n_samples_2016_to_2021, "0"),
                  n_detects = tidyr::replace_na(n_detects, "0"),
                  n_sample_dates = tidyr::replace_na(n_sample_dates, "0"),
                  n_c_exceedance = tidyr::replace_na(n_c_exceedance, "0"),
                  n_since_most_recent_c_exceedance = tidyr::replace_na(n_since_most_recent_c_exceedance, "No exceedance"),
                  was_last_c_exceedance_within_5_year_period = tidyr::replace_na(was_last_c_exceedance_within_5_year_period, "No exceedance")) %>%
    dplyr::mutate(n_c_exceedance = dplyr::case_when(ccc_criterion == "no criteria" & cmc_criterion == "no criteria" ~ "N/A - no Class C WQ criteria",
                                                                 TRUE ~ n_c_exceedance))

  for (i in 1:nrow(df)) {
    if (df$ccc_criterion[i] == "no criteria" & df$cmc_criterion[i] == "no criteria"){
      df[i, n_c_exceedance_10yr] = "N/A - no Class C WQ criteria"
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
            'n_c_exceedance',
            n_samples_10yr,
            n_c_exceedance_10yr,
            n_samples_5yr,
            'n_since_most_recent_c_exceedance',
            'ccc_criterion',
            'ccc_dl_ratio_range',
            'cmc_criterion',
            'cmc_dl_ratio_range',
            'was_last_c_exceedance_within_5_year_period',
            'c_decision_description',
            'c_decision_case_number')

  df <- df %>%
    dplyr::select(dplyr::one_of(cols))


  rename_n_samples_10yr <- paste("# of Samples Within Last 10 Years (", range_10yr, ")")
  rename_n_c_exceedance_10yr <- paste("# of Exceedances Within Last 10 Years (", range_10yr, ")")
  rename_n_samples_5yr <- paste("# of Samples Within Last 5 Years (", range_5yr, ")")


  df <- df %>%
    dplyr::rename("Waterbody" = waterbody_segment,
                  "Pollutant" = pollutant_name,
                  "Pollutant Group" = pollutant_group,
                  "Test Fraction " = test_fraction,
                  "Current Categorization in 2020 IR" = current_category,
                  "Impaired Use Class in 2020 IR" = n_sample_dates,
                  "# Samples 1990-2021" = n_samples,
                  "# Detects 1990-2021" = n_detects,
                  "# Exceedances 1990-2021" = n_c_exceedance,
                  !!rename_n_samples_10yr := n_samples_10yr,
                  !!rename_n_c_exceedance_10yr := n_c_exceedance_10yr,
                  !!rename_n_samples_5yr := n_samples_5yr,
                  "# Samples Since Last Exceedance" = n_since_most_recent_c_exceedance,
                  "Class C CCC Criterion (ug/L)" = ccc_criterion,
                  "Range of Ratios Between Detection Limit and Criterion for CCC" = ccc_dl_ratio_range,
                  "Class C CMC Criterion (ug/L)" = cmc_criterion,
                  "Range of Ratios Between Detection Limit and Criterion for CMC" = cmc_dl_ratio_range,
                  "Was Last Exceedance Within Current 5-year Assessment Period (2016 - 2021)?" = was_last_c_exceedance_within_5_year_period,
                  "Reevaluation Categorization Decision for Class C" = c_decision_description,
                  "Decision Logic Case #" = c_decision_case_number)





  return(df)


}
