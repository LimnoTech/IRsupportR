#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
create_ir_appendix_b_class_d <- function(basic_summary,
                                         basic_summary_recent,
                                         formatted_period_results) {


  period <- formatted_period_results %>%
    dplyr::mutate(has_there_been_multi_class_d_exceedance_in_3yr = dplyr::case_when(is.na(most_recent_period_with_multiple_d_exceedances) == F ~ "Yes",
                                                                  TRUE ~ "No")) %>%
    dplyr::mutate(when_was_last_period_with_multi_class_d_exceedances = dplyr::case_when(is.na(most_recent_period_with_multiple_d_exceedances) == F ~ most_recent_period_with_multiple_d_exceedances,
                                             TRUE ~ "Never"))


  df_grid <- expand.grid(waterbody_segment = unique(basic_summary$waterbody_segment), pollutant_group = paramlevels$pollutant_group, test_fraction = c("TOTAL", "DISSOLVED", NA))

  df <- df_grid %>%
    dplyr::left_join(basic_summary) %>%
    dplyr::left_join(basic_summary_recent) %>%
    dplyr::left_join(period) %>%
    dplyr::filter(waterbody_segment != "") %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "TOTAL") %>% #Keep only total metals. Remove metals where test fraction is DISSOLVED or NA
    dplyr::filter(!pollutant_group %in% organics | is.na(test_fraction)) %>%  #Remove total and dissolved rows for organics - data tracked without test fraction
    dplyr::mutate(`2020_3030d_listing_category` = "",
           impaired_use_category = "",
           number_of_samples_1990_to_2021 = n_samples,
           number_of_samples_2016_to_2021 = n_samples_recent,
           most_recent_sample_year = most_recent_sample_year,
           number_of_detects = n_detects,
           most_recent_detect_year = most_recent_detect_year,
           number_of_samples_exceeding_class_d = n_d_exceedance,
           most_recent_class_d_exceedance = most_recent_class_d_exceedance,
           number_of_samples_since_last_class_d_exceedance = n_since_most_recent_d_exceedance,
           has_there_been_multi_class_d_exceedance_in_3yr = has_there_been_multi_class_d_exceedance_in_3yr,
           when_was_last_period_with_multi_class_d_exceedance = when_was_last_period_with_multi_class_d_exceedances
    ) %>%
    dplyr::select(waterbody_segment,
           pollutant_group,
           test_fraction,
           `2020_3030d_listing_category`,
           impaired_use_category,
           number_of_samples_1990_to_2021,
           number_of_samples_2016_to_2021,
           most_recent_sample_year,
           number_of_detects,
           most_recent_detect_year,
           number_of_samples_exceeding_class_d,
           most_recent_class_d_exceedance,
           number_of_samples_since_last_class_d_exceedance,
           has_there_been_multi_class_d_exceedance_in_3yr,
           when_was_last_period_with_multi_class_d_exceedance)

  df <- df %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(number_of_samples_1990_to_2021 = tidyr::replace_na(number_of_samples_1990_to_2021, "0"),
                  number_of_samples_2016_to_2021 = tidyr::replace_na(number_of_samples_2016_to_2021, "0"),
                  most_recent_sample_year = tidyr::replace_na(most_recent_sample_year, "Never"),
                  number_of_detects = tidyr::replace_na(number_of_detects, "0"),
                  most_recent_detect_year = tidyr::replace_na(most_recent_detect_year, "Never"),
                  number_of_samples_exceeding_class_d = tidyr::replace_na(number_of_samples_exceeding_class_d, "0 (D)"),
                  most_recent_class_d_exceedance = tidyr::replace_na(most_recent_class_d_exceedance, "Never (D)"),
                  number_of_samples_since_last_class_d_exceedance = tidyr::replace_na(number_of_samples_since_last_class_d_exceedance, "0 (D)"),
                  has_there_been_multi_class_d_exceedance_in_3yr = tidyr::replace_na(has_there_been_multi_class_d_exceedance_in_3yr, "No (D)"),
                  when_was_last_period_with_multi_class_d_exceedance = tidyr::replace_na(when_was_last_period_with_multi_class_d_exceedance, "Never (D)"))

  return(df)

}
