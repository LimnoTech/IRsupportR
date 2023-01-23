#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
create_ir_appendix_b_class_c <- function (basic_summary,
                                          basic_summary_recent,
                                          formatted_period_results) {


  period <- formatted_period_results %>%
    dplyr::mutate(has_there_been_multi_ccc_exceedance_in_3yr = dplyr::case_when(is.na(most_recent_period_with_multiple_ccc_exceedances) == F ~ "Yes (CCC)",
                                                                  TRUE ~ "No (CCC)")) %>%
    dplyr::mutate(has_there_been_multi_cmc_exceedance_in_3yr = dplyr::case_when(is.na(most_recent_period_with_multiple_cmc_exceedances) == F ~ "Yes (CMC)",
                                                                  TRUE ~ "No (CMC)")) %>%
    dplyr::mutate(has_there_been_multi_class_c_exceedance_in_3yr = paste0(has_there_been_multi_ccc_exceedance_in_3yr, ", ", has_there_been_multi_cmc_exceedance_in_3yr)) %>%
    dplyr::mutate(when_was_last_ccc_3yr = dplyr::case_when(is.na(most_recent_period_with_multiple_ccc_exceedances) == F ~ paste0(most_recent_period_with_multiple_ccc_exceedances, " (CCC)"),
                                             TRUE ~ "Never (CCC)")) %>%
    dplyr::mutate(when_was_last_cmc_3yr = dplyr::case_when(is.na(most_recent_period_with_multiple_cmc_exceedances) == F ~ paste0(most_recent_period_with_multiple_cmc_exceedances, " (CMC)"),
                                             TRUE ~ "Never (CMC)")) %>%
    dplyr::mutate(when_was_last_period_with_multi_class_c_exceedances = paste0(when_was_last_ccc_3yr, ", ", when_was_last_cmc_3yr))


  df_grid <- expand.grid(waterbody_segment = unique(basic_summary$waterbody_segment), pollutant_group = paramlevels$pollutant_group,  test_fraction = c("TOTAL", "DISSOLVED", NA))

  df <- df_grid %>%
    dplyr::left_join(basic_summary) %>%
    dplyr::left_join(basic_summary_recent) %>%
    dplyr::left_join(period) %>%
    dplyr::filter(waterbody_segment != "") %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "DISSOLVED") %>% #Keep only dissolved metals. Remove metals where test fraction is DISSOLVED or NA
    dplyr::filter(!pollutant_group %in% organics | is.na(test_fraction)) %>%  #Remove total and dissolved rows for organics - data tracked without test fraction
    dplyr::mutate(`2020_3030d_listing_category` = "",
           impaired_use_category = "",
           number_of_samples_1990_to_2021 = n_samples,
           number_of_samples_2016_to_2021 = n_samples_recent,
           most_recent_sample_year = most_recent_sample_year,
           number_of_detects = n_detects,
           most_recent_detect_year = most_recent_detect_year,
           number_of_samples_exceeding_class_c = paste0(n_ccc_exceedance, " (CCC), ", n_cmc_exceedance, " (CMC)"),
           most_recent_class_c_exceedance = most_recent_class_c_exceedance,
           number_of_samples_since_last_class_c_exceedance = paste0(n_since_most_recent_ccc_exceedance, " (CCC), ", n_since_most_recent_cmc_exceedance, " (CMC)" ),
           has_there_been_multi_class_c_exceedance_in_3yr = has_there_been_multi_class_c_exceedance_in_3yr,
           when_was_last_period_with_multi_class_c_exceedance = when_was_last_period_with_multi_class_c_exceedances
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
           number_of_samples_exceeding_class_c,
           most_recent_class_c_exceedance,
           number_of_samples_since_last_class_c_exceedance,
           has_there_been_multi_class_c_exceedance_in_3yr,
           when_was_last_period_with_multi_class_c_exceedance)

  df <- df %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(number_of_samples_1990_to_2021 = tidyr::replace_na(number_of_samples_1990_to_2021, "0"),
                  number_of_samples_2016_to_2021 = tidyr::replace_na(number_of_samples_2016_to_2021, "0"),
                  most_recent_sample_year = tidyr::replace_na(most_recent_sample_year, "Never"),
                  number_of_detects = tidyr::replace_na(number_of_detects, "0"),
                  most_recent_detect_year = tidyr::replace_na(most_recent_detect_year, "Never"),
                  number_of_samples_exceeding_class_c = tidyr::replace_na(number_of_samples_exceeding_class_c, "0 (CCC) 0 (CMC)"),
                  most_recent_class_c_exceedance = tidyr::replace_na(most_recent_class_c_exceedance, "Never (CCC) Never (CMC)"),
                  number_of_samples_since_last_class_c_exceedance = tidyr::replace_na(number_of_samples_since_last_class_c_exceedance, "0 (CCC) 0 (CMC)"),
                  has_there_been_multi_class_c_exceedance_in_3yr = tidyr::replace_na(has_there_been_multi_class_c_exceedance_in_3yr, "No (CCC) No (CMC)"),
                  when_was_last_period_with_multi_class_c_exceedance = tidyr::replace_na(when_was_last_period_with_multi_class_c_exceedance, "Never (CCC) Never (CMC)"))

  return(df)


}
