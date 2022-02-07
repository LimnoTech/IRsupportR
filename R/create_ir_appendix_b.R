#' create_ir_appendix_b
#'
#' @param formatted_period_results
#'
#' @return
#' @export
#'
#' @examples
create_ir_appendix_b <- function(basic_summary,
                                 basic_summary_recent,
                                 formatted_period_results) {



  period <- formatted_period_results %>%
    mutate(has_there_been_multi_ccc_exceedance_in_3yr = case_when(is.na(most_recent_period_with_multiple_ccc_exceedances) == F ~ "Yes (CCC)",
                                                                  TRUE ~ "No (CCC)")) %>%
    mutate(has_there_been_multi_cmc_exceedance_in_3yr = case_when(is.na(most_recent_period_with_multiple_cmc_exceedances) == F ~ "Yes (CMC)",
                                                                  TRUE ~ "No (CMC)")) %>%
    mutate(has_there_been_multi_class_c_exceedance_in_3yr = paste0(has_there_been_multi_ccc_exceedance_in_3yr, ", ", has_there_been_multi_cmc_exceedance_in_3yr)) %>%
    mutate(when_was_last_ccc_3yr = case_when(is.na(most_recent_period_with_multiple_ccc_exceedances) == F ~ paste0(most_recent_period_with_multiple_ccc_exceedances, " (CCC)"),
                                             TRUE ~ "Never (CCC)")) %>%
    mutate(when_was_last_cmc_3yr = case_when(is.na(most_recent_period_with_multiple_cmc_exceedances) == F ~ paste0(most_recent_period_with_multiple_cmc_exceedances, " (CMC)"),
                                             TRUE ~ "Never (CMC)")) %>%
    mutate(when_was_last_period_with_multi_class_c_exceedances = paste0(when_was_last_ccc_3yr, ", ", when_was_last_cmc_3yr))


  df <- expand.grid(site_summary_segment = unique(basic_summary$site_summary_segment), group_lower = paramlevels$lower)

  df <- df %>%
    left_join(basic_summary) %>%
    left_join(basic_summary_recent) %>%
    left_join(period) %>%
    filter(site_summary_segment != "") %>%
    mutate(`2020_3030d_listing_category` = "",
           impaired_use_category = "",
           number_of_samples_1990_to_2021 = n_samples,
           number_of_samples_2016_to_2021 = n_samples_recent,
           most_recent_sample = most_recent_sample,
           number_of_detects = n_detects,
           most_recent_detect = most_recent_detect,
           number_of_samples_exceeding_class_c = paste0(n_ccc_exceedance, " (CCC), ", n_cmc_exceedance, " (CMC)"),
           most_recent_class_c_exceedance = most_recent_class_c_exceedance,
           number_of_samples_since_last_class_c_exceedance = paste0(n_since_most_recent_ccc_exceedance, " (CCC), ", n_since_most_recent_cmc_exceedance, " (CMC)" ),
           has_there_been_multi_class_c_exceedance_in_3yr = has_there_been_multi_class_c_exceedance_in_3yr,
           when_was_last_period_with_multi_class_c_exceedance = when_was_last_period_with_multi_class_c_exceedances
    ) %>%
    select(site_summary_segment,
           group_lower,
           `2020_3030d_listing_category`,
           impaired_use_category,
           number_of_samples_1990_to_2021,
           number_of_samples_2016_to_2021,
           most_recent_sample,
           number_of_detects,
           most_recent_detect,
           number_of_samples_exceeding_class_c,
           most_recent_class_c_exceedance,
           number_of_samples_since_last_class_c_exceedance,
           has_there_been_multi_class_c_exceedance_in_3yr,
           when_was_last_period_with_multi_class_c_exceedance)


}
