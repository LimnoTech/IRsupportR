#' compile_summaries
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
compile_summaries <- function (my_basic_summary,
                               my_basic_summary_10yr,
                               my_basic_summary_5yr) {


  df_grid <- expand.grid(waterbody_segment = unique(my_basic_summary$waterbody_segment), pollutant_name = pollutant_name$pollutant_name,  test_fraction = c("TOTAL", "DISSOLVED", NA))

  df <- df_grid %>%
    dplyr::left_join(pollutant_name, by = "pollutant_name") %>%
    dplyr::left_join(my_basic_summary, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction")) %>%
    dplyr::left_join(my_basic_summary_10yr, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction")) %>%
    dplyr::left_join(my_basic_summary_5yr, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction")) %>%
    # dplyr::filter(n_samples != "")
    dplyr::filter(!pollutant_group %in% organics | is.na(test_fraction))  #Remove total and dissolved rows for organics - data tracked without test fraction


  df <- df %>%
    dplyr::left_join(ir_categories, by = c("pollutant_name", "waterbody_segment")) %>%
    dplyr::left_join(criteria_ratios, by = c("pollutant_name", "test_fraction")) %>%
    dplyr::left_join(tidal_type, by = "waterbody_segment") %>%
    dplyr::left_join(fish_tissue_results, by = c("pollutant_name", "tidal_type")) %>%
    dplyr::left_join(updated_tmdl, by = c("pollutant_name", "waterbody_segment"))

}
