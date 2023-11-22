#' Compile All Summaries
#'
#' Join basic summary with period based summaries. Add data from lookup tables.
#'
#' @param my_basic_summary obtained from
#'   \code{\link{compile_basic}}.
#' @param my_basic_summary_10yr obtained from
#'   \code{\link{compile_basic_recent}}.
#' @param my_basic_summary_5yr obtained from
#'   \code{\link{compile_basic_recent}}.
#'
#' @return dataframe with columns from inputs plus additional lookup tables.
#' @export
#'
#' @examples
#' try( compile_summaries(my_basic_summary,
#'                         my_basic_summary_10yr,
#'                         my_basic_summary_5yr) )

compile_summaries <- function (my_basic_summary,
                               my_basic_summary_10yr,
                               my_basic_summary_5yr,
                               five_year_start_date,
                               five_year_end_date,
                               ten_year_start_date,
                               ten_year_end_date) {


  # Join Summary Tables
  df_grid <- expand.grid(waterbody_segment = unique(my_basic_summary$waterbody_segment), pollutant_name = pollutant_name$pollutant_name,  test_fraction = c("TOTAL", "DISSOLVED", NA))

  df <- df_grid %>%
    dplyr::left_join(pollutant_name, by = "pollutant_name") %>%
    dplyr::left_join(my_basic_summary, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction")) %>%
    dplyr::left_join(my_basic_summary_10yr, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction")) %>%
    dplyr::left_join(my_basic_summary_5yr, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction")) %>%
    # dplyr::filter(n_samples != "")
    dplyr::filter(!pollutant_group %in% organics | is.na(test_fraction))  #Remove total and dissolved rows for organics - data tracked without test fraction


  # Consolidate CCC and CMC
  five_year_start_year = format(as.Date(five_year_start_date, format="%m/%d/%Y"), "%Y")
  five_year_end_year <- format(as.Date(five_year_end_date, format="%m/%d/%Y"), "%Y")
  ten_year_start_year <- format(as.Date(ten_year_start_date, format="%m/%d/%Y"), "%Y")
  ten_year_end_year <- format(as.Date(ten_year_end_date, format="%m/%d/%Y"), "%Y")

  n_ccc_exceedance_5yr <- paste0("n_ccc_exceedance_", five_year_start_year, "_to_", five_year_end_year)
  n_ccc_exceedance_10yr <- paste0("n_ccc_exceedance_", ten_year_start_year, "_to_", ten_year_end_year)
  n_cmc_exceedance_5yr <- paste0("n_cmc_exceedance_", five_year_start_year, "_to_", five_year_end_year)
  n_cmc_exceedance_10yr <- paste0("n_cmc_exceedance_", ten_year_start_year, "_to_", ten_year_end_year)

  n_c_exceedance_5yr <- paste0("n_c_exceedance_", five_year_start_year, "_to_", five_year_end_year)
  n_c_exceedance_10yr <- paste0("n_c_exceedance_", ten_year_start_year, "_to_", ten_year_end_year)



  df$n_c_exceedance = pmax(df$n_ccc_exceedance, df$n_cmc_exceedance, na.rm = TRUE) # Columns exist with current names
  df[[n_c_exceedance_10yr]] = pmax(df[[n_ccc_exceedance_10yr]], df[[n_cmc_exceedance_10yr]], na.rm = TRUE) # Create columns with new names
  df[[n_c_exceedance_5yr]] = pmax(df[[n_ccc_exceedance_5yr]], df[[n_cmc_exceedance_5yr]], na.rm = TRUE) # Create columns with new names

  df <- df %>%
    dplyr::mutate(n_since_most_recent_c_exceedance = pmax(n_since_most_recent_ccc_exceedance, n_since_most_recent_cmc_exceedance, na.rm = TRUE),
                  most_recent_c_exceedance_date = pmax(most_recent_ccc_exceedance_date, most_recent_cmc_exceedance_date, na.rm = TRUE))

  # Join Additional Lookup Tables
  df <- df %>%
    dplyr::left_join(ir_categories, by = c("pollutant_name", "waterbody_segment")) %>%
    dplyr::left_join(criteria_ratios, by = c("pollutant_name", "test_fraction")) %>%
    dplyr::left_join(tidal_type, by = "waterbody_segment") %>%
    dplyr::left_join(fish_tissue_results, by = c("pollutant_name", "tidal_type")) %>%
    dplyr::left_join(updated_tmdl, by = c("pollutant_name", "waterbody_segment"))

}
