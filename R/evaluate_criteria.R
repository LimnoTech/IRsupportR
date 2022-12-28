#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
evaluate_criteria <- function(df) {

  # Determine all non-metals using list of all metals
  metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
  non_metals <- unique(ir_data$pollutant_name[!(ir_data$pollutant_name %in% metals)])

  # Create lists of metals to determine which criteria type and test fractions apply
  metals_d_total_criteria <- c("ARSENIC", "MERCURY", "ZINC")
  metals_ccc_diss_criteria <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
  metals_cmc_diss_criteria <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")


  #calculate raw exceedance results - evaluate metals and non-metals separately
  df <- df %>%
    dplyr::mutate(raw_exceedance_ccc = dplyr::case_when(pollutant_name %in% metals_ccc_diss_criteria &
                                                   test_fraction == "DISSOLVED" &
                                                   processed_result_value > ccc &
                                                   processed_detect_status != "ND" ~ 1,
                                                 pollutant_name %in% non_metals &
                                                   processed_result_value > ccc &
                                                   processed_detect_status != "ND" ~ 1,
                                                 TRUE ~ 0)) %>%
    dplyr::mutate(raw_exceedance_cmc = dplyr::case_when(pollutant_name %in% metals_cmc_diss_criteria &
                                                   test_fraction == "DISSOLVED" &
                                                   processed_result_value > cmc &
                                                   processed_detect_status != "ND" ~ 1,
                                                 pollutant_name %in% non_metals &
                                                   processed_result_value > cmc &
                                                   processed_detect_status != "ND" ~ 1,
                                                 TRUE ~ 0)) %>%
    dplyr::mutate(raw_exceedance_d = dplyr::case_when(pollutant_name %in% metals_d_total_criteria &
                                                   test_fraction == "TOTAL" &
                                                   processed_result_value > d &
                                                   processed_detect_status != "ND" ~ 1,
                                                 pollutant_name %in% non_metals &
                                                   processed_result_value > d &
                                                   processed_detect_status != "ND" ~ 1,
                                                 TRUE ~ 0))




}
