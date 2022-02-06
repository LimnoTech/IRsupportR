#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
evaluate_criteria <- function(df) {

  #only applicable for parameters for which an exceedance is a results greater than the criteria (e.g. metals). will not work for alternatives like DO

  #calculate raw exceedance results using only detection flags and results
  df <- df %>%
    dplyr::mutate(raw_exceedance_ccc = case_when(result > ccc & detection != "nd" ~ 1,
                                                            TRUE ~ 0)) %>%
    dplyr::mutate(raw_exceedance_cmc = case_when(result > cmc & detection != "nd" ~ 1,
                                                            TRUE ~ 0)) %>%
    dplyr::mutate(raw_exceedance_d = case_when(result > d & detection != "d" ~ 1,
                                               TRUE ~ 0))



  #calculate evidence based exceedances (excludes raw exceedances if the value is suspected as a ND because of a result equal to a rounded DL (e.g. 5, 25, or 50 ug/L))
  df <- df %>%
    dplyr::mutate(evidence_based_exceedance_ccc = case_when(raw_exceedance_ccc == TRUE &  suspected_nd == FALSE ~ 1,
                                                            TRUE ~ 0)) %>%
    dplyr::mutate(evidence_based_exceedance_cmc = case_when(raw_exceedance_cmc == TRUE &  suspected_nd == FALSE ~ 1,
                                                            TRUE ~ 0)) %>%
    dplyr::mutate(evidence_based_exceedance_d = case_when(raw_exceedance_d == TRUE &  suspected_nd == FALSE ~ 1,
                                                          TRUE ~ 0))


}
