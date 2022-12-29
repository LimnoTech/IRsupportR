#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
lookup_criteria <- function(df) {

  # Expand non_metals_criteria table so that there is a record for pollutant/waterbody combination
  my_non_metals_criteria <- expand.grid(waterbody_segment = waterbodies$waterbody_segment, pollutant_name = non_metals_criteria$pollutant_name )

  my_non_metals_criteria <- my_non_metals_criteria %>%
    dplyr::left_join(non_metals_criteria, by = "pollutant_name")


  # Combine metal and non-metal criteria
  all_criteria <- dplyr::bind_rows(metals_criteria, my_non_metals_criteria)


  # Join criteria to main dataframe
  df <- df %>%
    dplyr::left_join(all_criteria,
                     by = c("waterbody_segment", "pollutant_name"))
    # dplyr::mutate(ccc = as.numeric(ccc),
    #        cmc = as.numeric(cmc),
    #        d = as.numeric(d))

}
