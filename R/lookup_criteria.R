#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
lookup_criteria <- function(df) {

  # Expand organics_criteria table so that there is a record for pollutant/waterbody combination
  my_organics_criteria <- expand.grid(waterbody_segment = waterbodies$waterbody_segment, pollutant_name = organics_criteria$pollutant_name )

  my_organics_criteria <- my_organics_criteria %>%
    dplyr::left_join(organics_criteria, by = "pollutant_name")


  # Combine metal and non-metal criteria
  all_criteria <- dplyr::bind_rows(metals_criteria, my_organics_criteria)


  # Join criteria to main dataframe
  df <- df %>%
    dplyr::left_join(all_criteria,
                     by = c("waterbody_segment", "pollutant_name"))

}
