#' Look Up Criteria
#'
#' Add water quality criteria to dataset.
#'
#' @param df obtained from all_processed_data.R script in data-raw folder.
#'
#' @return dataframe with water quality criteria columns added.
#' @export
#'
#' @examples
#' try( lookup_criteria(ir_data) )

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
