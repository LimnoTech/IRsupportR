#' qaqc_input
#'
#' @param ir_data
#'
#' @return
#' @export
#'
#' @examples
qaqc_input <- function(ir_data) {


  # Fix known qaqc issues from imported file. To be fixed in curation script.
  df <- ir_data %>%
    mutate(site_summary_segment = case_when(site_summary_segment == "Watts Banch" ~ "Watts Branch",
                                            TRUE ~ site_summary_segment))

  return(df)

}
