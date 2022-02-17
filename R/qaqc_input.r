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



  # Convert all units to ug/L, assume ug/L if not given explicitly in raw data (only applies to DOEE lab from pre 2013)

  df <- df %>%
    mutate(result = case_when(unit == "mg/L" ~ result*1000,
                              unit == "mg/l" ~ result*1000,
                              unit == "ng/L" ~ result/1000,
                              TRUE ~ result)) %>%
    mutate(unit = "ug/l")



  return(df)

}
