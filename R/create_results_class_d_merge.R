#' Create Class D Results Table with Merge Cell Formatting
#'
#' Add additional formatting so certain cells are merged together. Function
#' produces two separate excel files that need to be manually combined in order
#' to preserve the merged cells.
#'
#' @param dataframe obtained from
#'   \code{\link{create_results_class_d}}.
#'
#' @return Two excel files. One file has merged cells for cases with no Class D
#'  water quality criteria. A second file has no cell merging.
#' @export
#'
#' @examples
#' try( create_results_class_d_merge(my_results_class_d) )

create_results_class_d_merge <- function(df) {



  # Create lists of pollutants to determine which cells need to be merged
  pollutants_d_merge <- c("COPPER", "LEAD", "NAPHTHALENE")

  # Update contents of col M based on pollutant and exceedance value
  df <- df %>%
    dplyr::mutate(`# Samples Since Last Exceedance` = dplyr::case_when(`Pollutant` %in% pollutants_d_merge &
                                                                         `# Exceedances 1990-2021` == "N/A - no Class D WQ criteria" ~ "N/A - no Class D WQ criteria",
                                                                       TRUE ~ `# Samples Since Last Exceedance`))

  # Create separate dataframes for values that need to be merged and those that don't need to be merged
  df_merge <- df %>%
    dplyr::filter(`# Samples Since Last Exceedance` == "N/A - no Class D WQ criteria")

  df_no_merge <- df %>%
    dplyr::filter(!`# Samples Since Last Exceedance` == "N/A - no Class D WQ criteria")



  # Create a workbook from df_merge dataframe
  openxlsx::write.xlsx(df_merge, file = "output/results_class_d_merge_part1.xlsx", rowNames = F)
  mv <- openxlsx::loadWorkbook(file = "output/results_class_d_merge_part1.xlsx")

  # Merge cells with for loop to reference row index
  for(i in 1:nrow(df_merge)) {
    x <- df_merge[i,13]
    if(x == "N/A - no Class D WQ criteria"){
      openxlsx::mergeCells(mv, 1, cols = 13:16, rows = i+1)
    }
  }

  #Saving the merged workbook as an excel file
  openxlsx::saveWorkbook(mv, "output/results_class_d_merge_part1.xlsx", overwrite = TRUE)

  # Save the df_no_merge dataframe as a csv file
  write.csv(df_no_merge, file = "output/results_class_d_merge_part2.csv", row.names = F)



}


