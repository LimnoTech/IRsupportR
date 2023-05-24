#' Create Class C Results Table with Merge Cell Formatting
#'
#' Add additional formatting so certain cells are merged together. Function
#' produces three separate excel files that need to be manually combined in
#' order to preserve the merged cells.
#'
#' @param dataframe obtained from
#'   \code{\link{create_ir_appendix_b_class_c}}.
#'
#' @return Three excel files. One file has merged cells for cases with no CCC
#'  and CMC water quality criteria. A second file has merged cells for cases
#'  with no CMC criteria. A third file has no cell merging for cases with both
#'  CCC and CMC criteria.
#' @export
#'
#' @examples
#' create_ir_appendix_b_class_c_merge(my_appendix_b_class_c)

create_ir_appendix_b_class_c_merge <- function (df) {



  #Create lists of pollutants to determine which cells need to be merged
  pollutants_c_merge <- c("BENZO_A_ANTHRACENE", "BENZO_A_PYRENE", "BENZO_K_FLUORANTHENE", "CHRYSENE", "DIBENZO_A_H_ANTHRACENE", "FLUORENE", "INDENO_1_2_3_CD_PYRENE", "PYRENE", "ANTHRACENE", "BENZO_B_FLUORANTHENE")
  pollutants_c_merge_cmc <- c("ACENAPHTHENE", "FLUORANTHENE", "NAPHTHALENE", "PCB_TOTAL")

  #Updating contents of col M based on pollutant and exceedance value
  df <- df %>%
    dplyr::mutate(`# Samples Since Last Exceedance` = dplyr::case_when(`Pollutant` %in% pollutants_c_merge ~ "N/A – no Class C WQ criteria",
                                                                                     TRUE ~ `# Samples Since Last Exceedance`))

  df <- df %>%
    dplyr::mutate(`Class C CMC Criterion (ug/L)` = dplyr::case_when(`Pollutant` %in% pollutants_c_merge_cmc ~ "N/A – no Class C CMC WQ criteria",
                                                   TRUE ~ `Class C CMC Criterion (ug/L)`))

  #Create new dataframe for values that need to be merged
  df_c_merge <- df %>%
    dplyr::filter(`# Samples Since Last Exceedance` == "N/A – no Class C WQ criteria")

  df_c_merge_cmc <- df %>%
    dplyr::filter(`Class C CMC Criterion (ug/L)` == "N/A – no Class C CMC WQ criteria")

  df_no_merge <- df %>%
    dplyr::filter(!`# Samples Since Last Exceedance` == "N/A – no Class C WQ criteria") %>%
    dplyr::filter(!`Class C CMC Criterion (ug/L)` == "N/A – no Class C CMC WQ criteria")



  #Creating a workbook from mergedValues data frame
  openxlsx::write.xlsx(df_c_merge, file = "output/appendix_b_class_c_merge_part1.xlsx", rowNames = F)
  openxlsx::write.xlsx(df_c_merge_cmc, file = "output/appendix_b_class_c_merge_part2.xlsx", rowNames = F)

  mv <- openxlsx::loadWorkbook(file = "output/appendix_b_class_c_merge_part1.xlsx")
  mv_cmc <- openxlsx::loadWorkbook(file = "output/appendix_b_class_c_merge_part2.xlsx")


  #Merging cells with for loop to reference row index
  for(i in 1:nrow(df_c_merge)) {
    x <- df_c_merge[i,13]
    if(x == "N/A – no Class C WQ criteria"){
      openxlsx::mergeCells(mv, 1, cols = 13:18, rows = i+1)
    }
  }

  for(i in 1:nrow(df_c_merge_cmc)) {
    x <- df_c_merge_cmc[i,16]
    if(x == "N/A – no Class C CMC WQ criteria"){
      openxlsx::mergeCells(mv_cmc, 1, cols = 16:17, rows = i+1)
    }
  }
  #Saving the workbook as an excel file
  openxlsx::saveWorkbook(mv, "output/appendix_b_class_c_merge_part1.xlsx", overwrite = TRUE)
  openxlsx::saveWorkbook(mv_cmc, "output/appendix_b_class_c_merge_part2.xlsx", overwrite = TRUE)


  # Save the df_no_merge dataframe as an excel file
  writexl::write_xlsx(df_no_merge, "output/appendix_b_class_c_merge_part3.xlsx")


}
