#' Create Decision Reconciliation Table
#'
#' Create table for reconciliation between Class C and Class D decisions.
#'
#' @param my_decision_logic_class_c obtained from
#'   \code{\link{create_decision_logic_class_c}}.
#' @param my_decision_logic_class_d obtained from
#'   \code{\link{create_decision_logic_class_d}}.
#'
#' @return dataframe with Class C and Class D decisions and descriptions plus
#' the reconciled decision descriptions.
#' @export
#'
#' @examples
#' create_ir_appendix_b_reconciliation(my_decision_logic_class_c,
#'                                     my_decision_logic_class_d)

create_ir_appendix_b_reconciliation <- function (my_decision_logic_class_c, my_decision_logic_class_d) {

  df_c <- my_decision_logic_class_c %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "DISSOLVED") %>% # Keep only dissolved metals. Remove metals where test fraction is TOTAL or NA
    dplyr::select(waterbody_segment, pollutant_name, pollutant_group, current_category, c_decision_description, c_decision_case_number)

  df_d <- my_decision_logic_class_d %>%
    dplyr::filter(!pollutant_group %in% metals | test_fraction == "TOTAL") %>% # Keep only total metals. Remove metals where test fraction is DISSOLVED or NA
    dplyr::select(waterbody_segment, pollutant_name, pollutant_group, current_category, d_decision_description, d_decision_case_number)

  df <- df_c %>%
    dplyr::left_join(df_d, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "current_category")) %>%
    dplyr::left_join(decision_reconciliation, by = c("current_category", "c_decision_description", "d_decision_description")) %>%
    dplyr::rename("Waterbody" = waterbody_segment,
                  "Pollutant" = pollutant_name,
                  "Pollutant Group" = pollutant_group,
                  "Current Categorization in 2020 IR" = current_category,
                  "Class C Decision" = c_decision_description,
                  "Class C Case #" = c_decision_case_number,
                  "Class D Decision" = d_decision_description,
                  "Class D Case #" = d_decision_case_number,
                  "Reconciliation of Class C and Class D Categorization Decision" = reconciled_decision_description)



  return(df)


}
