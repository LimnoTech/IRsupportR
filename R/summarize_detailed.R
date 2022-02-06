#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
summarize_detailed <- function(criteria_results, basic_summary, ...) {



  ######### Capture expressions #######

  dots <- ensyms(...)



  ###########

  df <- criteria_results

  df_<- df %>%
    dplyr::group_by_at(vars(!!!dots))




}
