#' consolidate_data
#'
#' @param df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
consolidate_data <- function(df,
                            ...) {

######### Capture expressions #######

  dots <- ensyms(...)

######### Group Data ##############


  df_grouped <- df %>%
    group_by_at(vars(!!!dots))


  return(df_grouped)

}
