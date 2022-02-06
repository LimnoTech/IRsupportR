#' filter_by_date
#'
#' @param df
#' @param start_date mm/dd/YYYY
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
filter_by_date <- function(df,
                           start_date,
                           end_date) {


  start_date <- as.Date(start_date, format = "%m/%d/%Y")
  end_date <- as.Date(end_date, format = "%m/%d/%Y")

  df <- df %>%
    filter(between(date, start_date, end_date))


}
