#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
lookup_criteria <- function(df) {

  df <- df %>%
    dplyr::left_join(criteria) %>%
    mutate(ccc = as.numeric(ccc),
           cmc = as.numeric(cmc),
           d = as.numeric(d))

}
