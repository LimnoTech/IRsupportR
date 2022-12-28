#' read_ir_data
#'
#' @param filename
#'
#' @return df
#' @export
#'
#' @examples
read_ir_data <- function(filename) {

#### Read in levels ####

  df <- read.csv(filename) %>%
    # dplyr::mutate(lower = group) %>%
    # dplyr::left_join(paramlevels, by = "lower") %>%
    # dplyr::select(-group) %>%
    # dplyr::rename("group_lower" = lower, "group_upper" = upper) %>%
    # tidyr::drop_na(group_upper) %>%
    dplyr::mutate(sample_date = as.Date(sample_date))

  return(df)

}
