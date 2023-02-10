#' read_ir_data
#'
#' @param filename
#'
#' @return df
#' @export
#'
#' @examples
read_ir_data <- function(filename) {


  df <- read.csv(filename) %>%
    dplyr::mutate(sample_date = as.Date(sample_date))

  return(df)

}
