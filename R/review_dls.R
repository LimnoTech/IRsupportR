#' review_dls
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
review_dls <- function(df) {

  df <- df %>%
    mutate(suspected_nd = case_when(detection == "d" & result <= dl ~ TRUE,
                                  TRUE ~ FALSE)) %>%
    mutate(insufficient_dl_class_c = case_when(dl > ccc ~ TRUE,
                                       TRUE ~ FALSE)) %>%
    mutate(insufficient_dl_class_d = case_when(d < dl ~ TRUE,
                                               TRUE ~ FALSE))


}
