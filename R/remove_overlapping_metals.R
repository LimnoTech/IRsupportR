#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
remove_overlapping_metals <- function(df) {

  metals <- c("arsenic", "copper", "lead", "mercury", "zinc")


  df_metals <- df %>%
    dplyr::filter(group_lower %in% metals) %>%
    tidyr::separate(parameter, c("metal", "form"), remove=F) %>%
    dplyr::group_by(station, date, group_lower) %>%
    dplyr::summarize(overlapping_metals = n(),
                     n_total = sum(form == "total")) %>%
    dplyr::mutate(n_total = tidyr::replace_na(n_total, 0)) %>%
    dplyr::right_join(df) %>%
    tidyr::separate(parameter, c("metal", "form"), remove=F) %>%
    dplyr::mutate(metals_flag = case_when(overlapping_metals > 1 & n_total > 0 & form == "dissolved" ~ "remove",
                                          TRUE ~ "-")) %>%
    dplyr::filter(metals_flag != "remove") %>%
    dplyr::select(-metal, -metals_flag, -overlapping_metals, -n_total) %>%
    dplyr::ungroup()




}
