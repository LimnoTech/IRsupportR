#' summarize_periods_2016_2021
#'
#' @param criteria_results
#' @param range_in_years
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summarize_periods_2016_2021 <- function(criteria_results,
                                        ...) {


  ######### Capture expressions #######

  dots <- ensyms(...)

  #####################################

  df <- criteria_results
  #df <- my_criteria_results


  df_summary <- df %>%
    #dplyr::group_by_at(vars(!!!dots)) %>%
    dplyr::group_by(site_summary_segment, group_lower, year) %>%
    dplyr::summarize(n_samples = n(),
                     most_recent_sample = max(year),
                     n_detects = sum(detection == "d"),
                     n_suspected_detects = sum(detection == "d" & suspected_nd == FALSE),
                     n_ccc_exceedance = sum(evidence_based_exceedance_ccc),
                     n_cmc_exceedance = sum(evidence_based_exceedance_cmc),
                     n_d_exceedance = sum(evidence_based_exceedance_d))


  #years <- seq(min(df$year), max(df$year))
  segments <- levels(as.factor(df$site_summary_segment))
  groups <- levels(as.factor(df$group_lower))
  dates <- c(as.Date("2016-07-01"), as.Date("2017-07-01"), as.Date("2018-07-01"))
  dateseq <- seq.Date(as.Date("2016-07-01"), as.Date("2021-06-30"), by="day")
  #mons <- seq(1,12)
  #year_mon <- expand.grid(year = years, mon= mons) %>% dplyr::mutate(year_mon = paste0(year, "-", mon)) %>% arrange(year, mon) %>% select(year_mon) %>% as.vector(.)

  df_ts <- expand.grid(site_summary_segment = segments, group_lower = groups, date = dateseq) %>%
    dplyr::left_join(df %>% select(site_summary_segment, group_lower, date, evidence_based_exceedance_ccc, evidence_based_exceedance_cmc))


  tally <- NULL
  for(g in groups) {
    for(site in segments) {
      dat <- df %>%
        dplyr::filter(group_lower == g & site_summary_segment == site)

      for (d in dates) {

        ts <- df_ts %>%
          dplyr::filter(group_lower == g & site_summary_segment == site) %>%
          dplyr::filter(between(date, d, d+365*3)) %>%
          dplyr::group_by(site_summary_segment, group_lower) %>%
          dplyr::summarize(evidence_based_exceedance_ccc_3yr_sum = sum(evidence_based_exceedance_ccc, na.rm = T),
                           evidence_based_exceedance_cmc_3yr_sum = sum(evidence_based_exceedance_cmc, na.rm = T)) %>%
          dplyr::mutate(start_date = d,
                        end_date = d+365*3)

        tally <- rbind(tally, ts)
      }
    }
  }


  period_summary <- tally %>%
    dplyr::mutate(start_date = as.Date(start_date, origin = "1970-01-01"),
                  end_date = as.Date(end_date, origin = "1970-01-01"))

  return(period_summary)


}
