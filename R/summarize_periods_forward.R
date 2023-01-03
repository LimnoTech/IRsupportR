#' summarize_periods_forward
#'
#' @param criteria_results
#' @param range_in_years
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
summarize_periods_forward <- function(criteria_results,
                              range_in_years,
                              ...) {


  ######### Capture expressions #######

  dots <- rlang::ensyms(...)

  #####################################

  df <- criteria_results
  #df <- my_criteria_results


  df_summary <- df %>%
    dplyr::group_by_at(dplyr::vars(!!!dots)) %>%
    #dplyr::group_by(waterbody_segment, pollutant_group, year) %>%
    dplyr::summarize(n_samples = dplyr::n(),
                     most_recent_sample = max(year),
                     n_detects = sum(processed_detect_status == "D"),
                     n_ccc_exceedance = sum(exceedance_ccc),
                     n_cmc_exceedance = sum(exceedance_cmc),
                     n_d_exceedance = sum(exceedance_d))


  #years <- seq(min(df$year), max(df$year))
  segments <- levels(as.factor(df$waterbody_segment))
  groups <- levels(as.factor(df$pollutant_group))
  dates <- seq.Date(min(df$sample_date), max(df$sample_date), by='day')
  #mons <- seq(1,12)
  #year_mon <- expand.grid(year = years, mon= mons) %>% dplyr::mutate(year_mon = paste0(year, "-", mon)) %>% arrange(year, mon) %>% select(year_mon) %>% as.vector(.)

  df_ts <- expand.grid(waterbody_segment = segments, pollutant_group = groups, date = dates) %>%
    dplyr::left_join(df %>% dplyr::select(waterbody_segment, pollutant_group, sample_date, exceedance_ccc, exceedance_cmc))


  tally <- NULL
  for(g in groups) {
    for(site in segments) {
      dat <- df %>%
        dplyr::filter(pollutant_group == g & waterbody_segment == site)
      dates <- unique(dat$sample_date)

      for(d in dates) {
        ts <- df_ts %>%
          dplyr::filter(pollutant_group == g & waterbody_segment == site) %>%
          dplyr::filter(dplyr::between(sample_date, d, d+365*range_in_years)) %>%
          dplyr::group_by(waterbody_segment, pollutant_group) %>%
          dplyr::summarize(exceedance_ccc_3yr_sum = sum(exceedance_ccc, na.rm = T),
                           exceedance_cmc_3yr_sum = sum(exceedance_cmc, na.rm = T)) %>%
          dplyr::mutate(start_date = d,
                        end_date = d+365*range_in_years)

        tally <- rbind(tally, ts)
      }
    }
  }


  period_summary <- tally %>%
    dplyr::mutate(start_date = as.Date(start_date, origin = "1970-01-01"),
                  end_date = as.Date(end_date, origin = "1970-01-01"))

  return(period_summary)


}
