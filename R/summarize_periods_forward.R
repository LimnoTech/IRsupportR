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
                              range_in_years) {


  ######### Capture expressions #######



  #####################################




  df_exceed_ccc <- criteria_results %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_date, exceedance_ccc) %>%
    dplyr::filter(exceedance_ccc == 1)

  df_exceed_cmc <- criteria_results %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_date, exceedance_cmc) %>%
    dplyr::filter(exceedance_cmc == 1)

  df_exceed_d <- criteria_results %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_date, exceedance_d) %>%
    dplyr::filter(exceedance_d == 1)



  tally <- NULL

  # CCC Exceedances
  for (row in 1:nrow(df_exceed_ccc)) {
    dat <- df_exceed_ccc[row, "sample_date"]
    seg <- df_exceed_ccc[row, "waterbody_segment"]
    grp <- df_exceed_ccc[row, "pollutant_group"]

    ts <- df_exceed_ccc %>%
      dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
      dplyr::filter(dplyr::between(sample_date, dat, dat+365*range_in_years)) %>%
      dplyr::group_by(waterbody_segment, pollutant_group) %>%
      dplyr::summarize(exceedance_ccc_3yr_sum = sum(exceedance_ccc, na.rm = T)) %>%
      dplyr::mutate(start_date = dat,
                    end_date = dat+365*range_in_years)

    tally <- dplyr::bind_rows(tally, ts)

  }


  # CMC Exceedances
  for (row in 1:nrow(df_exceed_cmc)) {
    dat <- df_exceed_cmc[row, "sample_date"]
    seg <- df_exceed_cmc[row, "waterbody_segment"]
    grp <- df_exceed_cmc[row, "pollutant_group"]

    ts <- df_exceed_cmc %>%
      dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
      dplyr::filter(dplyr::between(sample_date, dat, dat+365*range_in_years)) %>%
      dplyr::group_by(waterbody_segment, pollutant_group) %>%
      dplyr::summarize(exceedance_cmc_3yr_sum = sum(exceedance_cmc, na.rm = T)) %>%
      dplyr::mutate(start_date = dat,
                    end_date = dat+365*range_in_years)

    tally <- dplyr::bind_rows(tally, ts)

  }

  # Class D Exceedances
  for (row in 1:nrow(df_exceed_d)) {
    dat <- df_exceed_d[row, "sample_date"]
    seg <- df_exceed_d[row, "waterbody_segment"]
    grp <- df_exceed_d[row, "pollutant_group"]

    ts <- df_exceed_d %>%
      dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
      dplyr::filter(dplyr::between(sample_date, dat, dat+365*range_in_years)) %>%
      dplyr::group_by(waterbody_segment, pollutant_group) %>%
      dplyr::summarize(exceedance_d_3yr_sum = sum(exceedance_d, na.rm = T)) %>%
      dplyr::mutate(start_date = dat,
                    end_date = dat+365*range_in_years)

    tally <- dplyr::bind_rows(tally, ts)

  }




  period_summary <- tally %>%
    dplyr::relocate(start_date, end_date, .after = exceedance_d_3yr_sum) %>%
    dplyr::mutate(start_date = as.Date(start_date, origin = "1970-01-01"),
                  end_date = as.Date(end_date, origin = "1970-01-01"))


  return(period_summary)



}
