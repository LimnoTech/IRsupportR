#' summarize_periods_forward
#'
#' @param criteria_results
#'
#' @return
#' @export
#'
#' @examples
summarize_periods_forward <- function(criteria_results) {


  ######### Capture expressions #######



  #####################################



  df_exceed_ccc <- criteria_results %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_date, year, month, exceedance_ccc) %>%
    dplyr::filter(exceedance_ccc == 1)

  df_exceed_cmc <- criteria_results %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_date, year, month, exceedance_cmc) %>%
    dplyr::filter(exceedance_cmc == 1)

  df_exceed_d <- criteria_results %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_date, year, month, exceedance_d) %>%
    dplyr::filter(exceedance_d == 1)



  tally <- NULL

  # CCC Exceedances
  for (row in 1:nrow(df_exceed_ccc)) {
    seg <- df_exceed_ccc[row, "waterbody_segment"]
    grp <- df_exceed_ccc[row, "pollutant_group"]
    dat <- df_exceed_ccc[row, "sample_date"]
    yr <- df_exceed_ccc[row, "year"]
    mo <- df_exceed_ccc[row, "month"]


    # if sample_month is from January to June, filter between 7/1/(sample_year - 1) and 6/30/(sample_year + 2)
    if(mo >= 1 & mo <= 6){
      ts <- df_exceed_ccc %>%
        dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
        dplyr::filter(dplyr::between(sample_date, as.Date(paste((yr-1),7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+2),6,30, sep="-"), "%Y-%m-%d"))) %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_ccc_3yr_sum = sum(exceedance_ccc, na.rm = T)) %>%
        dplyr::mutate(start_year = yr-1,
                      end_year = yr+2)

      # if sample_month is from July to December, filter between 7/1/sample_year and 6/30/(sample_year + 3)
    } else if(mo >= 7 & mo <= 12){
      ts <- df_exceed_ccc %>%
        dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
        dplyr::filter(dplyr::between(sample_date, as.Date(paste(yr,7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+3),6,30, sep="-"), "%Y-%m-%d"))) %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_ccc_3yr_sum = sum(exceedance_ccc, na.rm = T)) %>%
        dplyr::mutate(start_year = yr,
                      end_year = yr+3)
    }

    tally <- dplyr::bind_rows(tally, ts)

  }


  # CMC Exceedances
  for (row in 1:nrow(df_exceed_cmc)) {
    seg <- df_exceed_cmc[row, "waterbody_segment"]
    grp <- df_exceed_cmc[row, "pollutant_group"]
    dat <- df_exceed_cmc[row, "sample_date"]
    yr <- df_exceed_cmc[row, "year"]
    mo <- df_exceed_cmc[row, "month"]


    # if sample_month is from January to June, filter between 7/1/(sample_year - 1) and 6/30/(sample_year + 2)
    if(mo >= 1 & mo <= 6){
      ts <- df_exceed_cmc %>%
        dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
        dplyr::filter(dplyr::between(sample_date, as.Date(paste((yr-1),7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+2),6,30, sep="-"), "%Y-%m-%d"))) %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_cmc_3yr_sum = sum(exceedance_cmc, na.rm = T)) %>%
        dplyr::mutate(start_year = yr-1,
                      end_year = yr+2)

      # if sample_month is from July to December, filter between 7/1/sample_year and 6/30/(sample_year + 3)
    } else if(mo >= 7 & mo <= 12){
      ts <- df_exceed_cmc %>%
        dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
        dplyr::filter(dplyr::between(sample_date, as.Date(paste(yr,7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+3),6,30, sep="-"), "%Y-%m-%d"))) %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_cmc_3yr_sum = sum(exceedance_cmc, na.rm = T)) %>%
        dplyr::mutate(start_year = yr,
                      end_year = yr+3)
    }

    tally <- dplyr::bind_rows(tally, ts)

  }

  # Class D Exceedances
  for (row in 1:nrow(df_exceed_d)) {
    seg <- df_exceed_d[row, "waterbody_segment"]
    grp <- df_exceed_d[row, "pollutant_group"]
    dat <- df_exceed_d[row, "sample_date"]
    yr <- df_exceed_d[row, "year"]
    mo <- df_exceed_d[row, "month"]


    # if sample_month is from January to June, filter between 7/1/(sample_year - 1) and 6/30/(sample_year + 2)
    if(mo >= 1 & mo <= 6){
      ts <- df_exceed_d %>%
        dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
        dplyr::filter(dplyr::between(sample_date, as.Date(paste((yr-1),7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+2),6,30, sep="-"), "%Y-%m-%d"))) %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_d_3yr_sum = sum(exceedance_d, na.rm = T)) %>%
        dplyr::mutate(start_year = yr-1,
                      end_year = yr+2)

      # if sample_month is from July to December, filter between 7/1/sample_year and 6/30/(sample_year + 3)
    } else if(mo >= 7 & mo <= 12){
      ts <- df_exceed_d %>%
        dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
        dplyr::filter(dplyr::between(sample_date, as.Date(paste(yr,7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+3),6,30, sep="-"), "%Y-%m-%d"))) %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_d_3yr_sum = sum(exceedance_d, na.rm = T)) %>%
        dplyr::mutate(start_year = yr,
                      end_year = yr+3)
    }

    tally <- dplyr::bind_rows(tally, ts)

  }




  period_summary <- tally %>%
    dplyr::relocate(start_year, end_year, .after = exceedance_d_3yr_sum)


  return(period_summary)



}
