#' summarize_periods_forward_pah
#'
#' @param criteria_results
#'
#' @return
#' @export
#'
#' @examples
summarize_periods_forward_pah <- function(criteria_results) {



  df <- criteria_results %>%
    dplyr::filter(pollutant_group %in% basic_pahs)

  df_exceed_ccc <- df %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_id, sample_date, year, month, exceedance_ccc) %>%
    dplyr::filter(exceedance_ccc == 1)

  df_exceed_cmc <- df %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_id, sample_date, year, month, exceedance_cmc) %>%
    dplyr::filter(exceedance_cmc == 1)

  df_exceed_d <- df %>%
    dplyr::select(waterbody_segment, pollutant_group, sample_id, sample_date, year, month, exceedance_d) %>%
    dplyr::filter(exceedance_d == 1)



  tally <- NULL

  # Look for exceedances, if df_exceed_ccc is not empty
  if(nrow(df_exceed_ccc) != 0) {

  # CCC Exceedances
    for (row in 1:nrow(df_exceed_ccc)) {
      seg <- df_exceed_ccc$waterbody_segment[row]
      grp <- df_exceed_ccc$pollutant_group[row]
      dat <- df_exceed_ccc$sample_date[row]
      yr <- df_exceed_ccc$year[row]
      mo <- df_exceed_ccc$month[row]


      # if sample_month is from January to June, filter between 7/1/(sample_year - 1) and 6/30/(sample_year + 2)
      if(mo >= 1 & mo <= 6){
        ts_samples <- df_exceed_ccc %>%
          dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
          dplyr::filter(dplyr::between(sample_date, as.Date(paste((yr-1),7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+2),6,30, sep="-"), "%Y-%m-%d"))) %>%
          dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
          dplyr::summarize(exceedance_ccc_3yr_sum = sum(exceedance_ccc, na.rm = T)) %>%
          dplyr::mutate(start_year = yr-1,
                        end_year = yr+2)

        # if sample_month is from July to December, filter between 7/1/sample_year and 6/30/(sample_year + 3)
      } else if(mo >= 7 & mo <= 12){
        ts_samples <- df_exceed_ccc %>%
          dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
          dplyr::filter(dplyr::between(sample_date, as.Date(paste(yr,7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+3),6,30, sep="-"), "%Y-%m-%d"))) %>%
          dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
          dplyr::summarize(exceedance_ccc_3yr_sum = sum(exceedance_ccc, na.rm = T)) %>%
          dplyr::mutate(start_year = yr,
                        end_year = yr+3)
      }

      ts <- ts_samples %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_ccc_3yr_sum = dplyr::n(),
                         start_year = max(start_year),
                         end_year = max(end_year))

      tally <- dplyr::bind_rows(tally, ts)

    }
  }

  # Look for exceedances if df_exceed_cmc is not empty
  if(nrow(df_exceed_cmc) != 0) {

    # CMC Exceedances
    for (row in 1:nrow(df_exceed_cmc)) {
      seg <- df_exceed_cmc$waterbody_segment[row]
      grp <- df_exceed_cmc$pollutant_group[row]
      dat <- df_exceed_cmc$sample_date[row]
      yr <- df_exceed_cmc$year[row]
      mo <- df_exceed_cmc$month[row]


      # if sample_month is from January to June, filter between 7/1/(sample_year - 1) and 6/30/(sample_year + 2)
      if(mo >= 1 & mo <= 6){
        ts_samples <- df_exceed_cmc %>%
          dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
          dplyr::filter(dplyr::between(sample_date, as.Date(paste((yr-1),7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+2),6,30, sep="-"), "%Y-%m-%d"))) %>%
          dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
          dplyr::summarize(exceedance_cmc_3yr_sum = sum(exceedance_cmc, na.rm = T)) %>%
          dplyr::mutate(start_year = yr-1,
                        end_year = yr+2)

        # if sample_month is from July to December, filter between 7/1/sample_year and 6/30/(sample_year + 3)
      } else if(mo >= 7 & mo <= 12){
        ts_samples <- df_exceed_cmc %>%
          dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
          dplyr::filter(dplyr::between(sample_date, as.Date(paste(yr,7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+3),6,30, sep="-"), "%Y-%m-%d"))) %>%
          dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
          dplyr::summarize(exceedance_cmc_3yr_sum = sum(exceedance_cmc, na.rm = T)) %>%
          dplyr::mutate(start_year = yr,
                        end_year = yr+3)
      }

      ts <- ts_samples %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_cmc_3yr_sum = dplyr::n(),
                         start_year = max(start_year),
                         end_year = max(end_year))

      tally <- dplyr::bind_rows(tally, ts)

    }
  }


  # Look for exceedances if df_exceed_d is not empty
  if(nrow(df_exceed_d) != 0) {

    # Class D Exceedances
    for (row in 1:nrow(df_exceed_d)) {
      seg <- df_exceed_d$waterbody_segment[row]
      grp <- df_exceed_d$pollutant_group[row]
      dat <- df_exceed_d$sample_date[row]
      yr <- df_exceed_d$year[row]
      mo <- df_exceed_d$month[row]


      # if sample_month is from January to June, filter between 7/1/(sample_year - 1) and 6/30/(sample_year + 2)
      if(mo >= 1 & mo <= 6){
        ts_samples <- df_exceed_d %>%
          dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
          dplyr::filter(dplyr::between(sample_date, as.Date(paste((yr-1),7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+2),6,30, sep="-"), "%Y-%m-%d"))) %>%
          dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
          dplyr::summarize(exceedance_d_3yr_sum = sum(exceedance_d, na.rm = T)) %>%
          dplyr::mutate(start_year = yr-1,
                        end_year = yr+2)

        # if sample_month is from July to December, filter between 7/1/sample_year and 6/30/(sample_year + 3)
      } else if(mo >= 7 & mo <= 12){
        ts_samples <- df_exceed_d %>%
          dplyr::filter(pollutant_group == grp & waterbody_segment == seg) %>%
          dplyr::filter(dplyr::between(sample_date, as.Date(paste(yr,7,1, sep="-"), "%Y-%m-%d"), as.Date(paste((yr+3),6,30, sep="-"), "%Y-%m-%d"))) %>%
          dplyr::group_by(waterbody_segment, pollutant_group, sample_id) %>%
          dplyr::summarize(exceedance_d_3yr_sum = sum(exceedance_d, na.rm = T)) %>%
          dplyr::mutate(start_year = yr,
                        end_year = yr+3)
      }

      ts <- ts_samples %>%
        dplyr::group_by(waterbody_segment, pollutant_group) %>%
        dplyr::summarize(exceedance_d_3yr_sum = dplyr::n(),
                         start_year = max(start_year),
                         end_year = max(end_year))


      tally <- dplyr::bind_rows(tally, ts)

    }
  }




  period_summary <- tally %>%
    dplyr::relocate(start_year, end_year, .after = exceedance_d_3yr_sum)


  return(period_summary)



}
