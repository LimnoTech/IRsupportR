#' Create Class D Decision Logic
#'
#' Determine decision case number based on established logic for Class D
#' criteria.
#'
#' @param my_compiled_summaries obtained from
#'   \code{\link{compile_summaries}}.
#'
#' @return dataframe with columns added for decision case number and decision
#'   description.
#' @export
#'
#' @examples
#' try( create_decision_logic_class_c(my_compiled_summaries) )

create_decision_logic_class_d <- function (df,
                                           five_year_start_date,
                                           five_year_end_date,
                                           ten_year_start_date,
                                           ten_year_end_date) {

  # Determine analysis years and corresponding column names
  five_year_start_year <- format(as.Date(five_year_start_date, format="%m/%d/%Y"), "%Y")
  five_year_end_year <- format(as.Date(five_year_end_date, format="%m/%d/%Y"), "%Y")
  ten_year_start_year <- format(as.Date(ten_year_start_date, format="%m/%d/%Y"), "%Y")
  ten_year_end_year <- format(as.Date(ten_year_end_date, format="%m/%d/%Y"), "%Y")

  n_d_exceedance_10yr <- paste0("n_d_exceedance_", ten_year_start_year, "_to_", ten_year_end_year)
  n_d_exceedance_5yr <- paste0("n_d_exceedance_", five_year_start_year, "_to_", five_year_end_year)
  n_samples_10yr <- paste0("n_samples_", ten_year_start_year, "_to_", ten_year_end_year)
  n_samples_5yr <- paste0("n_samples_", five_year_start_year, "_to_", five_year_end_year)


  # Code fails if there are any NA entries in the columns being evaluated
  df[[n_d_exceedance_10yr]] = tidyr::replace_na(df[[n_d_exceedance_10yr]], 0)
  df[[n_d_exceedance_5yr]] = tidyr::replace_na(df[[n_d_exceedance_5yr]], 0)
  df[[n_samples_10yr]] = tidyr::replace_na(df[[n_samples_10yr]], 0)
  df[[n_samples_5yr]] = tidyr::replace_na(df[[n_samples_5yr]], 0)
  df$d_criterion = as.character(df$d_criterion)
  df$d_criterion = tidyr::replace_na(df$d_criterion, "no criteria")





  for (i in 1:nrow(df)) {

    if ( df[i, "d_criterion"] == "no criteria") {

      df[i, "decision_case_number"] <- 0

    } else {
      if ( df[i, "current_category"] == "4a" ) {
        if ( df[i, n_d_exceedance_10yr] > 1 ) {

          df[i, "decision_case_number"] <- 1

        } else if ( df[i, n_d_exceedance_10yr] == 1 ) {
          if ( df[i, n_samples_10yr] >= 5 ) {
            if ( df[i, "d_dl_ratio"] == "less than 1" ) {

              df[i, "decision_case_number"] <- 6

            } else if ( df[i, "d_dl_ratio"] == "greater than 1" ) {

              df[i, "decision_case_number"] <- 5

            }
          } else {

            df[i, "decision_case_number"] <- 4

          }

        } else if ( df[i, n_d_exceedance_10yr] < 1 ) {
          if ( df[i, n_samples_10yr] >= 5 ) {
            if ( df[i, "d_dl_ratio"] == "less than 1" ) {

              df[i, "decision_case_number"] <- 3

            } else {

              df[i, "decision_case_number"] <- 2

            }

          } else {

            df[i, "decision_case_number"] <- 2

          }

        }
      } else if ( df[i, "current_category"] == "3" ) {
        if ( df[i, n_d_exceedance_10yr] > 1 ) {

          df[i, "decision_case_number"] <- 7

        } else if ( df[i, n_d_exceedance_10yr] == 1 ) {
          if ( df[i, n_samples_10yr] >= 5 ) {
            if ( df[i, "d_dl_ratio"] == "less than 1" ) {

              df[i, "decision_case_number"] <- 13

            } else if ( df[i, "d_dl_ratio"] == "greater than 1" ) {

              df[i, "decision_case_number"] <- 12

            }
          } else {

            df[i, "decision_case_number"] <- 11

          }

        } else if ( df[i, n_d_exceedance_10yr] < 1 ) {
          if ( df[i, n_samples_5yr] > 0 ) {
            if ( df[i, "d_dl_ratio"] == "less than 1" ) {

              df[i, "decision_case_number"] <- 10

            } else {

              df[i, "decision_case_number"] <- 9

            }
          } else {

            df[i, "decision_case_number"] <- 8

          }
        }
      } else if ( df[i, "current_category"] == "Not Listed" ) {
        if ( df[i, n_d_exceedance_10yr] > 1 ) {

          df[i, "decision_case_number"] <- 14

        } else if ( df[i, n_d_exceedance_10yr] == 1 ) {
          if ( df[i, "d_dl_ratio"] == "less than 1" ) {

            df[i, "decision_case_number"] <- 16

          } else if ( df[i, "d_dl_ratio"] == "greater than 1" ) {
            if ( df[i, n_d_exceedance_5yr] > 0) {

              df[i, "decision_case_number"] <- 18

            } else {

              df[i, "decision_case_number"] <- 17

            }
          }

        } else if ( df[i, n_d_exceedance_10yr] < 1 ) {

          df[i, "decision_case_number"] <- 15

        }
      } # end current_category loop


    } # end loop checking if criterion is NA


  } # end for loop



  df <- df %>%
    dplyr::left_join(decisions, by = c("current_category", "decision_case_number")) %>%
    dplyr::rename(d_decision_description = decision_description,
                  d_decision_case_number = decision_case_number)



  return(df)



}
