#' create_decision_logic
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
create_decision_logic<- function (df) {

  df <- df %>%
    dplyr::mutate(n_d_exceedance_2011_to_2021 = tidyr::replace_na(n_d_exceedance_2011_to_2021, 0)) %>%
    dplyr::mutate(n_samples_2011_to_2021 = tidyr::replace_na(n_samples_2011_to_2021, 0)) %>%
    dplyr::mutate(n_samples_2016_to_2021 = tidyr::replace_na(n_samples_2016_to_2021, 0))



  for (i in 1:nrow(df)) {

    if ( df[i, "current_category"] == "4a" ) {
      if ( df[i, "n_d_exceedance_2011_to_2021"] >= 1 ) {

        df[i, "decision_case_number"] <- 1

      } else if ( df[i, "n_d_exceedance_2011_to_2021"] == 1 ) {
        if ( df[i, "n_samples_2011_to_2021"] >= 5 ) {
          if ( df[i, "d_dl_ratio"] == "less than 1" ) {

            df[i, "decision_case_number"] <- 6

          } else if ( df[i, "d_dl_ratio"] == "greater than 1" ) {

            df[i, "decision_case_number"] <- 5

          }
        } else {

          df[i, "decision_case_number"] <- 4

        }

      } else if ( df[i, "n_d_exceedance_2011_to_2021"] < 1 ) {
        if ( df[i, "n_samples_2011_to_2021"] >= 5 ) {
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
      if ( df[i, "n_d_exceedance_2011_to_2021"] >= 1 ) {

        df[i, "decision_case_number"] <- 7

      } else if ( df[i, "n_d_exceedance_2011_to_2021"] == 1 ) {
        if ( df[i, "n_samples_2011_to_2021"] >= 5 ) {
          if ( df[i, "d_dl_ratio"] == "less than 1" ) {

            df[i, "decision_case_number"] <- 13

          } else if ( df[i, "d_dl_ratio"] == "greater than 1" ) {

            df[i, "decision_case_number"] <- 12

          }
        } else {

          df[i, "decision_case_number"] <- 11

        }

      } else if ( df[i, "n_d_exceedance_2011_to_2021"] < 1 ) {
        if ( df[i, "n_samples_2016_to_2021"] > 0 ) {
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
      if ( df[i, "n_d_exceedance_2011_to_2021"] >= 1 ) {

        df[i, "decision_case_number"] <- 14

      } else if ( df[i, "n_d_exceedance_2011_to_2021"] == 1 ) {
        if ( df[i, "d_dl_ratio"] == "less than 1" ) {

          df[i, "decision_case_number"] <- 16

        } else if ( df[i, "d_dl_ratio"] == "greater than 1" ) {
          if ( df[i, "n_d_exceedance_2016_to_2021"] > 0) {

            df[i, "decision_case_number"] <- 18

          } else {

            df[i, "decision_case_number"] <- 17

          }
        }

      } else if ( df[i, "n_d_exceedance_2011_to_2021"] < 1 ) {

        df[i, "decision_case_number"] <- 15

      }
    } # end current_category loop

  } # end for loop








  return(df)



}
