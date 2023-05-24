#' Compile Basic Summaries
#'
#' Bind together metal and organic basic data summaries.
#'
#' @param my_basic_summary_metals obtained from
#'   \code{\link{summarize_basic_metals}}.
#' @param my_basic_summary_other obtained from \code{\link{summarize_basic}}.
#'
#' @return dataframe with same columns as inputs. Column order may change.
#' @export
#'
#' @examples
#' try( compile_basic(my_basic_summary_other, my_basic_summary_metals) )

compile_basic <- function(my_basic_summary_metals, my_basic_summary_other) {


  df_compiled <- my_basic_summary_metals %>%
    dplyr::bind_rows(my_basic_summary_other) %>%
    dplyr::relocate(test_fraction, .after = pollutant_group)

  return(df_compiled)

}
