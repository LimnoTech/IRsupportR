% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_decision_logic_class_d.R
\name{create_decision_logic_class_d}
\alias{create_decision_logic_class_d}
\title{Create Class D Decision Logic}
\usage{
create_decision_logic_class_d(
  df,
  five_year_start_date,
  five_year_end_date,
  ten_year_start_date,
  ten_year_end_date
)
}
\arguments{
\item{my_compiled_summaries}{obtained from
\code{\link{compile_summaries}}.}
}
\value{
dataframe with columns added for decision case number and decision
  description.
}
\description{
Determine decision case number based on established logic for Class D
criteria.
}
\examples{
try( create_decision_logic_class_c(my_compiled_summaries) )
}
