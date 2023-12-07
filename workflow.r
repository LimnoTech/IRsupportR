## -----------------------------------------------------------------------------
##
## IR Assessment Workflow
## for Toxics Reevaluation Analysis
##
## -----------------------------------------------------------------------------


devtools::load_all()


# ------------------------------------------------------------------------------
# 0. Load Processed Data & Update Variables
# ------------------------------------------------------------------------------

ir_data <- all_processed_data

# Categories of pollutant_group - Updates also required in pollutant crosswalk spreadsheet
metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
organics <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PAH1", "PAH2","PAH3", "PCB_TOTAL")

# Timeframe for analysis - Update as needed
start_date_5yr <- "07/01/2018"
end_date_5yr <- "06/30/2023"
start_date_10yr <- "07/01/2013"
end_date_10yr <- "06/30/2023"


# ------------------------------------------------------------------------------
# 1. Lookup Criteria
# ------------------------------------------------------------------------------

ir_data <- lookup_criteria(ir_data)


# ------------------------------------------------------------------------------
# 2. Evaluate Criteria
# ------------------------------------------------------------------------------

criteria_results <- evaluate_criteria(ir_data)

write.csv(criteria_results, "output/criteria_results.csv")


# ------------------------------------------------------------------------------
# 3. Summarize Basic Summary
# ------------------------------------------------------------------------------

# 3a. Metals only (take into account test_fraction in analysis)
my_basic_summary_metals <- summarize_basic_metals(criteria_results)

# 3b. Other (non Metal) parameters
my_basic_summary_other <- summarize_basic(criteria_results)

# 3c. Compile All Basic Summaries
my_basic_summary <- compile_basic(my_basic_summary_other, my_basic_summary_metals)


# ------------------------------------------------------------------------------
# 4. Summarize Basic Recent Summary (Last 10 Years)
# ------------------------------------------------------------------------------

# 4a. Metals Only
my_basic_summary_10yr_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = start_date_10yr, end_date = end_date_10yr)

# 4b. Other (non Metal) parameters
my_basic_summary_10yr_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = start_date_10yr, end_date = end_date_10yr)

# 4c. Compile All Last 5 Year Summaries
my_basic_summary_10yr <- compile_basic_recent(my_basic_summary_10yr_metals, my_basic_summary_10yr_other)



# ------------------------------------------------------------------------------
# 5. Summarize Basic Recent Summary (Last 5 Years)
# ------------------------------------------------------------------------------

# 5a. Metals Only
my_basic_summary_5yr_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = start_date_5yr, end_date = end_date_5yr)

# 5b. Other (non Metal) parameters
my_basic_summary_5yr_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = start_date_5yr, end_date = end_date_5yr)

# 5c. Compile All Last 5 Year Summaries
my_basic_summary_5yr <- compile_basic_recent(my_basic_summary_5yr_metals, my_basic_summary_5yr_other)



# ------------------------------------------------------------------------------
# 6. Compile Summaries
# ------------------------------------------------------------------------------

# Consolidate Summaries
my_compiled_summaries <- compile_summaries(my_basic_summary, my_basic_summary_10yr, my_basic_summary_5yr, five_year_start_date = start_date_5yr, five_year_end_date = end_date_5yr, ten_year_start_date = start_date_10yr, ten_year_end_date = end_date_10yr)


# ------------------------------------------------------------------------------
# 7. Create Decision Logic
# ------------------------------------------------------------------------------

# 7a. Class C
my_decision_logic_class_c <- create_decision_logic_class_c(my_compiled_summaries, five_year_start_date = start_date_5yr, five_year_end_date = end_date_5yr, ten_year_start_date = start_date_10yr, ten_year_end_date = end_date_10yr)

# 7b. Class D
my_decision_logic_class_d <- create_decision_logic_class_d(my_compiled_summaries, five_year_start_date = start_date_5yr, five_year_end_date = end_date_5yr, ten_year_start_date = start_date_10yr, ten_year_end_date = end_date_10yr)



# ------------------------------------------------------------------------------
# 8. Create Results Tables
# ------------------------------------------------------------------------------

# 8a. Consolidate for Results - Class C
my_results_class_c <- create_results_class_c(my_decision_logic_class_c, five_year_start_date = start_date_5yr, five_year_end_date = end_date_5yr, ten_year_start_date = start_date_10yr, ten_year_end_date = end_date_10yr)
write.csv(my_results_class_c, file = "output/results_class_c.csv", row.names = F)

# 8b. Consolidate for Results - Class D
my_results_class_d <- create_results_class_d(my_decision_logic_class_d, five_year_start_date = start_date_5yr, five_year_end_date = end_date_5yr, ten_year_start_date = start_date_10yr, ten_year_end_date = end_date_10yr)
write.csv(my_results_class_d, file = "output/results_class_d.csv", row.names = F)

# 8c. Reconcile Class C and Class D decisions
my_results_reconciliation <- create_results_reconciliation(my_decision_logic_class_c, my_decision_logic_class_d)
write.csv(my_results_reconciliation, file = "output/results_reconciliation.csv", row.names = F)

# 8d. Recreate Results - Class C tables with merge formatting
my_results_class_c_merge <- create_results_class_c_merge(my_results_class_c)

# 8e. Recreate Results - Class D tables with merge formatting
my_results_class_d_merge <- create_results_class_d_merge(my_results_class_d)


# ------------------------------------------------------------------------------
# -1. Restore 3-Year Summary
# ------------------------------------------------------------------------------

# Detailed Exceedance Summary (Sum exceedances for three year periods starting in June and ending in July)

# Categories for Basic Summary
basic_metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
basic_pahs <- c("PAH1", "PAH2", "PAH3")
basic_other <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PCB_TOTAL")


# 5a. Metals Only
my_period_summary_forward_metals <- summarize_periods_forward_metals(criteria_results)

# 5b. PAHs Only
my_period_summary_forward_pah <- summarize_periods_forward_pah(criteria_results)

# 5c. Other (non Metal/PAH) parameters
my_period_summary_forward_other <- summarize_periods_forward(criteria_results)

# 5d. Compile All Period Summaries
my_period_summary_forward <- my_period_summary_forward_metals %>%
  dplyr::bind_rows(my_period_summary_forward_pah, my_period_summary_forward_other)

# 5e. Format period summary
my_formatted_period <- format_period_summary(my_period_summary_forward, period_end_year = "2023")

