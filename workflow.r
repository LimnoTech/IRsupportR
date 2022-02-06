#### IR Assessment Workflow ####


# 0. Raw data
file <- "data/All_TMDL_Data_forAnalysis_20220110.csv"
ir_data <- read_ir_data(file)

# 1. Remove overlapping metals (only use total if a total and dissolved are collected in same segment on same day)
ir_data <- remove_overlapping_metals(ir_data)


#1. Lookup Criteria
ir_data <- lookup_criteria(ir_data)


#2. Review DLs
ir_data <- review_dls(ir_data)


#3. Evaluate Criteria
criteria_results <- evaluate_criteria(ir_data)


#4. Filter by Desired Timeframe
my_criteria_results <- filter_by_date(criteria_results, "01/01/1990", "06/30/2021")


#5. Basic Summary
my_basic_summary <- summarize_basic(my_criteria_results, site_summary_segment, group_lower)


#6. Last 5 Year Summary
my_basic_summary_recent <- summarize_basic_recent(criteria_results = my_criteria_results, start_date = "07/01/2016", end_date = "06/30/2021", site_summary_segment, group_lower)


#7. Detailed Exceedance Summary (looks at sample plus next three years of samples)
my_period_summary_forward <- summarize_periods_forward(criteria_results = my_criteria_results, range_in_years = 3, site_summary_segment, group_lower, year)

#8. Detailed Exceedance Summary (looks at sample plus previous three years)
my_period_summary_backward <- summarize_periods_backward(criteria_results = my_criteria_results, range_in_years = 3, site_summary_segment, group_lower, year)

#9. Format period summary
my_formated_period <- format_period_summary(my_period_summary_backward)


#10. consolidate for appendix B




