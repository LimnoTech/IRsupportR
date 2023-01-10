#### IR Assessment Workflow ####

devtools::load_all()
# library(dplyr)

# 0. Processed data
file <- "data/final_processed_data_20230109.csv"
ir_data <- read_ir_data(file)


#1. Lookup Criteria
ir_data <- lookup_criteria(ir_data)

########## Write intermediate output #############
write.csv(ir_data, "output/curated_ir_data.csv")
##################################################


#3. Evaluate Criteria
criteria_results <- evaluate_criteria(ir_data)

########## Write intermediate output #############
write.csv(criteria_results, "output/criteria_results.csv")
##################################################


#5. Basic Summary
my_basic_summary <- summarize_basic(criteria_results)


#6. Last 5 Year Summary
my_basic_summary_recent <- summarize_basic_recent(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")


#7. Detailed Exceedance Summary (looks at sample plus next three years of samples)
my_period_summary_forward <- summarize_periods_forward(criteria_results = criteria_results, range_in_years = 3)


#9. Format period summary
my_formatted_period <- format_period_summary(my_period_summary_forward, period_end_year = "2021", range_in_years = 3)


#9. consolidate for Appendix B - Class C
my_appendix_b_class_c <- create_ir_appendix_b_class_c(my_basic_summary, my_basic_summary_recent, my_formatted_period)
write.csv(my_appendix_b_class_c, file = "output/appendix_b_class_c_draft_20230109.csv", row.names = F)


#10. consolidate for Appendix B - Class D
my_appendix_b_class_d <- create_ir_appendix_b_class_d(my_basic_summary, my_basic_summary_recent, my_formatted_period)
write.csv(my_appendix_b_class_d, file = "output/appendix_b_class_d_draft_20230109.csv", row.names = F)





