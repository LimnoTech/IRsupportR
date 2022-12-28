#### IR Assessment Workflow ####

devtools::load_all()
# library(dplyr)

# 0. Raw data
file <- "data/final_processed_data_20221228.csv"
ir_data <- read_ir_data(file)


#1. Lookup Criteria
ir_data <- lookup_criteria(ir_data)


#2. Review DLs
ir_data <- review_dls(ir_data)

########## Write intermediate output #############
write.csv(ir_data, "output/curated_ir_data.csv")
##################################################

#3. Evaluate Criteria
criteria_results <- evaluate_criteria(ir_data)


#4. Filter by Desired Timeframe
my_criteria_results <- filter_by_date(criteria_results, "01/01/1990", "06/30/2021")

########## Write intermediate output #############
write.csv(my_criteria_results, "output/criteria_results.csv")
##################################################


#5. Basic Summary
my_basic_summary <- summarize_basic(my_criteria_results)


#6. Last 5 Year Summary
my_basic_summary_recent <- summarize_basic_recent(criteria_results = my_criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")


#7. Exceedance summary, 2016-2021
my_period_summary_2016_2021 <- summarize_periods_2016_2021(my_criteria_results)


#8. Format period summary
my_formatted_period <- format_period_summary(my_period_summary_2016_2021)


#9. consolidate for Appendix B - Class C
my_appendix_b_class_c <- create_ir_appendix_b_class_c(my_basic_summary, my_basic_summary_recent, my_formatted_period)
write.csv(my_appendix_b_class_c, file = "output/appendix_b_class_c.csv", row.names = F)


#10. consolidate for Appendix B - Class D
my_appendix_b_class_d <- create_ir_appendix_b_class_d(my_basic_summary, my_basic_summary_recent, my_formatted_period)
write.csv(my_appendix_b_class_d, file = "output/appendix_b_class_d.csv", row.names = F)





