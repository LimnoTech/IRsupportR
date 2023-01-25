#### IR Assessment Workflow ####

devtools::load_all()
# library(dplyr)

# 0. Processed data
file <- "data/final_processed_data_20230116.csv"
ir_data <- read_ir_data(file)

# # Categories of pollutant_group
# metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
# organics <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PAH1", "PAH2","PAH3", "PCB_TOTAL")

# Categories for Basic Summary
basic_metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
baSic_pahs <- c("PAH1", "PAH2", "PAH3")
basic_other <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PCB_TOTAL")


#1. Lookup Criteria
ir_data <- lookup_criteria(ir_data)

########## Write intermediate output #############
write.csv(ir_data, "output/curated_ir_data_draft_20230125.csv")
##################################################


#3. Evaluate Criteria
criteria_results <- evaluate_criteria(ir_data)

########## Write intermediate output #############
write.csv(criteria_results, "output/criteria_results_draft_20230125.csv")
##################################################


#4a. Basic Summary - Metals only (take into account test_fraction in analysis)
my_basic_summary_metals <- summarize_basic_metals(criteria_results)

#4b. Basic Summary - PAHs only (Exceedances of more than one PAH within a PAH group within one sample is considered one exceedance)
my_basic_summary_pah <- summarize_basic_pah(criteria_results)

#4c. Basic Summary - Other (non Metal/PAH) parameters
my_basic_summary_other <- summarize_basic(criteria_results)

#4d. Compile Basic Summaries
my_basic_summary <- compile_basic(my_basic_summary_other, my_basic_summary_pah, my_basic_summary_metals)



#5a. Last 5 Year Summary - Metals Only
my_basic_summary_recent_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

#5b. Last 5 Year Summary - PAHs Only
my_basic_summary_recent_pah <- summarize_basic_recent_pah(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

#5c. Last 5 Year Summary - Other (non Metal/PAH) parameters
my_basic_summary_recent_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

#5d. Compile All Last 5 Year Summaries
my_basic_summary_recent <- compile_basic_recent(my_basic_summary_recent_metals,  my_basic_summary_recent_pah, my_basic_summary_recent_other)



#7. Detailed Exceedance Summary (Sum exceedances for three year periods starting in June and ending in July)
#7a. Metals Only
my_period_summary_forward_metals <- summarize_periods_forward_metals(criteria_results)

#7b. PAHs Only
my_period_summary_forward_pah <- summarize_periods_forward_pah(criteria_results)

#7c. Other (non Metal/PAH) parameters
my_period_summary_forward_other <- summarize_periods_forward(criteria_results)

#7d. Compile All Period Summaries
my_period_summary_forward <- compile_basic_recent(my_period_summary_forward_metals,  my_period_summary_forward_pah, my_period_summary_forward_other)

#9. Format period summary
my_formatted_period <- format_period_summary(my_period_summary_forward, period_end_year = "2021")




#9. consolidate for Appendix B - Class C
my_appendix_b_class_c <- create_ir_appendix_b_class_c(my_basic_summary, my_basic_summary_recent, my_formatted_period)
write.csv(my_appendix_b_class_c, file = "output/appendix_b_class_c_draft_20230125.csv", row.names = F)


#10. consolidate for Appendix B - Class D
my_appendix_b_class_d <- create_ir_appendix_b_class_d(my_basic_summary, my_basic_summary_recent, my_formatted_period)
write.csv(my_appendix_b_class_d, file = "output/appendix_b_class_d_draft_20230125.csv", row.names = F)





