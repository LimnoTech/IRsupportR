## -----------------------------------------------------------------------------
##
## IR Assessment Workflow
## for Toxics Reevaluation Analysis
##
## -----------------------------------------------------------------------------


devtools::load_all()


# ------------------------------------------------------------------------------
# 0. Load Processed Data
# ------------------------------------------------------------------------------

# file <- "data/final_processed_data_20230125.csv"
# ir_data <- read_ir_data(file)
ir_data <- all_processed_data

# Categories of pollutant_group
metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
organics <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PAH1", "PAH2","PAH3", "PCB_TOTAL")

# # Categories for Basic Summary
# basic_metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
# basic_pahs <- c("PAH1", "PAH2", "PAH3")
# basic_other <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PCB_TOTAL")




# ------------------------------------------------------------------------------
# 1. Lookup Criteria and Ratios
# ------------------------------------------------------------------------------


ir_data <- lookup_criteria(ir_data)

write.csv(ir_data, "output/curated_ir_data_draft_20230510.csv")


# ------------------------------------------------------------------------------
# 2. Evaluate Criteria
# ------------------------------------------------------------------------------

criteria_results <- evaluate_criteria(ir_data)

write.csv(criteria_results, "output/criteria_results_draft_20230510.csv")


# ------------------------------------------------------------------------------
# 3. Summarize Basic Summary
# ------------------------------------------------------------------------------

# 3a. Metals only (take into account test_fraction in analysis)
my_basic_summary_metals <- summarize_basic_metals(criteria_results)

# 3b. PAHs only (Exceedances of more than one PAH within a PAH group within one sample is considered one exceedance)
# my_basic_summary_pah <- summarize_basic_pah(criteria_results)

# 3c. Other (non Metal) parameters
my_basic_summary_other <- summarize_basic(criteria_results)

# 3d. Compile All Basic Summaries
my_basic_summary <- compile_basic(my_basic_summary_other, my_basic_summary_metals)

write.csv(my_basic_summary, file = "output/basic_summary_20230510.csv", row.names = F)

# ------------------------------------------------------------------------------
# 4. Summarize Basic Recent Summary (Last 10 Years)
# ------------------------------------------------------------------------------

# 4a. Metals Only
my_basic_summary_10yr_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = "07/01/2011", end_date = "06/30/2021")

# 4b. PAHs Only
# my_basic_summary_10yr_pah <- summarize_basic_recent_pah(criteria_results = criteria_results, start_date = "07/01/2011", end_date = "06/30/2021")

# 4c. Other (non Metal) parameters
my_basic_summary_10yr_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = "07/01/2011", end_date = "06/30/2021")

# 4d. Compile All Last 5 Year Summaries
my_basic_summary_10yr <- compile_basic_recent(my_basic_summary_10yr_metals, my_basic_summary_10yr_other)

# write.csv(my_basic_summary_10yr, file = "output/intermediate_results_d_draft_20230510.csv", row.names = F)



# ------------------------------------------------------------------------------
# 4. Summarize Basic Recent Summary (Last 5 Years)
# ------------------------------------------------------------------------------

# 4a. Metals Only
my_basic_summary_5yr_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

# 4b. PAHs Only
# my_basic_summary_5yr_pah <- summarize_basic_recent_pah(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

# 4c. Other (non Metal) parameters
my_basic_summary_5yr_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

# 4d. Compile All Last 5 Year Summaries
my_basic_summary_5yr <- compile_basic_recent(my_basic_summary_5yr_metals, my_basic_summary_5yr_other)






# # ------------------------------------------------------------------------------
# # 5. Summarize Period (3 Year Period)
# # ------------------------------------------------------------------------------
#
# # Detailed Exceedance Summary (Sum exceedances for three year periods starting in June and ending in July)
#
# # 5a. Metals Only
# my_period_summary_forward_metals <- summarize_periods_forward_metals(criteria_results)
#
# # 5b. PAHs Only
# my_period_summary_forward_pah <- summarize_periods_forward_pah(criteria_results)
#
# # 5c. Other (non Metal/PAH) parameters
# my_period_summary_forward_other <- summarize_periods_forward(criteria_results)
#
# # 5d. Compile All Period Summaries
# my_period_summary_forward <- compile_basic_recent(my_period_summary_forward_metals,  my_period_summary_forward_pah, my_period_summary_forward_other)
#
# # 5e. Format period summary
# my_formatted_period <- format_period_summary(my_period_summary_forward, period_end_year = "2021")
#


# ------------------------------------------------------------------------------
# 6. Compile Summaries
# ------------------------------------------------------------------------------

# 6a. consolidate for Appendix B - Class C
my_compiled_summaries <- compile_summaries(my_basic_summary, my_basic_summary_10yr, my_basic_summary_5yr)
# write.csv(my_compiled_summaries, file = "output/my_compiled_summaries_20230510.csv", row.names = F)


# ------------------------------------------------------------------------------
# 6. Create Decision Logic
# ------------------------------------------------------------------------------

# Class D
my_decision_logic_class_c <- create_decision_logic_class_c(my_compiled_summaries)
write.csv(my_decision_logic_class_c, file = "output/my_decision_logic_class_c.csv", row.names = F)

# Class D
my_decision_logic_class_d <- create_decision_logic_class_d(my_compiled_summaries)
write.csv(my_decision_logic_class_d, file = "output/my_decision_logic_class_d.csv", row.names = F)



# # ------------------------------------------------------------------------------
# # 6. Create Appendix B Tables
# # ------------------------------------------------------------------------------
#
# 6a. consolidate for Appendix B - Class C
my_appendix_b_class_c <- create_ir_appendix_b_class_c(my_decision_logic_class_c, five_year_start_date = "07/01/2016")
write.csv(my_appendix_b_class_c, file = "output/appendix_b_class_c_20230510.csv", row.names = F)

# 6b. consolidate for Appendix B - Class D
my_appendix_b_class_d <- create_ir_appendix_b_class_d(my_decision_logic_class_d, five_year_start_date = "07/01/2016")
write.csv(my_appendix_b_class_d, file = "output/appendix_b_class_d_20230510.csv", row.names = F)

# 6c. reconcile Class C and Class D decisions
my_appendix_b_reconciliation <- create_ir_appendix_b_reconciliation(my_decision_logic_class_c, my_decision_logic_class_d)
write.csv(my_appendix_b_reconciliation, file = "output/appendix_b_reconciliation_20230510.csv", row.names = F)

# 6d. consolidate for Appendix B - Class C Merge
my_appendix_b_class_c_merge <- create_ir_appendix_b_class_c_merge(my_decision_logic_class_c, five_year_start_date = "07/01/2016")
write.xlsx(my_appendix_b_class_c_merge, file = "output/appendix_b_class_c_merge_20230421.xlsx", row.names = F)

# 6e. consolidate for Appendix B - Class D Merge
my_appendix_b_class_d_merge <- create_ir_appendix_b_class_d_merge(my_decision_logic_class_d, five_year_start_date = "07/01/2016")
write.xlsx(my_appendix_b_class_d_merge, file = "output/appendix_b_class_d_merge_20230421.xlsx", row.names = F)





