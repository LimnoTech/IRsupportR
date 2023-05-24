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

ir_data <- all_processed_data

# Categories of pollutant_group
metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
organics <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PAH1", "PAH2","PAH3", "PCB_TOTAL")

# # Categories for Basic Summary
# basic_metals <- c("ARSENIC", "COPPER", "LEAD", "MERCURY", "ZINC")
# basic_pahs <- c("PAH1", "PAH2", "PAH3")
# basic_other <- c("CHLORDANE_TECHNICAL", "DDD", "DDE", "DDT", "DIELDRIN", "HEPTACHLOR_EPOXIDE", "PCB_TOTAL")



# ------------------------------------------------------------------------------
# 1. Lookup Criteria
# ------------------------------------------------------------------------------


ir_data <- lookup_criteria(ir_data)



# ------------------------------------------------------------------------------
# 2. Evaluate Criteria
# ------------------------------------------------------------------------------

criteria_results <- evaluate_criteria(ir_data)

write.csv(criteria_results, "output/criteria_results_20230523.csv")


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
my_basic_summary_10yr_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = "07/01/2011", end_date = "06/30/2021")

# 4b. Other (non Metal) parameters
my_basic_summary_10yr_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = "07/01/2011", end_date = "06/30/2021")

# 4c. Compile All Last 5 Year Summaries
my_basic_summary_10yr <- compile_basic_recent(my_basic_summary_10yr_metals, my_basic_summary_10yr_other)




# ------------------------------------------------------------------------------
# 5. Summarize Basic Recent Summary (Last 5 Years)
# ------------------------------------------------------------------------------

# 5a. Metals Only
my_basic_summary_5yr_metals <- summarize_basic_recent_metals(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

# 5b. Other (non Metal) parameters
my_basic_summary_5yr_other <- summarize_basic_recent(criteria_results = criteria_results, start_date = "07/01/2016", end_date = "06/30/2021")

# 5c. Compile All Last 5 Year Summaries
my_basic_summary_5yr <- compile_basic_recent(my_basic_summary_5yr_metals, my_basic_summary_5yr_other)




# ------------------------------------------------------------------------------
# 6. Compile Summaries
# ------------------------------------------------------------------------------

# Consolidate for Appendix B
my_compiled_summaries <- compile_summaries(my_basic_summary, my_basic_summary_10yr, my_basic_summary_5yr)


# ------------------------------------------------------------------------------
# 7. Create Decision Logic
# ------------------------------------------------------------------------------

# Class C
my_decision_logic_class_c <- create_decision_logic_class_c(my_compiled_summaries)


# Class D
my_decision_logic_class_d <- create_decision_logic_class_d(my_compiled_summaries)




# ------------------------------------------------------------------------------
# 8. Create Appendix B Tables
# ------------------------------------------------------------------------------

# 8a. Consolidate for Appendix B - Class C
my_appendix_b_class_c <- create_ir_appendix_b_class_c(my_decision_logic_class_c, five_year_start_date = "07/01/2016")
write.csv(my_appendix_b_class_c, file = "output/appendix_b_class_c_20230523.csv", row.names = F)

# 8b. Consolidate for Appendix B - Class D
my_appendix_b_class_d <- create_ir_appendix_b_class_d(my_decision_logic_class_d, five_year_start_date = "07/01/2016")
write.csv(my_appendix_b_class_d, file = "output/appendix_b_class_d_20230523.csv", row.names = F)

# 8c. Reconcile Class C and Class D decisions
my_appendix_b_reconciliation <- create_ir_appendix_b_reconciliation(my_decision_logic_class_c, my_decision_logic_class_d)
write.csv(my_appendix_b_reconciliation, file = "output/appendix_b_reconciliation_20230523.csv", row.names = F)

# 8d. Recreate Appendix B - Class C tables with merge formatting
my_appendix_b_class_c_merge <- create_ir_appendix_b_class_c_merge(my_appendix_b_class_c)

# 8e. Recreate Appendix B - Class D tables with merge formatting
my_appendix_b_class_d_merge <- create_ir_appendix_b_class_d_merge(my_appendix_b_class_d)






