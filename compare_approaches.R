## -----------------------------------------------------------------------------
##
## IR Assessment Comparison
## Choose two datasets to compare results
##
## -----------------------------------------------------------------------------



library(dplyr)
library(openxlsx)


# ------------------------------------------------------------------------------
# 0. Identify Files for Comparison
# ------------------------------------------------------------------------------


# Dataset 1
label_1 = "2024"
file_c_1 <- "output/results_class_c.csv"
file_d_1 <- "output/results_class_d.csv"


# Dataset 2
label_2 = "2022"
file_c_2 <- "output/Results_for_2022_Integrated_Report/results_class_c.csv"
file_d_2 <- "output/Results_for_2022_Integrated_Report/results_class_d.csv"


# ------------------------------------------------------------------------------
# 1. Run Comparison
# ------------------------------------------------------------------------------


# Load Data
results_c_1 <- read.csv(file_c_1) %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction",
         c_decision_description = "Reevaluation.Categorization.Decision.for.Class.C",
         c_decision_case_number = "Decision.Logic.Case..")
results_d_1 <- read.csv(file_d_1) %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction",
         d_decision_description = "Reevaluation.Categorization.Decision.for.Class.D",
         d_decision_case_number = "Decision.Logic.Case..")
results_c_2 <- read.csv(file_c_2) %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction.",
         c_decision_description = "Reevaluation.Categorization.Decision.for.Class.C",
         c_decision_case_number = "Decision.Logic.Case..")
results_d_2 <- read.csv(file_d_2) %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction.",
         d_decision_description = "Reevaluation.Categorization.Decision.for.Class.D",
         d_decision_case_number = "Decision.Logic.Case..")


# Define column Names
col_case_c_1 <- paste0("c_decision_case_number_", label_1)
col_case_d_1 <- paste0("d_decision_case_number_", label_1)
col_case_c_2 <- paste0("c_decision_case_number_", label_2)
col_case_d_2 <- paste0("d_decision_case_number_", label_2)

col_descr_c_1 <- paste0("c_decision_description_", label_1)
col_descr_d_1 <- paste0("d_decision_description_", label_1)
col_descr_c_2 <- paste0("c_decision_description_", label_2)
col_descr_d_2 <- paste0("d_decision_description_", label_2)


# Compare two datasets

compare_c <- results_c_2 %>%
  left_join(results_c_1, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction"), suffix = c(paste0("_", label_2), paste0("_", label_1))) %>%
  mutate(same_case_number = case_when(.data[[col_case_c_2]] == .data[[col_case_c_1]] ~ "Yes",
                                      TRUE ~ "No"),
         same_decision_description = case_when(.data[[col_descr_c_2]] == .data[[col_descr_c_1]] ~ "Yes",
                                        TRUE ~ "No"))


compare_d <- results_d_2 %>%
  left_join(results_d_1, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction"), suffix = c(paste0("_", label_2), paste0("_", label_1))) %>%
  mutate(same_case_number = case_when(.data[[col_case_d_2]] == .data[[col_case_d_1]] ~ "Yes",
                                      TRUE ~ "No"),
         same_decision_description = case_when(.data[[col_descr_d_2]] == .data[[col_descr_d_1]] ~ "Yes",
                                               TRUE ~ "No"))


# datasets <- list('Class C - 2022 vs 2024' = compare_c, 'Class D - 2022 vs 2024' = compare_d)
# datasets <- list(paste0("Class C - ", label_2, " vs ", label_1) = compare_c, paste0("Class D - ", label_2, " vs ", label_1) = compare_d)

datasets <- list(compare_c, compare_d)
names(datasets) <- c(
  paste0("Class C - ", label_2, " vs ", label_1),
  paste0("Class D - ", label_2, " vs ", label_1)
)
write.xlsx(datasets, file='output/results_comparison.xlsx')



