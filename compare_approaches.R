
library(dplyr)
library(openxlsx)

# Load Data
res_2024_ir_c <- read.csv("output/results_class_c.csv") %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction",
         c_decision_description = "Reevaluation.Categorization.Decision.for.Class.C",
         c_decision_case_number = "Decision.Logic.Case..")
res_2024_ir_d <- read.csv("output/results_class_d.csv") %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction",
         d_decision_description = "Reevaluation.Categorization.Decision.for.Class.D",
         d_decision_case_number = "Decision.Logic.Case..")
res_2022_ir_c <- read.csv("output/Results_for_2022_Integrated_Report/results_class_c.csv") %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction.",
         c_decision_description = "Reevaluation.Categorization.Decision.for.Class.C",
         c_decision_case_number = "Decision.Logic.Case..")
res_2022_ir_d <- read.csv("output/Results_for_2022_Integrated_Report/results_class_d.csv") %>%
  select(waterbody_segment = "Waterbody",
         pollutant_name = "Pollutant",
         pollutant_group = "Pollutant.Group",
         test_fraction = "Test.Fraction.",
         d_decision_description = "Reevaluation.Categorization.Decision.for.Class.D",
         d_decision_case_number = "Decision.Logic.Case..")



# Compare 2022 vs. 2024

compare_c <- res_2022_ir_c %>%
  left_join(res_2024_ir_c, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction"), suffix = c("_2022", "_2024")) %>%
  mutate(same_case_number = case_when(c_decision_case_number_2022 == c_decision_case_number_2024 ~ "Yes",
                                      TRUE ~ "No"),
         same_decision_description = case_when(c_decision_description_2022 == c_decision_description_2024 ~ "Yes",
                                        TRUE ~ "No"))


compare_d <- res_2022_ir_d %>%
  left_join(res_2024_ir_d, by = c("waterbody_segment", "pollutant_name", "pollutant_group", "test_fraction"), suffix = c("_2022", "_2024")) %>%
  mutate(same_case_number = case_when(d_decision_case_number_2022 == d_decision_case_number_2024 ~ "Yes",
                                      TRUE ~ "No"),
         same_decision_description = case_when(d_decision_description_2022 == d_decision_description_2024 ~ "Yes",
                                               TRUE ~ "No"))


datasets <- list('Class C - 2022 vs 2024' = compare_c, 'Class D - 2022 vs 2024' = compare_d)
write.xlsx(datasets, file='output/results_comparison.xlsx')



