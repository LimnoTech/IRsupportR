## code to prepare `all_processed_data` dataset goes here

# -----------------------------
# Load libraries
# -----------------------------

# library(readxl)
# library(writexl)
# library(dplyr)
# library(tidyr)
# library(lubridate)

# -----------------------------
# Step 0 -- Import Data & Initial Processing
# -----------------------------

column_types <- c(rep("text", times = 4), "date", rep("text", times = 7), "numeric", rep("text", times = 7), "numeric", rep("text", times = 7))


usgs_nwis <- readxl::read_excel("data-raw/formatted_data/USGS_NWIS.xlsx", sheet = "Formatted", col_types = column_types)
usgs_trib_study <- readxl::read_excel("data-raw/formatted_data/USGS_Tributary_Study.xlsx", sheet = "Formatted", col_types = column_types)
anacostia_sed <- readxl::read_excel("data-raw/formatted_data/Anacostia_River_Sediment_Project.xlsm", sheet = "Formatted_All", col_types = column_types)
toxics_phase1 <- readxl::read_excel("data-raw/formatted_data/District_of_Columbia_Toxic_Characterization_Phase_1.xlsx", sheet = "Formatted", col_types = column_types)
toxics_phase2 <- readxl::read_excel("data-raw/formatted_data/District_of_Columbia_Toxic_Characterization_Phase_2.xlsm", sheet = "Appx-A Formatted", col_types = column_types)
doee_lab <- readxl::read_excel("data-raw/formatted_data/DOEE_Ambiant_WQ_Program.xlsx", sheet = "Formatted - Target Metals", col_types = column_types)
background_study <- readxl::read_excel("data-raw/formatted_data/DOEE_Background_Study.xlsx", sheet = "Formatted", col_types = column_types)


### compile_raw_data
# Combine datasets
df <- dplyr::bind_rows(usgs_nwis, usgs_trib_study, anacostia_sed, toxics_phase1, toxics_phase2, doee_lab, background_study)


# Convert all text columns to uppercase
df <- df %>%
  dplyr::mutate(dplyr::across(where(is.character), toupper))


writexl::write_xlsx(df,"output/compiled_raw_data.xlsx")

# -----------------------------
# Step 1 -- Filter & Format Date
#   01/01/1999 - 06/30/2021
# -----------------------------


### Use existing filter_by_date function
### split_date (do for day too?)
# Filter for analysis timeframe and create column for year and month
df <- df %>%
  dplyr::filter(sample_date >= '1990-01-01',
                sample_date <= '2021-06-30') %>%
  dplyr::mutate(year = lubridate::year(sample_date),
                month = lubridate::month(sample_date))




# -----------------------------
# Step 2 -- Filter Sample Type
# -----------------------------

### filter_out_qc_types
# Keep Duplicates: "FD", "FIELD DUP", "FIELD-DUP","DUP", "DUPLICATE"
# Remove QC blanks
qc_types <- c("QUALITY CONTROL SAMPLE-FIELD BLANK", "RB", "RINSATE BLANK")


df <- df %>%
  dplyr::filter(!sample_type %in% qc_types)

# Note: results for duplicates must contain location_id or data won't get analyzed


# -----------------------------
# Step 3 -- Assign Waterbody
# -----------------------------

df_wb <- readxl::read_excel("data-raw/reference_tables/Location_ID_waterbody_crosswalk_20230113.xlsx", sheet = "Import")

df_wb <- df_wb %>%
  dplyr::select(location_id, waterbody_segment) %>%
  dplyr::mutate_all(.funs = toupper)

# # **TEST** - delete later??
# print(paste("number of rows before assigning waterbody:", nrow(df)))
# df_test <- df %>%
#   left_join(df_wb, by="location_id") %>%
#   filter(is.na(waterbody_segment))
# print(paste("number of rows with no waterbody_segment matches:", nrow(df_test)))
# levels(as.factor(df_test$location_id))


# Join segments to main df
# Remove rows with blanks in waterbody_segment column
df <- df %>%
  dplyr::left_join(df_wb, by="location_id") %>%
  tidyr::drop_na(waterbody_segment)


# # **TEST** - delete later??
# print(paste("number of rows with waterbody_segment matches:", nrow(df)))
# levels(as.factor(df$waterbody_segment))

# -----------------------------
# Step 4-6 -- Filter Pollutants
# -----------------------------
#Test: warning should be produced if the df has an entry that is not in df_pollutant

df_pollutant <- readxl::read_excel("data-raw/reference_tables/Pollutant_Crosswalk_20230103.xlsx", sheet = "Unique Paramaters")

df_pollutant <- df_pollutant %>%
  dplyr::select(parameter, pollutant_name, pollutant_group)

# # **TEST** - delete later??
# df_pol_test_2 <- df %>%
#   left_join(df_pollutant, by="parameter") %>%
#   filter(is.na(pollutant_group))
# print(paste("number of rows with no pollutant_group match:", nrow(df_pol_test_2)))

# Join pollutants to main df
# Remove rows with blanks in pollutant_group column
df <- df %>%
  dplyr::left_join(df_pollutant, by="parameter") %>%
  tidyr::drop_na(pollutant_group)

# # **TEST** - delete later??
# df_pol_test <- df %>%
#   left_join(df_pollutant, by="parameter") %>%
#   filter(is.na(parameter))
# print(paste("number of rows with no parameter match:", nrow(df_pol_test)))
# # Above number should be zero if all parameters showed up in Lookup table
#
# print(paste("number of rows with pollutant_group matches:", nrow(df)))
# levels(as.factor(df$pollutant_group))


# -----------------------------
# Step 8 -- Convert Units
# -----------------------------
# Test: Should be no NAs in processed_result_value -- all original units found in dataset

df <- df %>%
  dplyr::mutate(processed_result_value = dplyr::case_when(result_unit == "MG/L" ~ result_value * 1000,
                                            result_unit == "NG/L" ~ result_value / 1000,
                                            result_unit == "UG/L" ~ result_value),
                processed_result_unit = dplyr::case_when(result_unit == "MG/L" ~ "UG/L",
                                                  result_unit == "NG/L" ~ "UG/L",
                                                  result_unit == "UG/L" ~ "UG/L"))

# -----------------------------
# Step 9 -- Determine Detect Status
# -----------------------------


# If qualifier contains a "U", non-detect. Otherwise detect.
df <- df %>%
  dplyr::mutate(processed_detect_status = dplyr::case_when(grepl("U", qualifier) ~ "ND",
                                             TRUE ~ "D"))


writexl::write_xlsx(df,"output/processed_data_before_pcb_sums.xlsx")

# -----------------------------
# Step 7 -- Calculate PCB Totals
# -----------------------------


# Create df_pcb by filtering df. Set NDs as zero.
df_pcb <- df %>%
  dplyr::filter(pollutant_group == "PCB") %>%
  dplyr::mutate(processed_pcb_value = dplyr::case_when(processed_detect_status == "D" ~ processed_result_value,
                                                       processed_detect_status == "ND" ~ 0))

# Remove PCB data from df -- Will later add back in as PCB Totals
df <- df %>%
  dplyr::filter(!pollutant_group == "PCB")

# Calculate PCB Total separately for each dataset

### Background Study: Formatted dataset already includes PCB totals. Captured in analysis as PCB_TOTAL in pollutant_name and pollutant_group fields

### Toxics Phase 1 - Results only available for 20 PCB congeners. Summing 20 congeners so it can be compared to PCB totals criteria.
df_pcb_tox_ph1 <- df_pcb %>%
  dplyr::filter(data_source == "DISTRICT OF COLUMBIA TOXIC CHARACTERIZATION, PHASE 1") %>%
  dplyr::group_by(data_source, sample_id, location_id, matrix, sample_date, sample_time, year, month, waterbody_segment, processed_result_unit) %>%
  dplyr::summarise(pcb_total_value = sum(processed_pcb_value)) %>%
  dplyr::mutate(processed_detect_status = dplyr::case_when(pcb_total_value == 0 ~  "ND",
                                             TRUE ~ "D")) %>%
  dplyr::ungroup()


### Anacostia River Sediment Project
# Groups co-eluting congeners. All 209 congeners are represented in 162 PCB parameters. Sum all 162 parameters.
# TEST - number of samples in df_pcb_ana_sed should be same as if just grouped by sample_id

df_pcb_ana_sed <- df_pcb %>%
  dplyr::filter(data_source == "ANACOSTIA RIVER SEDIMENT PROJECT") %>%
  dplyr::group_by(data_source, sample_id, location_id, matrix, sample_date, sample_time, year, month, waterbody_segment, processed_result_unit) %>%
  dplyr::summarise(pcb_total_value = sum(processed_pcb_value)) %>%
  dplyr::mutate(processed_detect_status = dplyr::case_when(pcb_total_value == 0 ~  "ND",
                                             TRUE ~ "D")) %>%
  dplyr::ungroup()


# TEST - number of columns should be same as two input tables. Number of rows should be sum of two input tables
df_pcb_total <- df_pcb_tox_ph1 %>%
  dplyr::bind_rows(df_pcb_ana_sed) %>%
  dplyr::mutate(pollutant_name = "PCB_TOTAL",
                pollutant_group = "PCB_TOTAL") %>%
  dplyr::rename(processed_result_value = pcb_total_value)



# Merge Data back with main df
# TEST - number of columns of df should remain unchanged. Number of rows should be sum of df and df_pcb_total rows
all_processed_data <- df %>%
  dplyr::bind_rows(df_pcb_total)

# writexl::write_xlsx(all_processed_data,"output/final_processed_data_20230125.xlsx")
# write.csv(all_processed_data, "output/final_processed_data_20230125.csv")




usethis::use_data(all_processed_data, overwrite = TRUE)
