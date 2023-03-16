## -----------------------------------------------------------------------------
##
## Workflow for formatting raw data
## Formatted data will be input for IRSupportR workflow
##
## -----------------------------------------------------------------------------


devtools::load_all()


# ------------------------------------------------------------------------------
# 0. Load Raw Data
# ------------------------------------------------------------------------------

# Define data types
column_types <- c(rep("text", times = 4), "date", rep("text", times = 7), "numeric", rep("text", times = 7), "numeric", rep("text", times = 7))

# Read in individual data sources
usgs_nwis <- readxl::read_excel("data/formatted_data/USGS_NWIS.xlsx", sheet = "Formatted", col_types = column_types)
usgs_trib_study <- readxl::read_excel("data/formatted_data/USGS_Tributary_Study.xlsx", sheet = "Formatted", col_types = column_types)
anacostia_sed <- readxl::read_excel("data/formatted_data/Anacostia_River_Sediment_Project.xlsm", sheet = "Formatted_All", col_types = column_types)
toxics_phase1 <- readxl::read_excel("data/formatted_data/District_of_Columbia_Toxic_Characterization_Phase_1.xlsx", sheet = "Formatted", col_types = column_types)
toxics_phase2 <- readxl::read_excel("data/formatted_data/District_of_Columbia_Toxic_Characterization_Phase_2.xlsm", sheet = "Appx-A Formatted", col_types = column_types)
doee_lab <- readxl::read_excel("data/formatted_data/DOEE_Ambiant_WQ_Program.xlsx", sheet = "Formatted - Target Metals", col_types = column_types)
background_study <- readxl::read_excel("data/formatted_data/DOEE_Background_Study.xlsx", sheet = "Formatted", col_types = column_types)


raw_data <- read_raw_data(usgs_nwis, usgs_trib_study, anacostia_sed, toxics_phase1, toxics_phase2, doee_lab, background_study)
