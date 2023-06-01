
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IRsupportR

<!-- badges: start -->
<!-- badges: end -->

The objective of this package is to compile and evaluate historic water
quality data to verify if the data supports impairments attributed to
toxics. In order to accomplish this objective, data from various
datasets are compiled and processed into one consistent dataset for
comparing to numeric water quality criteria. Exceedances of the water
quality criteria are determined and categorization reevaluation
decisions are proposed based on the implementation of an approved
decision logic.

## Installation

You can install the development version of IRsupportR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LimnoTech/IRsupportR")
```

The repository can also be downloaded as a zip file by clicking on the
green ‘code’ dropdown button.

## Using the Package

This package contains several functions that encompass the analysis for
reevaluating toxic impairments in District of Columbia waterbodies.

### Data Processing

The original formatted data used for this analysis can be found in the
[data-raw](https://github.com/LimnoTech/IRsupportR/tree/main/data-raw/formatted_data)
folder. Data was compiled for seven different datasets and formatting
was standardized across the datasets. The
[all_processed_data.R](https://github.com/LimnoTech/IRsupportR/blob/main/data-raw/all_processed_data.R)
script is used to further process the data. The final output of the
script is saved as all_processed_data.rda in the
[data](https://github.com/LimnoTech/IRsupportR/tree/main/data) folder.

### Lookup Tables

In addition to the input data, the
[data](https://github.com/LimnoTech/IRsupportR/tree/main/data) folder
contains various lookup tables used throughout the analysis. The package
utilizes the .rda files. For ease of reference, .xlsx files have also
been included for each lookup table.

### Data Analysis

The steps for the data analysis can be found in the
[worflow.R](https://github.com/LimnoTech/IRsupportR/blob/main/workflow.r)
file. Source the contents of this document to complete the analysis.
Functions are run to evaluate criteria, perform summary statistics,
execute decision logic, and compile results tables.

### Outputs

Multiple tables and spreadsheets are produced from the analysis and
saved in the
[output](https://github.com/LimnoTech/IRsupportR/tree/main/output)
folder. They include the following:

- Intermediate results with criteria evaluated

  - criteria_results.csv

- Final results as CSVs

  - results_class_c.csv

  - results_class_d.csv

- Final results with merged cell formatting. Excel files required for
  merged cells. Csv files used for parts with no merged cells

  - Class C

    - results_class_c\_merge_part1.xlsx

    - results_class_c\_merge_part2.xlsx

    - results_class_c\_merge_part3.csv

  - Class D

    - results_class_d\_merge_part1.xlsx

    - results_class_d\_merge_part2.csv

- Final results as combined Excel spreadsheets. Includes merged cell
  formatting. Combined manually

  - manual_merge_parts/results_class_c\_merged_all.xlsx
  - merge_parts_manual/results_class_d\_merged_all.xlsx
