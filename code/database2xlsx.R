
# Load the required libraries
library(tidyverse) # Includes necessary data manipulation functions
library(sf)        # For spatial data frames
library(xlsx)  # For reading Excel files, assuming 'read.xlsx2' function intention

# Set WD
setwd("~/Dropbox/Github/NationalSoilDB-main/code")
setwd("../")

# Define the path to the Excel file
path_to_excel <- 'data_input/template.xlsx'

# Load DB types
source('code/db_types.R')

# Function for loading and converting types for 'each' sheet
load_and_convert_types <- function(path, sheet_name, type_conversions) {
  data <- read.xlsx2(path, sheetName = sheet_name)
  for (col_name in names(type_conversions)) {
    data[[col_name]] <- type_conversions[[col_name]](data[[col_name]])
  }
  return(data)
}

# Load and convert types for 'element' sheet
element <- load_and_convert_types(path_to_excel, 'element', element_types)
observation_phys_chem <- load_and_convert_types(path_to_excel, 'observation_phys_chem', observation_phys_chem_types)
plot <- load_and_convert_types(path_to_excel, 'plot', plot_types)
procedure_phys_chem <- load_and_convert_types(path_to_excel, 'procedure_phys_chem', procedure_phys_chem_types)
profile <- load_and_convert_types(path_to_excel, 'profile', profile_types)
project <- load_and_convert_types(path_to_excel, 'project', project_types)
property_phys_chem <- load_and_convert_types(path_to_excel, 'property_phys_chem', property_phys_chem_types)
site <- load_and_convert_types(path_to_excel, 'site', site_types)
site_project <- load_and_convert_types(path_to_excel, 'site_project', site_project_types)
unit_of_measure <- load_and_convert_types(path_to_excel, 'unit_of_measure', unit_of_measure_types)



