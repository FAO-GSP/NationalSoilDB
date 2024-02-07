
# Load the required libraries
library(tidyverse) # Includes necessary data manipulation functions
library(sf)        # For spatial data frames
library(xlsx)  # For reading Excel files, assuming 'read.xlsx2' function intention

# Set WD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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

# Extract longitude and latitude from the `position` column for the `site` and `plot` data frames
# Assuming position is stored in WKT format and needs to be parsed
# Here is the example for the `site` data frame, apply a similar approach for `plot`
site <- site %>%
  mutate(longitude = numeric(),
         latitude = numeric()
  ) %>% 
  select(-position)

# Create the 'site' tibble by joining the necessary tables
site_tibble <- site %>%
  left_join(site_project, by = "site_id") %>%
  left_join(project, by = "project_id") %>%
  left_join(plot, by = "site_id") %>%
  left_join(profile, by = "plot_id") %>%
  select(site_id, site_code, plot_id, plot_code, profile_id, profile_code, 
         longitude, latitude, position, project_id, project_name=name)

# Create the 'horizons' tibble by joining the necessary tables
horizon_tibble <- profile %>%
  full_join(element, by = "profile_id") %>%
  full_join(result_phys_chem, by = "element_id") %>%
  full_join(observation_phys_chem, by = "observation_phys_chem_id") %>%
  full_join(property_phys_chem, by = "property_phys_chem_id") %>%  
  select(profile_id, profile_code, element_id, type, order_element, upper_depth, 
         lower_depth, specimen_id, specimen_code, result_phys_chem_id,
         observation_phys_chem_r_label, value)

horizon_tibble <- horizon_tibble %>% 
  # Spread the label_property values into separate columns
  pivot_wider(names_from = observation_phys_chem_r_label, values_from = value)

horizon_tibble <- horizon_tibble[-1,]


xlsx::write.xlsx(x = site_tibble, file = "data_input/template.xlsx", 
                 sheetName = "site_template", append = TRUE, row.names=FALSE)
xlsx::write.xlsx(x = horizon_tibble, file = "data_input/template.xlsx", 
                 sheetName = "horizon_template", append = TRUE, row.names=FALSE)
