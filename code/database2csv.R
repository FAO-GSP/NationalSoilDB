setwd("C:/GIT/NationalSoilDB/csv_files/")
list.files(path = 'C:/GIT/NationalSoilDB/csv_files/', ".csv", full.names = TRUE)
rm(list = ls())

# Load the required libraries
library(tidyverse)
library(readr)
library(sf)

# Define the path to the CSV files
path_to_csv <- 'C:/GIT/NationalSoilDB/csv_files/'

# Load the CSV files into data frames with type conversions
element <- read_csv(paste0(path_to_csv, 'element.csv')) %>%
  mutate(
    element_id = as.integer(element_id),
    type = as.character(type),
    profile_id = as.integer(profile_id),
    order_element = as.integer(order_element),
    upper_depth = as.numeric(upper_depth),
    lower_depth = as.numeric(lower_depth),
    specimen_id = as.integer(specimen_id),
    specimen_code = as.character(specimen_code)
  )
names(element)

observation_phys_chem <- read_csv(paste0(path_to_csv, 'observation_phys_chem.csv')) %>%
  mutate(
    observation_phys_chem_id = as.integer(observation_phys_chem_id),
    property_phys_chem_id = as.integer(property_phys_chem_id),
    procedure_phys_chem_id = as.integer(procedure_phys_chem_id),
    unit_of_measure_id = as.integer(unit_of_measure_id),
    value_min = as.numeric(value_min),
    value_max = as.numeric(value_max),
    observation_phys_chem_r_label = as.character(observation_phys_chem_r_label)
  )
names(observation_phys_chem)

plot <- read_csv(paste0(path_to_csv, 'plot.csv')) %>%
  mutate(
    plot_id = as.integer(plot_id),
    plot_code = as.character(plot_code),
    site_id = as.integer(site_id),
    plot_type = as.character(plot_type)
    # position will be processed for longitude and latitude extraction later
  )
names(plot)
plot <- select(plot, -position)

procedure_phys_chem <- read_csv(paste0(path_to_csv, 'procedure_phys_chem.csv')) %>%
  mutate(
    procedure_phys_chem_id = as.integer(procedure_phys_chem_id),
    procedure_phys_chem_label = as.character(procedure_phys_chem_label),
    procedure_phys_chem_url = as.character(procedure_phys_chem_url)
  )


profile <- read_csv(paste0(path_to_csv, 'profile.csv')) %>%
  mutate(
    profile_id = as.integer(profile_id),
    profile_code = as.character(profile_code),
    plot_id = as.integer(plot_id)
  )
names(profile)

project <- read_csv(paste0(path_to_csv, 'project.csv')) %>%
  mutate(
    project_id = as.integer(project_id),
    name = as.character(name)
  )
names(project)

property_phys_chem <- read_csv(paste0(path_to_csv, 'property_phys_chem.csv')) %>%
  mutate(
    property_phys_chem_id = as.integer(property_phys_chem_id),
    property_phys_chem_label = as.character(property_phys_chem_label),
    property_phys_chem_url= as.character(property_phys_chem_url)
  )
names(property_phys_chem)

result_phys_chem <- read_csv(paste0(path_to_csv, 'result_phys_chem.csv')) %>%
  mutate(
    result_phys_chem_id = as.integer(result_phys_chem_id),
    observation_phys_chem_id = as.integer(observation_phys_chem_id),
    element_id = as.integer(element_id),
    value = as.numeric(value)
  )
names(result_phys_chem)

site <- read_csv(paste0(path_to_csv, 'site.csv')) %>%
  mutate(
    site_id = as.integer(site_id),
    site_code = as.character(site_code)
    # position will be processed for longitude and latitude extraction later
  )
names(site)

site_project <- read_csv(paste0(path_to_csv, 'site_project.csv')) %>%
  mutate(
    site_id = as.integer(site_id),
    project_id = as.integer(project_id)
  )
names(site_project)

unit_of_measure <- read_csv(paste0(path_to_csv, 'unit_of_measure.csv')) %>%
  mutate(
    unit_of_measure_id = as.integer(unit_of_measure_id),
    unit_label = as.character(unit_label),
    unit_url = as.character(unit_url),
    unit_description = as.character(unit_description)
  )
names(unit_of_measure)

# Extract longitude and latitude from the `position` column for the `site` and `plot` data frames
# Assuming position is stored in WKT format and needs to be parsed
# Here is the example for the `site` data frame, apply a similar approach for `plot`
site <- site %>%
  mutate(longitude = numeric(),
         latitude = numeric()
  )

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

write_csv(site_tibble, "../data_output/site_glosis.csv")
write_csv(horizon_tibble, "../data_output/horizon_glosis.csv")
