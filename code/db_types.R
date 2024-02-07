# Definition of types in the DB

# Type conversions for 'element'
element_types <- list(
  element_id = as.integer,
  type = as.character,
  profile_id = as.integer,
  order_element = as.integer,
  upper_depth = as.numeric,
  lower_depth = as.numeric,
  specimen_id = as.integer,
  specimen_code = as.character
)
# Type conversions for 'observation_phys_chem'
observation_phys_chem_types <- list(
  observation_phys_chem_id = as.integer,
  property_phys_chem_id = as.integer,
  procedure_phys_chem_id = as.integer,
  unit_of_measure_id = as.integer,
  value_min = as.numeric,
  value_max = as.numeric,
  observation_phys_chem_r_label = as.character
)

# Type conversions for 'plot'
plot_types <- list(
  plot_id = as.integer,
  plot_code = as.character,
  site_id = as.integer,
  plot_type = as.character
  # Note: 'position' column processing for longitude and latitude extraction is to be handled separately
)

# Type conversions for 'procedure_phys_chem'
procedure_phys_chem_types <- list(
  procedure_phys_chem_id = as.integer,
  procedure_phys_chem_label = as.character,
  procedure_phys_chem_url = as.character
)

# Type conversions for 'profile'
profile_types <- list(
  profile_id = as.integer,
  profile_code = as.character,
  plot_id = as.integer
)

# Type conversions for 'project'
project_types <- list(
  project_id = as.integer,
  name = as.character
)

# Type conversions for 'property_phys_chem'
property_phys_chem_types <- list(
  property_phys_chem_id = as.integer,
  property_phys_chem_label = as.character,
  property_phys_chem_url = as.character
)

# Type conversions for 'result_phys_chem'
result_phys_chem_types <- list(
  result_phys_chem_id = as.integer,
  observation_phys_chem_id = as.integer,
  element_id = as.integer,
  value = as.numeric
)

# Type conversions for 'site'
site_types <- list(
  site_id = as.integer,
  site_code = as.character
  # Note: 'position' column processing for longitude and latitude extraction is to be handled separately
)

# Type conversions for 'site_project'
site_project_types <- list(
  site_id = as.integer,
  project_id = as.integer
)

# Type conversions for 'unit_of_measure'
unit_of_measure_types <- list(
  unit_of_measure_id = as.integer,
  unit_label = as.character,
  unit_url = as.character,
  unit_description = as.character
)


