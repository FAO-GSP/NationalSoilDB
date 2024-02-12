# The script creates the SQL tables with defined types
# Then Populate the SQL tables with data stores in dataframes

library('RPostgreSQL')
library('dplyr')

dsn_database = "carsis"   # Specify the name of your Database
dsn_hostname = "localhost"  
dsn_port = "5432"                # Specify your port number
dsn_uid = "luislado"         # Specify your username. e.g. "admin"
dsn_pwd = ""        # Specify your password

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("connecting to Database…")
  con <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
  print("Database conected!")
},
error=function(cond) {
  print("Unable to conect to Database.")
})


## SQL CODE TO CREATE TABLES----
source("code/create_tables.R")


## Fetch data
project.df <- dbGetQuery(con, "SELECT * FROM project"); project.df
site.df <- dbGetQuery(con, "SELECT * FROM site"); site.df # Los datos los he añadido con el tibble de más abajo
plot.df <- dbGetQuery(con, "SELECT * FROM plot"); plot.df 
site_project.df <- dbGetQuery(con, "SELECT * FROM site_project"); site_project.df
profile.df <- dbGetQuery(con, "SELECT * FROM profile"); profile.df
element.df <- dbGetQuery(con, "SELECT * FROM element"); element.df
unit_of_measure.df <- dbGetQuery(con, "SELECT * FROM unit_of_measure"); unit_of_measure.df
procedure_phys_chem.df <- dbGetQuery(con, "SELECT * FROM procedure_phys_chem"); procedure_phys_chem.df
property_phys_chem.df <- dbGetQuery(con, "SELECT * FROM property_phys_chem"); property_phys_chem.df
observation_phys_chem.df <- dbGetQuery(con, "SELECT * FROM observation_phys_chem"); observation_phys_chem.df

result_phys_chem.df <- dbGetQuery(con, "SELECT * FROM result_phys_chem"); result_phys_chem.df
glosis_procedures.df <- dbGetQuery(con, "SELECT * FROM glosis_procedures"); glosis_procedures.df

### ADD DATA TO TABLES----

## EXAMPLE WITH A TIBBLE
site_tibble <- tibble(
  site_id = c(1, 2),
  site_code = c("SiteA", "SiteB"),
  plot_id = c(101, 102),
  plot_code = c("PlotA", "PlotB"),
  plot_type = c("TypeA", "TypeB"), # Se lo ha añadido, faltaba en el site_tibble
  profile_id = c(1001, 1002),
  profile_code = c("ProfileA", "ProfileB"),
  longitude = c(-123.3656, -122.6784),
  latitude = c(48.4284, 47.4944),
  position = c("POINT(-123.3656 48.4284)", "POINT(-122.6784 47.4944)"), # Example WKT format
  project_id = c(1, 2),
  project_name = c("ProjectA", "ProjectB")
)

#site_tibble <- read.csv("site_test.csv")
site_tibble

# Assuming you have a function to safely execute SQL commands
safeExecute <- function(con, query) {
  tryCatch({
    dbSendQuery(con, query)
  }, error = function(e) {
    cat("Error in executing SQL: ", e$message, "\n")
  })
}

# Insert data into the 'project' table
unique_projects <- unique(site_tibble[, c("project_id", "project_name")])
for (row in 1:nrow(unique_projects)) {
  query <- sprintf("INSERT INTO project (project_id, name) VALUES (%d, '%s') ON CONFLICT (project_id) DO NOTHING;",
                   unique_projects$project_id[row], unique_projects$project_name[row])
  safeExecute(con, query)
}

# Insert data into the 'site' table
# Assuming 'position' is generated from 'longitude' and 'latitude', and these fields exist in your 'site' table
unique_sites <- unique(site_tibble[, c("site_id", "site_code", "longitude", "latitude")])
for (row in 1:nrow(unique_sites)) {
  query <- sprintf("INSERT INTO site (site_id, site_code, location) VALUES (%d, '%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT (site_id) DO NOTHING;",
                   unique_sites$site_id[row], unique_sites$site_code[row], unique_sites$longitude[row], unique_sites$latitude[row])
  safeExecute(con, query)
}

# Insert data into the 'plot' table
unique_plots <- unique(site_tibble[, c("plot_id", "plot_code", "site_id", "plot_type")])
for (i in 1:nrow(unique_plots)) {
  row <- unique_plots[i, ]
  # SQL command to insert data, avoiding duplicates using ON CONFLICT DO NOTHING
  query <- sprintf(
    "INSERT INTO plot (plot_id, plot_code, site_id, plot_type) VALUES (%d, '%s', %d, '%s') ON CONFLICT (plot_id) DO NOTHING;",
    row$plot_id, row$plot_code, row$site_id, row$plot_type
  )
  # Execute the SQL command
  safeExecute(con, query)
}

# Insert data into the 'site_project' table
unique_sites <- unique(site_tibble[, c("site_id", "project_id")])
# Insert data
for (i in 1:nrow(unique_sites)) {
  # Prepare the SQL INSERT statement
  query <- sprintf("INSERT INTO site_project (site_id, project_id) VALUES (%s, %s) ON CONFLICT DO NOTHING;",
                   unique_sites$site_id[i], unique_sites$project_id[i])
  # Execute the SQL command
  safeExecute(con, query)
}

# Insert data into the 'profile' table
unique_sites <- unique(site_tibble[, c("profile_id", "profile_code", "plot_id")])
# Insert data
for (i in 1:nrow(unique_sites)) {
  query <- sprintf("INSERT INTO profile (profile_id, profile_code, plot_id) VALUES (%d, '%s', %d) ON CONFLICT DO NOTHING;",
                   unique_sites$profile_id[i], unique_sites$profile_code[i], unique_sites$plot_id[i])
  safeExecute(con, query)
}

# Insert data into the 'element' table
unique_sites <- unique(site_tibble[c("profile_id")])
for (i in 1:nrow(unique_sites)) {
  # Prepare the SQL INSERT statement
  query <- sprintf("INSERT INTO element (profile_id) VALUES (%d) ON CONFLICT (element_id) DO NOTHING;",
    unique_sites$profile_id[i])
  # Execute the SQL INSERT statement
  safeExecute(con, query)
}

# Insert data into the 'unit_of_measure' table
# unique_sites <- unique(site_tibble[, c("unit_of_measure_id", "label", "description", "url")])
# for (i in 1:nrow(unit_of_measure_tibble)) {
#   # Prepare the SQL INSERT statement
#   query <- sprintf(
#     "INSERT INTO unit_of_measure (label, description, url) VALUES ('%s', '%s', '%s') ON CONFLICT (unit_of_measure_id) DO NOTHING;",
#     unit_of_measure_tibble$label[i], 
#     unit_of_measure_tibble$description[i], 
#     unit_of_measure_tibble$url[i]
#   )
#   # Execute the SQL INSERT statement
#   safeExecute(con, query)
# }

# # Insert data into the 'procedure_phys_chem' table
# unique_sites <- unique(site_tibble[, c("procedure_phys_chem_id", "label", "url")])
# for (i in 1:nrow(unique_sites)) {
#   # Prepare the SQL INSERT statement
#   query <- sprintf(
#     "INSERT INTO procedure_phys_chem (procedure_phys_chem_id, label, url) VALUES (%d, '%s', '%s') ON CONFLICT (procedure_phys_chem_id) DO NOTHING;",
#     unique_sites$procedure_phys_chem_id[i], 
#     unique_sites$label[i], 
#     unique_sites$url[i]
#   )
#   
#   # Execute the SQL INSERT statement
#   safeExecute(con, query)
# }

# # Insert data into the 'property_phys_chem_id' table
# unique_sites <- unique(site_tibble[, c("property_phys_chem_id", "label", "url")])
# for (i in 1:nrow(unique_sites)) {
#   # Prepare the SQL INSERT statement
#   query <- sprintf("INSERT INTO property_phys_chem_id (property_phys_chem_id, label, url) VALUES (%d, '%s', '%s') ON CONFLICT (property_phys_chem_id) DO NOTHING;",
#                    unique_sites$property_phys_chem_id[i], 
#                    unique_sites$label[i], 
#                    unique_sites$url[i]
#   )
#   
#   # Execute the SQL INSERT statement
#   safeExecute(con, query)
# }

  
# # Insert data into the 'observation_phys_chem' table
# unique_sites <- unique(site_tibble[c("observation_phys_chem_id","property_phys_chem_id","procedure_phys_chem_id","unit_of_measure_id","value_min","value_max","observation_phys_chem_r_label")])
# for (i in 1:nrow(unique_sites)) {
#   # Prepare the SQL INSERT statement
#   query <- sprintf(
#     "INSERT INTO observation_phys_chem (property_phys_chem_id, procedure_phys_chem_id, unit_of_measure_id, value_min, value_max, observation_phys_chem_r_label) VALUES (%d, %d, %d, %d, %d, %d,'%s') ON CONFLICT (observation_phys_chem_id) DO NOTHING;",
#     unique_sites$property_phys_chem_id[i], 
#     unique_sites$procedure_phys_chem_id[i],
#     unique_sites$unit_of_measure_id[i],
#     unique_sites$value_min[i], 
#     unique_sites$value_max[i],
#     unique_sites$observation_phys_chem_r_label[i]
#   )
#   # Execute the SQL INSERT statement
#   safeExecute(con, query)
# }

# # Insert data into the 'result_phys_chem' table
# unique_sites <- unique(site_tibble[c("result_phys_chem_id", "observation_phys_chem_id","element_id","value")])
# for (i in 1:nrow(result_phys_chem_tibble)) {
#   # Prepare the SQL INSERT statement
#   query <- sprintf(
#     "INSERT INTO result_phys_chem (observation_phys_chem_id, element_id, value) VALUES (%d, %d, %f) ON CONFLICT (result_phys_chem_id) DO NOTHING;",
#     result_phys_chem_tibble$observation_phys_chem_id[i], 
#     result_phys_chem_tibble$element_id[i],
#     result_phys_chem_tibble$value[i]
#   )
#   
#   # Execute the SQL INSERT statement
#   safeExecute(con, query)
# }



# # Insert data into the 'glosis_procedures' table
# unique_sites <- unique(site_tibble[c("result_phys_chem_id", "observation_phys_chem_id","element_id","value")])
# for (i in 1:nrow(unique_sites)) {
#   # Prepare the SQL INSERT statement
#   query <- sprintf(
#     "INSERT INTO glosis_procedures (name, description) VALUES ('%s', '%s', ) ON CONFLICT (procedure_id) DO NOTHING;",
#     unique_sites$name[i], 
#     unique_sites$description[i]
#   )
#   # Execute the SQL INSERT statement
#   safeExecute(con, query)
# }



# Fetch data
df <- dbGetQuery(con, "SELECT * FROM site"); df # Los datos los he añadido con el tibble csv
df <- dbGetQuery(con, "SELECT * FROM plot"); df
df <- dbGetQuery(con, "SELECT * FROM project"); df
df <- dbGetQuery(con, "SELECT * FROM site_project"); df


# Close the connection
dbDisconnect(con)


### END OF SCRIPT



