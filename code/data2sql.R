# The script creates the SQL tables with defined types
# Then Populate the SQL tables with data stores in dataframes

library('RPostgreSQL')

dsn_database = "carsis"   # Specify the name of your Database
dsn_hostname = "localhost"  
dsn_port = "5432"                # Specify your port number. e.g. 98939
dsn_uid = "user"         # Specify your username. e.g. "admin"
dsn_pwd = ""        # Specify your password. e.g. "xxx"

tryCatch({
  drv <- dbDriver("PostgreSQL")
  print("Connecting to Database…")
  conn <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
  print("Database Connected!")
},
error=function(cond) {
  print("Unable to connect to Database.")
})

# Add POSTGIS
dbSendQuery(conn, "CREATE EXTENSION IF NOT EXISTS postgis;")

# Fetch data
df <- dbGetQuery(conn, "SELECT * FROM plot");df # Los datos los he añadido con el tibble de más abajo



## SQL CODE TO CREATE TABLES----
## Table:  project
create_table_query <- "
CREATE TABLE IF NOT EXISTS project (
  project_id SERIAL PRIMARY KEY,
  name VARCHAR(255),
  description TEXT
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  site
create_table_query <- " CREATE TABLE IF NOT EXISTS site (
  site_id SERIAL PRIMARY KEY,
  site_code VARCHAR(255),
  location GEOGRAPHY(Point)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  plot
create_table_query <- " CREATE TABLE IF NOT EXISTS plot (
  plot_id SERIAL PRIMARY KEY,
  plot_code VARCHAR(255),
  site_id INTEGER NOT NULL,
  plot_type VARCHAR(255),
  FOREIGN KEY (site_id) REFERENCES site(site_id)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  profile
create_table_query <- " CREATE TABLE IF NOT EXISTS profile (
  profile_id SERIAL PRIMARY KEY,
  profile_code VARCHAR(255),
  plot_id INTEGER NOT NULL,
  FOREIGN KEY (plot_id) REFERENCES plot(plot_id)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  unit_of_measure
create_table_query <- " CREATE TABLE IF NOT EXISTS unit_of_measure (
  unit_of_measure_id SERIAL PRIMARY KEY,
  label VARCHAR(255),
  description TEXT,
  url VARCHAR(255)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  property_phys_chem
create_table_query <- " CREATE TABLE IF NOT EXISTS property_phys_chem (
  property_phys_chem_id SERIAL PRIMARY KEY,
  label VARCHAR(255),
  url VARCHAR(255)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  procedure_phys_chem
create_table_query <- " CREATE TABLE IF NOT EXISTS procedure_phys_chem (
  procedure_phys_chem_id SERIAL PRIMARY KEY,
  label VARCHAR(255),
  url VARCHAR(255)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  glosis_procedures
create_table_query <- " CREATE TABLE IF NOT EXISTS glosis_procedures (
  procedure_id SERIAL PRIMARY KEY,
  name VARCHAR(255),
  description TEXT
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  element
create_table_query <- " CREATE TABLE IF NOT EXISTS element (
  element_id SERIAL PRIMARY KEY,
  type VARCHAR(255),
  profile_id INTEGER NOT NULL,
  order_element INTEGER,
  upper_depth NUMERIC,
  lower_depth NUMERIC,
  specimen_id INTEGER,
  specimen_code VARCHAR(255),
  FOREIGN KEY (profile_id) REFERENCES profile(profile_id)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  observation_phys_chem
create_table_query <- " CREATE TABLE IF NOT EXISTS observation_phys_chem (
  observation_phys_chem_id SERIAL PRIMARY KEY,
  property_phys_chem_id INTEGER NOT NULL,
  procedure_phys_chem_id INTEGER NOT NULL,
  unit_of_measure_id INTEGER NOT NULL,
  value_min NUMERIC,
  value_max NUMERIC,
  observation_phys_chem_r_label VARCHAR(255),
  FOREIGN KEY (property_phys_chem_id) REFERENCES property_phys_chem(property_phys_chem_id),
  FOREIGN KEY (procedure_phys_chem_id) REFERENCES procedure_phys_chem(procedure_phys_chem_id),
  FOREIGN KEY (unit_of_measure_id) REFERENCES unit_of_measure(unit_of_measure_id)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  result_phys_chem
create_table_query <- " CREATE TABLE IF NOT EXISTS result_phys_chem (
  result_phys_chem_id SERIAL PRIMARY KEY,
  observation_phys_chem_id INTEGER NOT NULL,
  element_id INTEGER NOT NULL,
  value NUMERIC,
  FOREIGN KEY (observation_phys_chem_id) REFERENCES observation_phys_chem(observation_phys_chem_id),
  FOREIGN KEY (element_id) REFERENCES element(element_id)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

## Table:  site_project
create_table_query <- " CREATE TABLE IF NOT EXISTS site_project (
  site_id INTEGER NOT NULL,
  project_id INTEGER NOT NULL,
  PRIMARY KEY (site_id, project_id),
  FOREIGN KEY (site_id) REFERENCES site(site_id),
  FOREIGN KEY (project_id) REFERENCES project(project_id)
);
"
# Execute the SQL command
dbSendQuery(conn, create_table_query)

### END OF SCRIPT


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

# Assuming you have a function to safely execute SQL commands
safeExecute <- function(conn, query) {
  tryCatch({
    dbSendQuery(conn, query)
  }, error = function(e) {
    cat("Error in executing SQL: ", e$message, "\n")
  })
}

# Insert data into the 'projects' table
unique_projects <- unique(site_tibble[, c("project_id", "project_name")])
for (row in 1:nrow(unique_projects)) {
  query <- sprintf("INSERT INTO project (project_id, name) VALUES (%d, '%s') ON CONFLICT (project_id) DO NOTHING;",
                   unique_projects$project_id[row], unique_projects$project_name[row])
  safeExecute(conn, query)
}

# Insert data into the 'site' table
# Assuming 'position' is generated from 'longitude' and 'latitude', and these fields exist in your 'site' table
unique_sites <- unique(site_tibble[, c("site_id", "site_code", "longitude", "latitude")])
for (row in 1:nrow(unique_sites)) {
  query <- sprintf("INSERT INTO site (site_id, site_code, location) VALUES (%d, '%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT (site_id) DO NOTHING;",
                   unique_sites$site_id[row], unique_sites$site_code[row], unique_sites$longitude[row], unique_sites$latitude[row])
  safeExecute(conn, query)
}

# Insert data into the 'site' table
# Assuming 'position' is generated from 'longitude' and 'latitude', and these fields exist in your 'site' table
unique_sites <- unique(site_tibble[, c("site_id", "site_code", "longitude", "latitude")])
for (row in 1:nrow(unique_sites)) {
  query <- sprintf("INSERT INTO site (site_id, site_code, location) VALUES (%d, '%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT (site_id) DO NOTHING;",
                   unique_sites$site_id[row], unique_sites$site_code[row], unique_sites$longitude[row], unique_sites$latitude[row])
  safeExecute(conn, query)
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
  safeExecute(conn, query)
}


# Close the connection
dbDisconnect(conn)


### END OF SCRIPT
# Create databases in psql

# \list
# \dt
# 
# CREATE DATABASE carsis;




