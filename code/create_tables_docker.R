# The script creates the SQL empty tables with the defined types for CARSIS variables

library('RPostgres')
library('DBI')
library('dplyr')

# Create connection to postgres
dsn_database = "carsis"       # Specify the name of your Database
dsn_hostname = "localhost"    # Specify your hostname
dsn_port = 5432             # Specify your port number
dsn_uid = "luislado"          # Specify your username. e.g. "admin"
dsn_pwd = "luislado"   

# Connect to the database
tryCatch({
  print("connecting to Database…")
  con <- dbConnect(RPostgres::Postgres(),
                    dbname = dsn_database,
                    host = dsn_hostname, # Use "host.docker.internal" if running on Docker for Mac/Windows
                    port = dsn_port,
                    user = dsn_uid,
                    password = dsn_pwd)
  
  print("Database conected!")
},
error=function(cond) {
  print("Unable to conect to Database.")
})

# Add POSTGIS
dbSendQuery(con, "CREATE EXTENSION IF NOT EXISTS postgis;")


## SQL CODE TO CREATE TABLES----


## ADD TABLES
## Table:  project
create_table_query <- "
CREATE TABLE IF NOT EXISTS project (
  project_id SERIAL PRIMARY KEY,
  name VARCHAR(255),
  description TEXT
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)

## Table:  site
create_table_query <- " CREATE TABLE IF NOT EXISTS site (
  site_id SERIAL PRIMARY KEY,
  site_code VARCHAR(255),
  location GEOGRAPHY(Point)
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)


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
dbSendQuery(con, create_table_query)


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
dbSendQuery(con, create_table_query)

## Table:  profile
create_table_query <- " CREATE TABLE IF NOT EXISTS profile (
  profile_id SERIAL PRIMARY KEY,
  profile_code VARCHAR(255),
  plot_id INTEGER NOT NULL,
  FOREIGN KEY (plot_id) REFERENCES plot(plot_id)
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)

## Table:  unit_of_measure
create_table_query <- " CREATE TABLE IF NOT EXISTS unit_of_measure (
  unit_of_measure_id SERIAL PRIMARY KEY,
  label VARCHAR(255),
  description TEXT,
  url VARCHAR(255)
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)

## Table:  property_phys_chem
create_table_query <- " CREATE TABLE IF NOT EXISTS property_phys_chem (
  property_phys_chem_id SERIAL PRIMARY KEY,
  label VARCHAR(255),
  url VARCHAR(255)
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)

## Table:  procedure_phys_chem
create_table_query <- " CREATE TABLE IF NOT EXISTS procedure_phys_chem (
  procedure_phys_chem_id SERIAL PRIMARY KEY,
  label VARCHAR(255),
  url VARCHAR(255)
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)

## Table:  glosis_procedures
create_table_query <- " CREATE TABLE IF NOT EXISTS glosis_procedures (
  procedure_id SERIAL PRIMARY KEY,
  name VARCHAR(255),
  description TEXT
);
"
# Execute the SQL command
dbSendQuery(con, create_table_query)

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
dbSendQuery(con, create_table_query)

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
dbSendQuery(con, create_table_query)

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
dbSendQuery(con, create_table_query)


