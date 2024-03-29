# Load packages
lapply(c("shiny","shinydashboard","shinycssloaders","RPostgres","DBI","DT","shinyjs","readxl","dplyr"), library, character.only = TRUE)

r.labels <- read.csv("rlabels.csv")
procedures <- read.csv("glosis_procedure.csv")
procedures <- dplyr::left_join(procedures,r.labels, by="attribute")

# Step 1: Find rows where 'definition' contains 'clay'
rows_with_clay <- grep("Sand, silt, clay", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'clay'
clay_rows <- procedures[rows_with_clay, ]
clay_rows$r.label <- "clay"

# Step 1: Find rows where 'definition' contains 'silt'
rows_with_silt <- grep("Sand, silt, clay", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'silt'
silt_rows <- procedures[rows_with_silt, ]
silt_rows$r.label <- "silt"

# Step 1: Find rows where 'definition' contains 'sand'
rows_with_sand <- grep("Sand, silt, clay", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'sand'
sand_rows <- procedures[rows_with_sand, ]
sand_rows$r.label <- "sand"

# Step 1: Find rows where 'definition' contains 'ca'
rows_with_ca <- grep("Exch bases \\(Ca, Mg, K, Na\\)", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'ca'
ca_rows <- procedures[rows_with_ca, ]
ca_rows$r.label <- "ca"
# Step 1: Find rows where 'definition' contains 'mg'
rows_with_mg <- grep("Exch bases \\(Ca, Mg, K, Na\\)", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'mg'
mg_rows <- procedures[rows_with_mg, ]
mg_rows$r.label <- "mg"
# Step 1: Find rows where 'definition' contains 'na'
rows_with_na <- grep("Exch bases \\(Ca, Mg, K, Na\\)", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'na'
na_rows <- procedures[rows_with_na, ]
na_rows$r.label <- "na"
# Step 1: Find rows where 'definition' contains 'k'
rows_with_k <- grep("Exch bases \\(Ca, Mg, K, Na\\)", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'k'
k_rows <- procedures[rows_with_k, ]
k_rows$r.label <- "k"

# Step 1: Find rows where 'definition' contains 'h'
rows_with_h <- grep("Exch acidity", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'h'
h_rows <- procedures[rows_with_h, ]
h_rows$r.label <- "h"
# Step 1: Find rows where 'definition' contains 'al'
rows_with_al <- grep("Exch acidity", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'al'
al_rows <- procedures[rows_with_al, ]
al_rows$r.label <- "al"

# Step 1: Find rows where 'definition' contains 'Exch (extractable / potential) acidity'
rows_with_ac_al <- grep("Exch \\(extractable / potential\\) acidity", procedures$definition, ignore.case = FALSE)
# Step 2 & 3: Duplicate these rows and set 'r.label' to 'al'
ac_al_rows <- procedures[rows_with_ac_al, ]
ac_al_rows$r.label <- "al"

# Step 4: Combine the original data frame with the modified duplicated rows
procedures.rlabel <- dplyr::bind_rows(procedures, clay_rows, silt_rows, sand_rows,
                        ca_rows, mg_rows, na_rows, k_rows,h_rows,al_rows,ac_al_rows)

procedures.rlabel <- procedures.rlabel %>%
  filter(r.label!="")

# Reset row names to be sequential
rownames(procedures.rlabel) <- NULL

property_phys_chem <- unique(procedures.rlabel[c("attribute","r.label")]) %>%
  arrange(r.label,attribute)

property_phys_chem <- property_phys_chem[property_phys_chem$r.label!="texture",]
rownames(property_phys_chem) <- NULL

# uploaded_df.procedure<- p %>% left_join(procedures, by="label")


# PostgreSQL credentials
database_name <- "postgres"
host_name <- "postgis"
port_number <- "5432"
user_name <- "gsp"
password_name <- "gsp"
global_pass <- ""

# Expected data types for the uploaded file
expected_vars <- list(
  project_name = "character",
  profile_code = "character",
  site_code = "character",
  plot_code = "character",
  plot_type = "character",
  longitude = "numeric",
  latitude = "numeric",
  position = "character"
)


# Function to check if the uploaded file matches the expected structure
checkFileStructure <- function(df) {
  # Extract the types of the uploaded data frame
  uploaded_types <- sapply(df, class)
  expected_types <- unlist(expected_vars)
  # Check if all expected variables are present and match the expected type
  if (!all(names(expected_vars) %in% names(df)) || 
      !all(uploaded_types[names(expected_vars)] == expected_types)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# Function to create empty tables at start the database, if tables exist, then this step is skipped
createTables <- function(database_name, host_name, port_number, user_name, password_name) {

  # Attempt to connect to the database
  con <- tryCatch({
    message("Connecting to Database…")
    dbConnect(RPostgres::Postgres(),
              dbname = database_name,
              host = host_name,
              port = port_number,
              user = user_name,
              password = password_name)
  }, error=function(cond) {
    message("Unable to connect to Database.")
    return(NULL) # Return NULL to indicate failure
  })
  
  # Exit function early if connection failed
  if (is.null(con)) {
    return(FALSE)
  }
  
  message("Database connected!")
  
  # Ensure PostGIS and postgis_raster extensions are available
  dbSendQuery(con, "CREATE EXTENSION IF NOT EXISTS postgis;")
  dbSendQuery(con, "CREATE EXTENSION IF NOT EXISTS postgis_raster;")
  
  # List of SQL commands to create tables
  #"CREATE TABLE IF NOT EXISTS dem (rid integer, rast raster);",
  
  table_creation_queries <- c(
    "CREATE TABLE IF NOT EXISTS project (project_id SERIAL PRIMARY KEY, name VARCHAR(255), description TEXT);",
    "CREATE TABLE IF NOT EXISTS site (site_id SERIAL PRIMARY KEY, site_code VARCHAR(255), location GEOGRAPHY(Point));",
    "CREATE TABLE IF NOT EXISTS site_project (site_id INTEGER NOT NULL, project_id INTEGER NOT NULL, PRIMARY KEY (site_id, project_id), FOREIGN KEY (site_id) REFERENCES site(site_id), FOREIGN KEY (project_id) REFERENCES project(project_id));",
    "CREATE TABLE IF NOT EXISTS plot (plot_id SERIAL PRIMARY KEY, plot_code VARCHAR(255),  site_id INTEGER NOT NULL,  plot_type VARCHAR(255), FOREIGN KEY (site_id) REFERENCES site(site_id));",
    "CREATE TABLE IF NOT EXISTS profile (profile_id SERIAL PRIMARY KEY, profile_code VARCHAR(255), plot_id INTEGER NOT NULL,  FOREIGN KEY (plot_id) REFERENCES plot(plot_id));",
    "CREATE TABLE IF NOT EXISTS element (element_id SERIAL PRIMARY KEY, type VARCHAR(255), profile_id INTEGER NOT NULL, order_element INTEGER, upper_depth NUMERIC, lower_depth NUMERIC, specimen_id INTEGER,  specimen_code VARCHAR(255), FOREIGN KEY (profile_id) REFERENCES profile(profile_id));",
    "CREATE TABLE IF NOT EXISTS specimen (specimen_id SERIAL PRIMARY KEY, code VARCHAR(255), plot_id INTEGER NOT NULL, specimen_prep_process_id INTEGER, depth INTEGER, FOREIGN KEY (plot_id) REFERENCES plot(plot_id));",
    
    "CREATE TABLE IF NOT EXISTS unit_of_measure (unit_of_measure_id SERIAL PRIMARY KEY,label VARCHAR(255), description TEXT, url VARCHAR(255));",
    "CREATE TABLE IF NOT EXISTS procedure_phys_chem (procedure_phys_chem_id SERIAL PRIMARY KEY, label VARCHAR(255), definition VARCHAR(255), url VARCHAR(255))",
    "CREATE TABLE IF NOT EXISTS property_phys_chem (property_phys_chem_id SERIAL PRIMARY KEY, label VARCHAR(255),  url VARCHAR(255));",
    
    "CREATE TABLE IF NOT EXISTS observation_phys_chem (observation_phys_chem_id SERIAL PRIMARY KEY, property_phys_chem_id INTEGER NOT NULL,
  procedure_phys_chem_id INTEGER NOT NULL, unit_of_measure_id INTEGER NOT NULL, value_min NUMERIC, value_max NUMERIC, observation_phys_chem_r_label VARCHAR(255), FOREIGN KEY (property_phys_chem_id) REFERENCES property_phys_chem(property_phys_chem_id),
  FOREIGN KEY (procedure_phys_chem_id) REFERENCES procedure_phys_chem(procedure_phys_chem_id), FOREIGN KEY (unit_of_measure_id) REFERENCES unit_of_measure(unit_of_measure_id));",
  
    "CREATE TABLE IF NOT EXISTS result_phys_chem (result_phys_chem_id SERIAL PRIMARY KEY, observation_phys_chem_id INTEGER NOT NULL, element_id INTEGER NOT NULL, value NUMERIC, FOREIGN KEY (observation_phys_chem_id) REFERENCES observation_phys_chem(observation_phys_chem_id), FOREIGN KEY (element_id) REFERENCES element(element_id));",
  
  "CREATE TABLE IF NOT EXISTS glosis_procedures (procedure_id SERIAL PRIMARY KEY, name VARCHAR(255), description TEXT);",
  "CREATE TABLE IF NOT EXISTS location (location_id SERIAL PRIMARY KEY, coordinates GEOGRAPHY(Point, 4326), description TEXT);",
  "CREATE TABLE IF NOT EXISTS project_site_location (project_id INTEGER NOT NULL, site_id INTEGER NOT NULL, location_id INTEGER NOT NULL, PRIMARY KEY (project_id, site_id, location_id), FOREIGN KEY (project_id) REFERENCES project(project_id), FOREIGN KEY (site_id) REFERENCES site(site_id), FOREIGN KEY (location_id) REFERENCES location(location_id));"
  )
  
  # Execute each SQL command
  for (query in table_creation_queries) {
    result <- dbSendQuery(con, query)
    dbClearResult(result)
  }
  
  
  # Close the database connection
  dbDisconnect(con)
  
  message("Database tables created or verified successfully.")
  
  return(TRUE) # Indicate success
}

# Function to backup database 
backupAndCreateDatabase <- function() {
  # Define your backup, create, and restore commands here
  # Ensure these commands are configured to run without requiring interactive input (e.g., passwords)
  backupCommand <- "/Applications/Postgres.app/Contents/Versions/16/bin/pg_dump -U luislado -h localhost -p 5432 -Fc carsis > carsis_backup.dump"
  backupDB <- paste0("carsis_",format(Sys.time(), "%d_%m_%y"))
  createDbCommand <- paste0("/Applications/Postgres.app/Contents/Versions/16/bin/createdb -U luislado -h localhost -p 5432 ",backupDB)
  restoreCommand <- paste0("/Applications/Postgres.app/Contents/Versions/16/bin/pg_restore -U luislado -h localhost -p 5432 -d ",backupDB,"  carsis_backup.dump")
  deleteCommand <- paste0("/Applications/Postgres.app/Contents/Versions/16/bin/dropdb -U luislado -h localhost -p 5432 ", backupDB)
  
  # Execute the commands
  system(backupCommand, intern = TRUE)
  system(deleteCommand, intern = TRUE)
  system(createDbCommand, intern = TRUE)
  system(restoreCommand, intern = TRUE)
}

# Function to create a database 
createAndConnectDatabase <- function(database_name, host_name, port_number, user_name, password_name) {
  con <- dbConnect(RPostgres::Postgres(),
                   dbname = "postgres",
                   host = host_name,
                   port = port_number,
                   user = user_name,
                   password = password_name)
  
  dbExists <- dbGetQuery(con, sprintf("SELECT 1 FROM pg_database WHERE datname = '%s'", database_name))
  
  messageContent <- "" # Initialize status message
  
  if (nrow(dbExists) == 0) {
    dbCreateStatement <- sprintf("CREATE DATABASE \"%s\";", database_name)
    dbExecute(con, dbCreateStatement)
    createTables(database_name, host_name, port_number, user_name, password_name)
    messageContent <- sprintf("Database '%s' created successfully", database_name)
    backgroundColor <- "darkorange"
    backgroundBorder <- "yellow"
  } else {
    messageContent <- sprintf("Database '%s' available", database_name)
    backgroundColor <- "dodgerblue"
    backgroundBorder <- "white"
  }
  
  dbDisconnect(con)
  
  newCon <- dbConnect(RPostgres::Postgres(),
                      dbname = database_name,
                      host = host_name,
                      port = port_number,
                      user = user_name,
                      password = password_name)
  
  if (!is.null(newCon)) {
    return(list(con = newCon, message = messageContent, backcolor = backgroundColor, backborder = backgroundBorder))
  } else {
    return(list(con = NULL, message = "Failed to connect to the database."))
  }
}

# Assuming you have a function to safely execute SQL commands
safeExecute <- function(conn, query, session) {
  tryCatch({
    result <- dbSendQuery(conn, query)
    dbClearResult(result)
  }, error = function(e) {
    print(paste("Error caught:", e$message))
    session$sendCustomMessage(type = "showErrorModal", message = e$message)
  })
}


