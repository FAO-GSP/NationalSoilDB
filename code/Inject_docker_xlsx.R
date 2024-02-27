# Shiny App to inject harmonized data in PostgreSQL database

# Load packages
lapply(c("shiny","shinydashboard","RPostgres","DBI","DT","shinyjs","readxl"), library, character.only = TRUE)

# Variable type definitions
# expected_vars <- list(
#   project_id = "integer",
#   project_name = "character",
#   profile_id = "integer",
#   profile_code = "character",
#   site_id = "integer",
#   site_code = "character",
#   plot_code = "character",
#   plot_type = "character",
#   longitude = "numeric",
#   latitude = "numeric",
#   position = "character"
# )
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


# PostgreSQL credentials
database_name <- "carsis"
host_name <- "localhost"
port_number <- "5432"
user_name <- "luislado"
password_name <- "luislado"




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

# Function to backup database 
backupAndCreateDatabase <- function() {
  # Define your backup, create, and restore commands here
  # Ensure these commands are configured to run without requiring interactive input (e.g., passwords)
  backupCommand <- "/Applications/Postgres.app/Contents/Versions/16/bin/pg_dump -U luislado -h localhost -p 5432 -Fc carsis > carsis_backup.dump"
  
  #--dbname=postgresql://luislado:luislado@127.0.0.1:5432/carsis
  
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



# Define UI ----
ui <- fluidPage(useShinyjs(),
                dashboardPage(
                  skin = "red" ,
                  dashboardHeader(
                    title = "CARSIS Database Update",
                    tags$li(
                      class = "dropdown",
                      tags$img(
                        src = "fao_logo1.png",
                        height = "40px",
                        style = "position: absolute; right: 20px; top: 5px;"
                      )
                    ),
                    titleWidth = 250
                  ),
                  dashboardSidebar(
                    tags$head(
                      tags$style(HTML(".main-sidebar, .left-side {background-color: #FFC527 !important;}"))
                    ),
                    tags$br(),
                    actionButton("btnToggleConn", "Connect to CARSIS", icon = icon("plug"), width = '80%'),
                    tags$br(),
                    uiOutput("backupWarning"), # Add this line to display warnings
                    uiOutput("dynamicFileInput"), # Dynamic UI for fileInput
                    uiOutput("fileUploadWarning"), # Add this line to display warnings
                    uiOutput("connectionWarning") # Add this line to display warnings
                  ),
                  
                  dashboardBody(
                    tags$head(tags$style(
                      HTML(
                        "
        /* Change the dashboard body background color to green */
        .content-wrapper {
          background-color: D3D3D3 !important;
        }
        /* Set the tabBox to occupy full width */
        .tab-content {
          width: 100% !important;
        }
        /* Optional: Adjust the height */
        .content-wrapper, .tab-content {
          height: 80vh !important; /* Adjust based on your needs */
          overflow-y: auto; /* Adds scroll to the content if it exceeds the viewport height */
        }
      "
                      )
                    )),
      tabBox(
        id = "tabs",
        width = 12,
        tabPanel("Project", DTOutput("viewProject")),
        tabPanel("Site", DTOutput("viewSite")),
        tabPanel("Site Project", DTOutput("viewSite_project")),
        tabPanel("Plot", DTOutput("viewPlot")),
        tabPanel("Profile", DTOutput("viewProfile")),
        tabPanel("Element", DTOutput("viewElement")),
        tabPanel("Unit_of_measure", DTOutput("viewUnit_of_measure")),
        tabPanel("Procedure_phys_chem", DTOutput("viewProcedure_phys_chem")),
        tabPanel("Property_phys_chem", DTOutput("viewProperty_phys_chem")),
        tabPanel("Observation_phys_chem", DTOutput("viewObservation_phys_chem")),
        tabPanel("Result_phys_chem", DTOutput("viewResult_phys_chem")),
        tabPanel("Glosis procedures", DTOutput("viewGlosis_procedures"))
      )
                  )
                ))


# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive value to store the database connection object
  dbCon <- reactiveVal(NULL)
  # Reactive value to store the connection status text
  connectionStatus <- reactiveVal("Not connected")
  
  # Toggle connection ON/OFF
  observeEvent(input$btnToggleConn, {
    if (is.null(dbCon())) {
      # Display backup warning immediately
      output$backupWarning <- renderUI({
        tags$div(
          style = "color: white; background-color: dodgerblue; font-weight: bold; text-align: center; border: 1px solid white; padding: 10px; margin: 10px; border-radius: 5px;",
          "Creating CARSIS Backup"
        )
      })
      # Backup operation with an immediate start and no delay
      #delay(500,backupAndCreateDatabase())
      
      # Attempt to connect
      delay(500,
            tryCatch({
              # Start to connect
              conn <-
                dbConnect(
                  RPostgres::Postgres(),
                  dbname = database_name,
                  host = host_name,
                  port = port_number,
                  user = user_name,
                  password = password_name
                )
              dbCon(conn) # Store the connection object
              
              
              connectionStatus("Connected")
              
              updateActionButton(session,
                                 "btnToggleConn",
                                 label = "Disconnect from CARSIS",
                                 icon = icon("ban"))
              
              delay(1000,output$backupWarning <- renderUI({}))
            }, error = function(e) {
              connectionStatus("Failed to connect")
              output$backupWarning <- renderUI({})
              output$connectionWarning <- renderUI({
                tags$div(
                  style = "color: white; background-color: red; font-weight: bold; text-align: center; border: 1px solid white; padding: 10px; margin: 10px; border-radius: 5px;",
                  "Failed to connect..."
                )})
              delay(800,output$connectionWarning <- renderUI({}))
            })
      )
    } else {
      # Disconnect
      dbDisconnect(dbCon())
      output$backupWarning <- renderUI({})
      dbCon(NULL) # Clear the connection object
      connectionStatus("Not connected")
      updateActionButton(session,
                         "btnToggleConn",
                         label = "Connect to CARSIS",
                         icon = icon("plug"))
    }
    
  })
  
  # Reset warning message when connection status changes from "Failed to connect"
  observeEvent(connectionStatus(), {
    if (connectionStatus() != "Failed to connect") {
      output$fileUploadWarning <- renderUI({})
    }
  })
  
  # Observe file upload and check structure
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    tryCatch({
      #uploaded_df <- read.csv(input$fileUpload$datapath)
      uploaded_df.site <- read_excel(input$fileUpload$datapath,sheet="Site Data")
      uploaded_df.hor <- read_excel(input$fileUpload$datapath,sheet="Horizon Data")
      uploaded_df <- left_join(uploaded_df.hor,uploaded_df.site)
      
      
      
      if (!checkFileStructure(uploaded_df)) {
        # Display warning message
        output$dynamicFileInput <- renderUI({
          if (!is.null(dbCon())) {
            fileInput("fileUpload", "Data Injection (csv)", accept = ".xlsx")
          }
        })
        
        output$fileUploadWarning <- renderUI({
          if (!is.null(dbCon())) {
            tags$div(
              tags$div(
                style = "color: white; background-color: red; font-weight: bold; text-align: center; border: 2px solid red; padding: 10px; margin: 10px; border-radius: 5px;",
                HTML("Warning:<br>Uploaded file does not match the expected structure or variable types")
              ),
              tags$div(
                style = "color: red; font-weight: bold; text-align: center; border: 2px solid red; padding: 10px; margin: 10px; border-radius: 5px;",
                HTML("Please, check your data")
              )
            )
          }
        })
      } else {
        output$fileUploadWarning <- renderUI({
          if (!is.null(dbCon())) {
            tags$div(
              tags$div(
                style = "color: white; background-color: green; font-weight: bold; text-align: center; border: 1px solid white; padding: 10px; margin: 10px; border-radius: 5px;",
                HTML("Update Successful")
              )
            )
          }
        })
        # Clear previous warnings if any
        delay(3000,output$fileUploadWarning <- renderUI({}))
        
        # Proceed with database operations...
      }
    }, error = function(e) {
      output$fileUploadWarning <- renderUI({
        tags$div(
          style = "color: red; font-weight: bold;",
          paste("Error reading file:", e$message)
        )
      })
    })
  })
  
  
  # Define Functions ----
  # Function to Load data and convert variable types
  # load_and_convert_types <- function(path, sheet_name, type_conversions) {
  #   data <- xlsx::read.xlsx(path, sheetName = sheet_name, stringsAsFactors = F)
  #   for (col_name in names(type_conversions)) {
  #     data[[col_name]] <- type_conversions[[col_name]](data[[col_name]])
  #   }
  #   return(data)
  # }
  
  # Assuming you have a function to safely execute SQL commands
  safeExecute <- function(conn, query, session) {
    tryCatch({
      #print(paste("Executing query:", query))
      result <- dbSendQuery(conn, query)
      dbClearResult(result)
    }, error = function(e) {
      print(paste("Error caught:", e$message))
      session$sendCustomMessage(type = "showErrorModal", message = e$message)
    })
  }
  
  # Function to dynamically render data tables
  renderDataTables <- function(tableName) {
    renderDT({
      req(dbCon()) # Ensure there's a connection
      df <-
        dbGetQuery(dbCon(), sprintf("SELECT * FROM %s", tableName))
      df
    }, filter = "top", options = list(
      pageLength = 20),
    rownames = FALSE
    )
  }
  
  output$viewProject <- renderDataTables("project")
  output$viewSite <- renderDataTables("site")
  output$viewSite_project <- renderDataTables("site_project")
  output$viewPlot <- renderDataTables("plot")
  output$viewProfile <- renderDataTables("profile")
  output$viewElement <- renderDataTables("element")
  output$viewUnit_of_measure <- renderDataTables("unit_of_measure")
  output$viewProcedure_phys_chem <- renderDataTables("procedure_phys_chem")
  output$viewProperty_phys_chem <- renderDataTables("property_phys_chem")
  output$viewObservation_phys_chem <- renderDataTables("observation_phys_chem")
  output$viewResult_phys_chem <- renderDataTables("result_phys_chem")
  output$viewGlosis_procedures <- renderDataTables("glosis_procedures")
  
  
  # Dynamically render fileInput based on connection status
  output$dynamicFileInput <- renderUI({
    if (!is.null(dbCon())) {
      #fileInput("fileUpload", "Data Injection (csv)", accept = ".csv")
      fileInput("fileUpload", "Data Injection (csv)", accept = ".xlsx")
    }
  })
  
  observeEvent(input$fileUpload, {
    
    # Read the uploaded file
    #site_tibble <- read.csv(input$fileUpload$datapath)
    uploaded_df.site <- read_excel(input$fileUpload$datapath,sheet="Site Data")
    uploaded_df.hor <- read_excel(input$fileUpload$datapath,sheet="Horizon Data")
    site_tibble <- left_join(uploaded_df.hor,uploaded_df.site)
    
    
    #  Insert data into 'project' table ----
    # Adjust this logic based on your actual database schema
    unique_data <- unique(site_tibble[, c("project_name")])
    tryCatch({
      for (row in unique_data) {
        query <-
          sprintf(
            "INSERT INTO project (project_id, name) VALUES (DEFAULT, '%s') ON CONFLICT DO NOTHING;",
            row
          )
        # use dbCon() to get the current connection object
        safeExecute(dbCon(), query, session)
      }
      query <-
        sprintf("DELETE FROM project a USING project b WHERE b.project_id < a.project_id AND a.name = b.name;"
        )
      safeExecute(dbCon(), query, session)
    })
    
    # Insert data into the 'site' table ----
    # Assuming 'position' is generated from 'longitude' and 'latitude', and these fields exist in your 'site' table
    unique_data <- unique(site_tibble[, c("site_code", "longitude", "latitude")])
    tryCatch({
      for (row in 1:nrow(unique_data)) {
        query <-
          sprintf(
            "INSERT INTO site (site_code, location) VALUES ('%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT DO NOTHING;",
            unique_data$site_code[row],
            unique_data$longitude[row],
            unique_data$latitude[row]
          )
        safeExecute(dbCon(), query, session)
      }
      query <-
        sprintf("DELETE FROM site a USING site b WHERE b.site_id < a.site_id AND a.site_code = b.site_code AND a.location = b.location;"
        )
      safeExecute(dbCon(), query, session)
    })
    
    # Insert data into the 'site_project' table ----
    # First, ensure unique_data contains the necessary information
    unique_data <- unique(site_tibble[, c("site_code", "project_name")])
    tryCatch({
      for (i in 1:nrow(unique_data)) {
        pair <- unique_data[i, ]
        
        # Retrieve 'site_id' based on 'site_code'
        site_id_query <- sprintf("SELECT site_id FROM site WHERE site_code = '%s'", pair$site_code)
        site_id_result <- dbGetQuery(dbCon(), site_id_query)
        
        # Retrieve 'project_id' based on 'project_name'
        project_id_query <- sprintf("SELECT project_id FROM project WHERE name = '%s'", pair$project_name)
        project_id_result <- dbGetQuery(dbCon(), project_id_query)
        
        if (nrow(site_id_result) > 0 && nrow(project_id_result) > 0) {
          site_id <- site_id_result$site_id[1]
          project_id <- project_id_result$project_id[1]
          
          # Insert the pair into 'site_project', avoiding duplicates
          insert_query <- sprintf(
            "INSERT INTO site_project (site_id, project_id) VALUES (%d, %d) ON CONFLICT DO NOTHING;",
            site_id, project_id
          )
          dbSendQuery(dbCon(), insert_query)
        }
      }
    }, error = function(e) {
      message(sprintf("Error inserting data into site_project table: %s", e$message))
    })
    
    # Insert data into the 'plot' table ----
    # First, ensure unique_data contains the necessary information
    unique_data <- unique(site_tibble[, c("site_code", "project_name", "plot_code", "plot_type")])
    # Insert data into the 'plot' table and then associate plots with sites and projects
    tryCatch({
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row, ]
        
        # Retrieve 'site_id' based on 'site_code'
        site_id_result <- dbGetQuery(dbCon(), sprintf("SELECT site_id FROM site WHERE site_code = '%s'", current_row$site_code))
        
        # Retrieve 'project_id' based on 'project_name'
        project_id_result <- dbGetQuery(dbCon(), sprintf("SELECT project_id FROM project WHERE name = '%s'", current_row$project_name))
        
        if (nrow(site_id_result) > 0 && nrow(project_id_result) > 0) {
          site_id <- site_id_result$site_id[1]
          project_id <- project_id_result$project_id[1]
          
          # Check if the plot_code already exists to avoid duplicate entries
          existing_plot <- dbGetQuery(dbCon(), sprintf("SELECT plot_id FROM plot WHERE plot_code = '%s'", current_row$plot_code))
          
          if (nrow(existing_plot) == 0) {
            # Insert new plot with site_id
            dbSendQuery(dbCon(), sprintf("INSERT INTO plot (plot_code, plot_type, site_id) VALUES ('%s', '%s', %d)", current_row$plot_code, current_row$plot_type, site_id))
          }
          
          # Now, ensure the site and project association in 'site_project' table
          # This might be redundant if 'site_project' population logic is handled elsewhere or if plots are not directly associated with projects
          insert_query <- sprintf("INSERT INTO site_project (site_id, project_id) VALUES (%d, %d) ON CONFLICT DO NOTHING;", site_id, project_id)
          dbSendQuery(dbCon(), insert_query)
        }
      }
    }, error = function(e) {
      message(sprintf("Error during plot insertion or plot population: %s", e$message))
    })
    
    
    # Insert data into the 'profile' table ----
    unique_data <- unique(site_tibble[, c("plot_code", "profile_code")])
    tryCatch({
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row, ]
        
        # Retrieve 'plot_id' based on 'plot_code'
        plot_id_result <- dbGetQuery(dbCon(), sprintf("SELECT plot_id FROM plot WHERE plot_code = '%s'", current_row$plot_code))
        
        if (nrow(plot_id_result) > 0) {
          plot_id <- plot_id_result$plot_id[1]
          
          # Check if the profile_code already exists to avoid duplicate entries
          existing_profile <- dbGetQuery(dbCon(), sprintf("SELECT profile_id FROM profile WHERE profile_code = '%s'", current_row$profile_code))
          
          if (nrow(existing_profile) == 0) {
            # Insert new profile with plot_id
            dbSendQuery(dbCon(), sprintf("INSERT INTO profile (profile_code, plot_id) VALUES ('%s', %d)", current_row$profile_code, plot_id))
          }
          # If profile_code already exists, you might want to update the plot_id or take other actions based on your application logic
        }
      }
    }, error = function(e) {
      message(sprintf("Error during profile insertion: %s", e$message))
    })
    
    # Insert data into the 'element' table ----
    unique_profile_codes <- unique(site_tibble$profile_code)
    tryCatch({
      for (profile_code in unique_profile_codes) {
        # Retrieve 'profile_id' based on 'profile_code'
        profile_id_result <- dbGetQuery(dbCon(), sprintf("SELECT profile_id FROM profile WHERE profile_code = '%s'", profile_code))
        
        if (nrow(profile_id_result) > 0) {
          profile_id <- profile_id_result$profile_id[1]
          
          # Insert new element with only profile_id, leaving other attributes to be filled later
          insert_query <- sprintf("INSERT INTO element (profile_id) VALUES (%d)", profile_id)
          
          dbSendQuery(dbCon(), insert_query)
        }
        query <-
          sprintf("DELETE FROM element a USING element b WHERE b.element_id < a.element_id AND a.profile_id = b.profile_id;"
          )
        safeExecute(dbCon(), query, session)
      }
    }, error = function(e) {
      message(sprintf("Error during element insertion: %s", e$message))
    })
    
    
    # Insert data into the 'unit_of_measure' table
    # unique_data <- 
    # unique(site_tibble[, c("unit_of_measure_id", "label", "description", "url")])
    # tryCatch({
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO unit_of_measure (label, description, url) VALUES ('%s', '%s', '%s') ON CONFLICT DO NOTHING;",
    #     unit_of_measure_tibble$label[row], 
    #     unit_of_measure_tibble$description[row], 
    #     unit_of_measure_tibble$url[row]
    #  )
    #   safeExecute(dbCon(), query, session)
    # }
    #})
    
    # # Insert data into the 'procedure_phys_chem' table
    #  unique_data <- 
    #  unique(site_tibble[, c("procedure_phys_chem_id", "label", "url")])
    #  tryCatch({
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO procedure_phys_chem (procedure_phys_chem_id, label, url) VALUES (%d, '%s', '%s') ON CONFLICT DO NOTHING;",
    #     unique_data$procedure_phys_chem_id[row], 
    #     unique_data$label[row], 
    #     unique_data$url[row]
    #   )
    #     safeExecute(dbCon(), query, session)
    #   }
    # })
    
    # # Insert data into the 'property_phys_chem' table
    # unique_data <-
    #   unique(site_tibble[, c("property_phys_chem_id", "label", "url")])
    # tryCatch({
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #   "INSERT INTO property_phys_chem (property_phys_chem_id, label, url) VALUES (%d, '%s', '%s') ON CONFLICT DO NOTHING;",
    #                    unique_data$property_phys_chem_id[row],
    #                    unique_data$label[row],
    #                    unique_data$url[row]
    #   )
    #     safeExecute(dbCon(), query, session)
    #   }
    # })
    
    
    # # Insert data into the 'observation_phys_chem' table
    # unique_data <-
    #   unique(site_tibble[c("observation_phys_chem_id","property_phys_chem_id","procedure_phys_chem_id","unit_of_measure_id","value_min","value_max","observation_phys_chem_r_label")])
    # tryCatch({
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #  query <- sprintf(
    #    "INSERT INTO observation_phys_chem (property_phys_chem_id, procedure_phys_chem_id, unit_of_measure_id, value_min, value_max, observation_phys_chem_r_label) VALUES (%d, %d, %d, %d, %d, %d,'%s') ON CONFLICT DO NOTHING;",
    #    unique_data$property_phys_chem_id[row],
    #    unique_data$procedure_phys_chem_id[row],
    #    unique_data$unit_of_measure_id[row],
    #    unique_data$value_min[row],
    #    unique_data$value_max[row],
    #    unique_data$observation_phys_chem_r_label[row]
    #  )
    #    safeExecute(dbCon(), query, session)
    #  }
    # })
    
    # # Insert data into the 'result_phys_chem' table
    # unique_data <-
    #   unique(site_tibble[c("result_phys_chem_id", "observation_phys_chem_id","element_id","value")])
    # tryCatch({
    # for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO result_phys_chem (observation_phys_chem_id, element_id, value) VALUES (%d, %d, %f) ON CONFLICT DO NOTHING;",
    #     result_phys_chem_tibble$observation_phys_chem_id[row],
    #     result_phys_chem_tibble$element_id[row],
    #     result_phys_chem_tibble$value[row]
    #  )
    #   safeExecute(dbCon(), query, session)
    # }
    # })
    
    
    # # Insert data into the 'glosis_procedures' table
    # try({
    #  unique_data <-
    #  unique(site_tibble[c("result_phys_chem_id", "observation_phys_chem_id","element_id","value")])
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO glosis_procedures (name, description) VALUES ('%s', '%s', ) ON CONFLICT DO NOTHING;",
    #     unique_data$name[row],
    #     unique_data$description[row]
    #  )
    #   safeExecute(dbCon(), query, session)
    # }
    # })
    
    output$viewProject <- renderDataTables("project")
    output$viewSite <- renderDataTables("site")
    output$viewSite_project <- renderDataTables("site_project")
    output$viewPlot <- renderDataTables("plot")
    output$viewProfile <- renderDataTables("profile")
    output$viewElement <- renderDataTables("element")
    output$viewUnit_of_measure <- renderDataTables("unit_of_measure")
    output$viewProcedure_phys_chem <- renderDataTables("procedure_phys_chem")
    output$viewProperty_phys_chem <- renderDataTables("property_phys_chem")
    output$viewObservation_phys_chem <- renderDataTables("observation_phys_chem")
    output$viewResult_phys_chem <- renderDataTables("result_phys_chem")
    output$viewGlosis_procedures <- renderDataTables("glosis_procedures")
    
  })
}

# Run the app
shinyApp(ui, server)
