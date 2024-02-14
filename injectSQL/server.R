# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive value to store the database connection object
  dbCon <- reactiveVal(NULL)
  
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
  
  # Reactive value to store the connection status text
  connectionStatus <- reactiveVal("Not connected")
  
  # Toggle connection ON/OFF
  observeEvent(input$btnToggleConn, {
    if (is.null(dbCon())) {
      # Attempt to connect
      tryCatch({
        drv <- dbDriver(driver_name)
        conn <-
          dbConnect(
            drv,
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
                           label = "Disconnect",
                           icon = icon("ban"))
      }, error = function(e) {
        connectionStatus("Failed to connect")
      })
    } else {
      # Disconnect
      dbDisconnect(dbCon())
      dbCon(NULL) # Clear the connection object
      connectionStatus("Not connected")
      updateActionButton(session,
                         "btnToggleConn",
                         label = "Connect",
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
      uploaded_df <- read.csv(input$fileUpload$datapath)
      
      if (!checkFileStructure(uploaded_df)) {
        # Display warning message
        output$dynamicFileInput <- renderUI({
          if (!is.null(dbCon())) {
            fileInput("fileUpload", "Data Injection (csv)", accept = ".csv")
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
        # Clear previous warnings if any
        output$fileUploadWarning <- renderUI({})
        
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
  
  # Assuming you have a function to safely execute SQL commands
  safeExecute <- function(conn, query, session) {
    tryCatch({
      print(paste("Executing query:", query))
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
      fileInput("fileUpload", "Data Injection (csv)", accept = ".csv")
    }
  })
  
  observeEvent(input$fileUpload, {
    # Read the uploaded file
    site_tibble <- read.csv(input$fileUpload$datapath)
    
    #  Insert data into 'project' table
    # Adjust this logic based on your actual database schema
    try({
      unique_data <-
        unique(site_tibble[, c("project_id", "project_name")])
      for (row in 1:nrow(unique_data)) {
        query <-
          sprintf(
            "INSERT INTO project (project_id, name) VALUES (%d, '%s') ON CONFLICT (project_id) DO NOTHING;",
            unique_data$project_id[row],
            unique_data$project_name[row]
          )
        # Corrected line: use dbCon() to get the current connection object
        safeExecute(dbCon(), query, session)
      }
    })
    
    # Insert data into the 'site' table
    # Assuming 'position' is generated from 'longitude' and 'latitude', and these fields exist in your 'site' table
    try({
      unique_data <-
        unique(site_tibble[, c("site_id", "site_code", "longitude", "latitude")])
      for (row in 1:nrow(unique_data)) {
        query <-
          sprintf(
            "INSERT INTO site (site_id, site_code, location) VALUES (%d, '%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT (site_id) DO NOTHING;",
            unique_data$site_id[row],
            unique_data$site_code[row],
            unique_data$longitude[row],
            unique_data$latitude[row]
          )
        safeExecute(dbCon(), query, session)
      }
    })
    
    # Insert data into the 'site_project' table
    try({
      unique_data <- 
        unique(site_tibble[, c("site_id", "project_id")])
      # Insert data
      for (row in 1:nrow(unique_data)) {
        # Prepare the SQL INSERT statement
        query <- sprintf(
          "INSERT INTO site_project (site_id, project_id) VALUES (%s, %s) ON CONFLICT (site_id) DO NOTHING;",
          unique_data$site_id[row],
          unique_data$project_id[row]
        )
        safeExecute(dbCon(), query, session)
      }
    })
    
    # Insert data into the 'plot' table
    try({
      unique_data <-
        unique(site_tibble[, c("plot_id", "plot_code", "site_id", "plot_type")])
      for (row in 1:nrow(unique_data)) {
        query <- sprintf(
          "INSERT INTO plot (plot_id, plot_code, site_id, plot_type) VALUES (%d, '%s', %d, '%s') ON CONFLICT (plot_id) DO NOTHING;",
          unique_data$plot_id[row],
          unique_data$plot_code[row],
          unique_data$site_id[row],
          unique_data$plot_type[row]
        )
        safeExecute(dbCon(), query, session)
      }
    })
    

    # Insert data into the 'profile' table
    try({
      unique_data <- 
        unique(site_tibble[, c("profile_id", "profile_code", "plot_id")])
      # Insert data
      for (row in 1:nrow(unique_data)) {
        query <- sprintf(
          "INSERT INTO profile (profile_id, profile_code, plot_id) VALUES (%d, '%s', %d) ON CONFLICT (profile_id) DO NOTHING;",
          unique_data$profile_id[row],
          unique_data$profile_code[row],
          unique_data$plot_id[row]
        )
        safeExecute(dbCon(), query, session)
      }
    })
    
    # Insert data into the 'element' table
    try({
      unique_data <- unique(site_tibble[c("profile_id")])
      # Insert data
      for (row in 1:nrow(unique_data)) {
      query <- sprintf(
        "INSERT INTO element (profile_id) VALUES (%d) ON CONFLICT (element_id) DO NOTHING;",
        unique_data$profile_id[row]
      )
      safeExecute(dbCon(), query, session)
      }
    })
    
    # Insert data into the 'unit_of_measure' table
    # try({
    # unique_data <- 
    # unique(site_tibble[, c("unit_of_measure_id", "label", "description", "url")])
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO unit_of_measure (label, description, url) VALUES ('%s', '%s', '%s') ON CONFLICT (unit_of_measure_id) DO NOTHING;",
    #     unit_of_measure_tibble$label[row], 
    #     unit_of_measure_tibble$description[row], 
    #     unit_of_measure_tibble$url[row]
    #  )
    #   safeExecute(dbCon(), query, session)
    # }
    #})
    
    # # Insert data into the 'procedure_phys_chem' table
    # try({
    #  unique_data <- 
    #  unique(site_tibble[, c("procedure_phys_chem_id", "label", "url")])
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO procedure_phys_chem (procedure_phys_chem_id, label, url) VALUES (%d, '%s', '%s') ON CONFLICT (procedure_phys_chem_id) DO NOTHING;",
    #     unique_data$procedure_phys_chem_id[row], 
    #     unique_data$label[row], 
    #     unique_data$url[row]
    #   )
    #     safeExecute(dbCon(), query, session)
    #   }
    # })
    
    # # Insert data into the 'property_phys_chem' table
    # try({
    #  unique_data <- 
    # unique(site_tibble[, c("property_phys_chem_id", "label", "url")])
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #   "INSERT INTO property_phys_chem (property_phys_chem_id, label, url) VALUES (%d, '%s', '%s') ON CONFLICT (property_phys_chem_id) DO NOTHING;",
    #                    unique_data$property_phys_chem_id[row], 
    #                    unique_data$label[row], 
    #                    unique_data$url[row]
    #   )
    #     safeExecute(dbCon(), query, session)
    #   }
    # })
    
    
    # # Insert data into the 'observation_phys_chem' table
    # try({
    #  unique_data <- 
    #  unique(site_tibble[c("observation_phys_chem_id","property_phys_chem_id","procedure_phys_chem_id","unit_of_measure_id","value_min","value_max","observation_phys_chem_r_label")])
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #  query <- sprintf(
    #    "INSERT INTO observation_phys_chem (property_phys_chem_id, procedure_phys_chem_id, unit_of_measure_id, value_min, value_max, observation_phys_chem_r_label) VALUES (%d, %d, %d, %d, %d, %d,'%s') ON CONFLICT (observation_phys_chem_id) DO NOTHING;",
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
    # try({
    #  unique_data <- 
    # unique(site_tibble[c("result_phys_chem_id", "observation_phys_chem_id","element_id","value")])
    # for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO result_phys_chem (observation_phys_chem_id, element_id, value) VALUES (%d, %d, %f) ON CONFLICT (result_phys_chem_id) DO NOTHING;",
    #     result_phys_chem_tibble$observation_phys_chem_id[row], 
    #     result_phys_chem_tibble$element_id[row],
    #     result_phys_chem_tibble$value[row]
    #  )
    #   safeExecute(dbCon(), query, session)
    # }
    #})
    
    
    # # Insert data into the 'glosis_procedures' table
    # try({
    #  unique_data <- 
    #  unique(site_tibble[c("result_phys_chem_id", "observation_phys_chem_id","element_id","value")])
    #  for (row in 1:nrow(unique_data)) {
    #   # Prepare the SQL INSERT statement
    #   query <- sprintf(
    #     "INSERT INTO glosis_procedures (name, description) VALUES ('%s', '%s', ) ON CONFLICT (procedure_id) DO NOTHING;",
    #     unique_data$name[row], 
    #     unique_data$description[row]
    #  )
    #   safeExecute(dbCon(), query, session)
    # }
    #})
    
    
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
