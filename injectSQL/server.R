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
    }, options = list(
      pageLength = 20),
      rownames = FALSE
    )
  }
  
  output$viewProject <- renderDataTables("project")
  output$viewSite <- renderDataTables("site")
  output$viewPlot <- renderDataTables("plot")
  
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
    
    output$viewProject <- renderDataTables("project")
    output$viewSite <- renderDataTables("site")
    output$viewPlot <- renderDataTables("plot")
    
  })
  
}

