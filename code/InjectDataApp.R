library(shiny)
library(shinydashboard)
library(RPostgreSQL)
library(DT)

# Define UI
ui <- dashboardPage(skin = "red" ,
  dashboardHeader(title = "CARSIS Database Update",
                  tags$li(class = "dropdown", 
                          tags$img(src = "fao_logo1.png", height = "40px", style = "position: absolute; right: 20px; top: 5px;")),
                  titleWidth = 250),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        /* Change the sidebar background color to yellow */
        .main-sidebar, .left-side {
          background-color: #FFC527 !important;
        }
      "))
    ),
    tags$br(), 
    actionButton("btnToggleConn", "Connect to CARSIS", icon = icon("plug"),width='80%'), # Updated line
    tags$br(), 
    uiOutput("dynamicFileInput") # Dynamic UI for fileInput
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
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
      "))
    ),
    tabBox(
      id = "tabs",
      width = 12,
      tabPanel("Project", DTOutput("viewProject")),
      tabPanel("Site", DTOutput("viewSite")),
      tabPanel("Plot", DTOutput("viewPlot"))
      # Add more tabs for other tables as needed
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the database connection object
  dbCon <- reactiveVal(NULL)
  
  # Reactive value to store the connection status text
  connectionStatus <- reactiveVal("Not connected")
  
  observeEvent(input$btnToggleConn, {
    if (is.null(dbCon())) {
      # Attempt to connect
      tryCatch({
        drv <- dbDriver("PostgreSQL")
        conn <- dbConnect(drv, dbname = "carsis", host = "localhost", port = "5432",
                          user = "luislado", password = "")
        dbCon(conn) # Store the connection object
        connectionStatus("Connected")
        updateActionButton(session, "btnToggleConn", label = "Disconnect", icon = icon("ban"))
      }, error = function(e) {
        connectionStatus("Failed to connect")
      })
    } else {
      # Disconnect
      dbDisconnect(dbCon())
      dbCon(NULL) # Clear the connection object
      connectionStatus("Not connected")
      updateActionButton(session, "btnToggleConn", label = "Connect", icon = icon("plug"))
    }
  })
  

  
  # Assuming you have a function to safely execute SQL commands
  safeExecute <- function(conn, query) {
    tryCatch({
      dbSendQuery(conn, query)
    }, error = function(e) {
      cat("Error in executing SQL: ", e$message, "\n")
    })
  }

  # Fetch updated 'project' table data to display
  output$viewProject <- renderDT({
    if (!is.null(dbCon())) {
      # Use the connection stored in dbCon()
      df <- dbGetQuery(dbCon(), "SELECT * FROM project")
      return(df)
    } else {
      # Return an empty data frame or a message indicating no connection
      return(data.frame())  # Or a more informative placeholder
    }
  }, options = list(pageLength = 20))
  
  # Fetch updated 'site' table data to display
  output$viewSite <- renderDT({
    if (!is.null(dbCon())) {
      # Use the connection stored in dbCon()
      df <- dbGetQuery(dbCon(), "SELECT * FROM site")
      return(df)
    } else {
      # Return an empty data frame or a message indicating no connection
      return(data.frame())  # Or a more informative placeholder
    }
  }, options = list(pageLength = 20))
  
  # Fetch updated 'plot' table data to display
  output$viewPlot <- renderDT({
    if (!is.null(dbCon())) {
      # Use the connection stored in dbCon()
      df <- dbGetQuery(dbCon(), "SELECT * FROM plot")
      return(df)
    } else {
      # Return an empty data frame or a message indicating no connection
      return(data.frame())  # Or a more informative placeholder
    }
  }, options = list(pageLength = 20))
  
  # Dynamically render fileInput based on connection status
  output$dynamicFileInput <- renderUI({
    if (!is.null(dbCon())) {
      fileInput("fileUpload", "Inject Data (csv)", accept = ".csv")
    }
  })
  
  observeEvent(input$fileUpload, {
    # Read the uploaded file
    site_tibble <- read.csv(input$fileUpload$datapath)
    
    #  Insert data into 'project' table
    # Adjust this logic based on your actual database schema
    try({
      unique_data <- unique(site_tibble[, c("project_id", "project_name")])
      for (row in 1:nrow(unique_data)) {
        query <- sprintf("INSERT INTO project (project_id, name) VALUES (%d, '%s') ON CONFLICT (project_id) DO NOTHING;",
                         unique_data$project_id[row], unique_data$project_name[row])
        # Corrected line: use dbCon() to get the current connection object
        safeExecute(dbCon(), query)
      }
    })
    
    # Insert data into the 'site' table
    # Assuming 'position' is generated from 'longitude' and 'latitude', and these fields exist in your 'site' table
    try({
    unique_data <- unique(site_tibble[, c("site_id", "site_code", "longitude", "latitude")])
    for (row in 1:nrow(unique_data)) {
      query <- sprintf("INSERT INTO site (site_id, site_code, location) VALUES (%d, '%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT (site_id) DO NOTHING;",
                       unique_data$site_id[row], unique_data$site_code[row], unique_data$longitude[row], unique_data$latitude[row])
      safeExecute(dbCon(), query)
    }
  })
    
    
    # Insert data into the 'plot' table
    try({
    unique_data <- unique(site_tibble[, c("plot_id", "plot_code", "site_id", "plot_type")])
    for (row in 1:nrow(unique_data)) {
      query <- sprintf(
        "INSERT INTO plot (plot_id, plot_code, site_id, plot_type) VALUES (%d, '%s', %d, '%s') ON CONFLICT (plot_id) DO NOTHING;",
        unique_data$plot_id[row], unique_data$plot_code[row], unique_data$site_id[row], unique_data$plot_type[row])
      safeExecute(dbCon(), query)
    }
    })
    
    # Fetch updated 'project' table data to display
    output$viewProject <- renderDT({
      if (!is.null(dbCon())) {
        # Use the connection stored in dbCon()
        df <- dbGetQuery(dbCon(), "SELECT * FROM project")
        return(df)
      } else {
        # Return an empty data frame or a message indicating no connection
        return(data.frame())  # Or a more informative placeholder
      }
    }, options = list(pageLength = 20))
    
    # Fetch updated 'site' table data to display
    output$viewSite <- renderDT({
      if (!is.null(dbCon())) {
        # Use the connection stored in dbCon()
        df <- dbGetQuery(dbCon(), "SELECT * FROM site")
        return(df)
      } else {
        # Return an empty data frame or a message indicating no connection
        return(data.frame())  # Or a more informative placeholder
      }
    }, options = list(pageLength = 20))
    
      # Fetch updated 'plot' table data to display
      output$viewPlot <- renderDT({
        if (!is.null(dbCon())) {
          # Use the connection stored in dbCon()
          df <- dbGetQuery(dbCon(), "SELECT * FROM plot")
          return(df)
        } else {
          # Return an empty data frame or a message indicating no connection
          return(data.frame())  # Or a more informative placeholder
        }
      }, options = list(pageLength = 20))
    
  })
  
}

# Run the app
shinyApp(ui, server)

