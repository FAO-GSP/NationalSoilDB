# Define server logic ----
server <- function(input, output, session) {
  source("fill_tables.R")  # Place this outside of your observeEvent or server function
  
  # Reactive value to store the database connection object
  dbCon <- reactiveVal(NULL)
  # Reactive value to store the connection status text
  connectionStatus <- reactiveVal("Not connected")
  
  # When the 'Input Database Name' button is clicked search or add a database
  observeEvent(input$btn_create_db, {
    showModal(modalDialog(
      title = "Enter Database Name",
      textInput("db_name_input", "Database Name", value = ""),
      # JavaScript for enabling/disabling the Confirm button and setting focus
      tags$script(HTML("
        $(document).on('shown.bs.modal', function() {
          $('#db_name_input').focus();
        });
        $(document).on('keyup', '#db_name_input', function() {
          if ($('#db_name_input').val().trim() != '') {
            $('#confirm').prop('disabled', false);
          } else {
            $('#confirm').prop('disabled', true);
          }
        });
        $(document).on('keydown', '#db_name_input', function(e) {
          if(e.keyCode == 13) {
            $('#confirm').click();
          }
        });
      ")),
      footer = tagList(
        actionButton("confirm", "Confirm", class = "btn-primary", disabled = TRUE),
        modalButton("Cancel")
      ),
      easyClose = FALSE,
      fade = TRUE,
      # JavaScript to trigger Confirm click on Enter key in the Database Name input field
      tags$script(HTML("
        $(document).on('keydown', '#db_name_input', function(e) {
          if(e.keyCode == 13) {  // 13 is the Enter key
            $('#confirm').click();
          }
        });
      "))
    ))
  })
  
  # Handle the 'Confirm' action for database creation
  observeEvent(input$confirm, {
    database_name <- isolate(input$db_name_input)
    
    if (nchar(trimws(database_name)) == 0) {
      # Show an error message if no database name is provided
      output$dbMessage <- renderUI({
        tags$div(
          style = "font-weight: bold; text-align: center; padding: 10px; margin: 10px; border-radius: 5px; color: white; background-color: red; border: 2px solid white;",
          HTML("Error: Please enter a database name.")
        )
      })
    } else {
      # Prompt for admin password
      showModal(modalDialog(
        title = "Enter Admin Password",
        passwordInput("admin_password", "Password"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("password_confirm", "Confirm", class = "btn-primary")
        ),
        easyClose = FALSE,
        fade = TRUE,
        tags$script(HTML("
        $(document).on('shown.bs.modal', function() {
          $('#admin_password').focus();
        });
        $(document).on('keydown', '#admin_password', function(e) {
          if(e.keyCode == 13) {  // 13 is the Enter key
            $('#password_confirm').click();
          }
        });
      "))
      ))
    }
  })
  

  observeEvent(input$password_confirm, {
    passwordInput <- isolate(input$admin_password)
    # Assume global_pass is defined elsewhere in your server function or globally
    if (passwordInput == global_pass) {
      removeModal()  # Close the modal only if the password is correct
      
      database_name <- isolate(input$db_name_input)
      
      showModal(modalDialog(
        title = "Checking Databases",
        "Please wait...",
        footer = NULL
      ))
      dbCreationResult <- createAndConnectDatabase(database_name, host_name, port_number, user_name, password_name)
      print(dbCreationResult)
      # Define the base style template for the message
      baseStyleTemplate <- "font-weight: bold; text-align: center; padding: 10px; margin: 10px; border-radius: 5px; color: white; background-color: %s; border: 2px solid %s;"
      
      # Initialize variables for message content, background color, and border color based on the operation result
      backgroundColor <- ifelse(!is.null(dbCreationResult$backcolor), dbCreationResult$backcolor, "tomato1")
      borderColor <- ifelse(!is.null(dbCreationResult$backcolor), dbCreationResult$backborder, "darkred")
      messageContent <- ifelse(!is.null(dbCreationResult$con), dbCreationResult$message, "Error: Failed to create or connect to the database.")
      
      # Apply the style with the dynamic background color and content
      styledMessage <- sprintf(baseStyleTemplate, backgroundColor, borderColor)
      
      output$dbMessage <- renderUI({
        tags$div(style = styledMessage, HTML(messageContent))
      })
    } else {
      # Password incorrect, do not close the modal and alert the user
      shinyjs::alert("Incorrect password, please try again.")
      # The modal stays open allowing the user to try again or cancel
    }
    
    # After rendering, remove the modal
    removeModal()
    
    # Notify the user it's ready
    shiny::showNotification("Database check successful!", type = "message")
    
  })
  
  
  # Toggle database connection ON/OFF
  observeEvent(input$btnToggleConn, {
    req(input$btn_create_db)
    if (is.null(dbCon())) {
      # Attempt to reconnect using the last known successful database_name
      database_name <- isolate(input$db_name_input) # Ensure this is accessible here; might need adjustment
      dbCreationResult <- createAndConnectDatabase(database_name, host_name, port_number, user_name, password_name)
      
      if (!is.null(dbCreationResult$con)) {
        dbCon(dbCreationResult$con) # Re-establish the connection
        connectionStatus("Reconnected to database")
        updateActionButton(session, "btnToggleConn", label = "Disconnect from Database", icon = icon("ban"))
      } else {
        connectionStatus("Failed to reconnect")
      }
    } else {
      # Disconnect
      dbDisconnect(dbCon())
      dbCon(NULL) # Clear the connection object
      connectionStatus("Disconnected")
      updateActionButton(session, "btnToggleConn", label = "Connect to Database", icon = icon("plug"))
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
      uploaded_df.site <- read_excel(input$fileUpload$datapath, sheet = "Site Data")
      uploaded_df.hor <- read_excel(input$fileUpload$datapath, sheet = "Horizon Data")
      uploaded_df.procedure <- read_excel(input$fileUpload$datapath, sheet = "Procedures") %>%
        left_join(procedures, by="label")
      
      site_tibble <- left_join(uploaded_df.hor,uploaded_df.site)
      
      if (!checkFileStructure(site_tibble)) {
        # Display warning message
        output$dynamicFileInput <- renderUI({
          if (!is.null(dbCon())) {
            fileInput("fileUpload", "Data Injection (.xlsx)", accept = ".xlsx")
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
        delay(1000,output$fileUploadWarning <- renderUI({}))
        
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
  
  # Render Database Tables
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
  output$viewLocation <- renderDataTables("location")
  output$viewPSL <- renderDataTables("project_site_location")
  output$viewSpecimen <- renderDataTables("specimen")
  
  # Assuming 'connectionStatus' is a reactive value holding the message
  output$dbMessage <- renderUI({
    # Define the base style template
    baseStyleTemplate <- "font-weight: bold; text-align: center; padding: 10px; margin: 10px; border-radius: 5px; color: white; background-color: %s; border: 2px solid %s;"
    
    # Initialize variables for message content and background color
        messageContent <- "Status:<br>No current database."
        backgroundColor <- "gray80"
        borderColor <- "white"
    # Apply the style with the dynamic background color and content
    styledMessage <- sprintf(baseStyleTemplate, backgroundColor, borderColor)
    tags$div(style = styledMessage, HTML(messageContent))
  })
  

  # Dynamically render fileInput based on connection status
  output$dynamicFileInput <- renderUI({
    if (!is.null(dbCon())) {
      fileInput("fileUpload", "Data Injection (.xlsx)", accept = ".xlsx")
    }
  })

  # Dynamically create the render button after file is uploaded
  output$renderButton <- renderUI({
    if (!is.null(dbCon())) {
      actionButton("dashboard", "Update Dashboard", 
                   style = "display: block; margin: 0 auto; text-align: center;")
    }
  })
  
  
  observeEvent(input$fileUpload, {
    
    # Read the uploaded file
    uploaded_df.site <- read_excel(input$fileUpload$datapath, sheet = "Site Data")
    uploaded_df.hor <- read_excel(input$fileUpload$datapath, sheet = "Horizon Data")
    uploaded_df.procedure <- read_excel(input$fileUpload$datapath, sheet = "Procedures") %>%
      left_join(procedures, by="label")
    
    site_tibble <- left_join(uploaded_df.hor,uploaded_df.site)
    
    ## Start of fill_tables (delete 'if' and replace with 'fill_tables.R)
    if (!is.null(site_tibble) && nrow(site_tibble) > 0) {
      insertProjectData(site_tibble, uploaded_df.procedure,dbCon, session)  # Call the function
    }
    ## End of fill tables (delete 'if' and replace with 'fill_tables.R)
    
  # Render Tables ----
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
    output$viewLocation <- renderDataTables("location")
    output$viewPSL <- renderDataTables("project_site_location")
    output$viewSpecimen <- renderDataTables("specimen")
    
    # Dynamically create the render button after file is uploaded
    # output$renderButton <- renderUI({
    #   if (!is.null(dbCon())) {
    #     actionButton("dashboard", "Update Dashboard", 
    #                  style = "display: block; margin: 0 auto; text-align: center;")
    #   }
    # })
    
    
  })
  
  # Render Dashboard ----
  observeEvent(input$dashboard, {
    #req(input$fileUpload) # if you want to trigger the action  only if updated file
    # Show modal right before rendering
    showModal(modalDialog(
      title = "Rendering Dashboard",
      "Please wait...",
      footer = NULL
    ))
    # Render the R Markdown document
    output_path <- "iso28258DM.html"
    rmarkdown::render("dashboard.Rmd", output_file = output_path)
    # After rendering, remove the modal
    removeModal()
    # Notify the user it's ready
    shiny::showNotification("Dashboard rendered successfully!", type = "message")
  })
  
}
