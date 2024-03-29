# Define UI ----
ui <- fluidPage(useShinyjs(),
                dashboardPage(
                  skin = "red" ,
                  dashboardHeader(
                    title = "ISO-28258 Soil domain model",
                    tags$li(
                      class = "dropdown",
                      tags$img(
                        src = "fao_logo1.png",
                        height = "40px",
                        style = "position: absolute; right: 20px; top: 5px;"
                      )
                    ),
                    titleWidth = 300
                  ),
                  dashboardSidebar(
                    tags$head(
                      tags$style(HTML(".main-sidebar, .left-side {background-color: #FFC527 !important;}"))
                    ),
                    tags$br(),
                    actionButton("btn_create_db", "Find Database", icon = icon("database"), width = '85%'),
                    uiOutput("dbMessage"),
                    uiOutput("password_modal"),  
                    actionButton("btnToggleConn", "Connect to Database", icon = icon("plug"), width = '85%'),
                    tags$br(),
                    uiOutput("backupWarning"), # Add this line to display warnings
                    uiOutput("dynamicFileInput"), # Dynamic UI for fileInput
                    #uiOutput("fileUploadWarning"), # Add this line to display warnings
                    uiOutput("connectionWarning"), # Add this line to display warnings
                    uiOutput("renderButton")# Dynamic UI for render dashboard
                  ),
                  
                  dashboardBody(
                    tags$head(tags$style(
                      HTML(
                        "
                          /* Change the dashboard body background color to green */
                          .content-wrapper {background-color: D3D3D3 !important;}
                          /* Set the tabBox to occupy full width */
                          .tab-content {width: 100% !important;}
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
        tabPanel("Project", DTOutput("viewProject") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Site", DTOutput("viewSite") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Site Project", DTOutput("viewSite_project") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Plot", DTOutput("viewPlot") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Profile", DTOutput("viewProfile") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Element", DTOutput("viewElement") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Specimen", DTOutput("viewSpecimen") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Unit_of_measure", DTOutput("viewUnit_of_measure") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Procedure_phys_chem", DTOutput("viewProcedure_phys_chem") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Property_phys_chem", DTOutput("viewProperty_phys_chem") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Observation_phys_chem", DTOutput("viewObservation_phys_chem") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Result_phys_chem", DTOutput("viewResult_phys_chem") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Glosis procedures", DTOutput("viewGlosis_procedures") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("Location", DTOutput("viewLocation") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2)),
        tabPanel("PSL", DTOutput("viewPSL") %>% withSpinner(color="#0275D8",color.background="#ffffff", size = .8, type = 2))

      )
  )
))

