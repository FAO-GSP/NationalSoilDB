
ui <- dashboardPage(
    dashboardHeader(title = "National Soil Repository",titleWidth = 400),
    dashboardSidebar(
        fluidRow(
            align = "center",
            fileInput("file1", "Choose Excel File", accept = c(".xlsx")),
            downloadButton("downloadData", "Download")
        ),
        br(),
        box(
            title = "Project",
            width = NULL,
            status = "info",
            p(
                class = "text-muted",
                align = "center",
                paste("SOILCARE-CARSIS")
            ),
            p(
                class = "text-muted",
                align = "center",
                paste("FAO-GSP")
            )
        )
    ),
    dashboardBody(
        tabBox(width="100%",
            id = "resultsTab",
            tabPanel("Site Data", div(DT::dataTableOutput("siteResults"),
                                      style = "overflow-y: scroll;overflow-x: scroll;"
                                      )
                     ),
            tabPanel("Horizon Data", div(DT::dataTableOutput("horizonResults"),
                                   style = "overflow-y: scroll;overflow-x: scroll;"
                                   )
                     )
            )
        )
    )
