
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
                paste("CARSIS")
            ),
            p(
                class = "text-muted",
                align = "center",
                paste("FAO-GSP")
            )
        )
    ),
    dashboardBody(
        tabBox(
            id = "resultsTab",
            tabPanel("Site Data", tableOutput("siteResults")),
            tabPanel("Horizon Data", tableOutput("horizonResults"))
        )
    )
)
