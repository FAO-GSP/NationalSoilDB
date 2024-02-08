
ui <- dashboardPage(
    dashboardHeader(title = "Harmonize Soil DB",titleWidth = 400),
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
                paste("SoilFER")
            ),
            p(
                class = "text-muted",
                align = "center",
                paste("FAO-GSP")
            ),
            p(
                class = "text-muted",
                align = "center",
                tags$a(href = "https://www.fao.org/documents/card/en?details=cc9430en", "SoilFER Site")
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