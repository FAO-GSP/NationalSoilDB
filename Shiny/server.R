# Server logic
server <- function(input, output, session) {

    # Reactive values to store processed data
    reactiveData <- reactiveValues(site = NULL, horizon = NULL)
    

    observeEvent(input$file1, {
        req(input$file1)
        
        inFile <- input$file1
        
        # Load and convert types for 'element' sheet
        element <- load_and_convert_types(inFile$datapath, 'element', element_types)
        observation_phys_chem <- load_and_convert_types(inFile$datapath, 'observation_phys_chem', observation_phys_chem_types)
        plot <- load_and_convert_types(inFile$datapath, 'plot', plot_types)
        procedure_phys_chem <- load_and_convert_types(inFile$datapath, 'procedure_phys_chem', procedure_phys_chem_types)
        profile <- load_and_convert_types(inFile$datapath, 'profile', profile_types)
        project <- load_and_convert_types(inFile$datapath, 'project', project_types)
        property_phys_chem <- load_and_convert_types(inFile$datapath, 'property_phys_chem', property_phys_chem_types)
        result_phys_chem <- load_and_convert_types(inFile$datapath, 'result_phys_chem', result_phys_chem_types)
        site <- load_and_convert_types(inFile$datapath, 'site', site_types)
        site_project <- load_and_convert_types(inFile$datapath, 'site_project', site_project_types)
        unit_of_measure <- load_and_convert_types(inFile$datapath, 'unit_of_measure', unit_of_measure_types)
        # Extract longitude and latitude from the `position` column for the `site` and `plot` data frames
        # Assuming position is stored in WKT format and needs to be parsed
        # Here is the example for the `site` data frame, apply a similar approach for `plot`
        site <- site %>%
            mutate(longitude = numeric(),
                   latitude = numeric()
            ) %>% 
            select(-position)
        
        # Create the 'site' tibble by joining the necessary tables
        site_tibble <- site %>%
            left_join(site_project, by = "site_id") %>%
            left_join(project, by = "project_id") %>%
            left_join(plot, by = "site_id") %>%
            left_join(profile, by = "plot_id") %>%
            select(site_id, site_code, plot_id, plot_code, profile_id, profile_code, 
                   longitude, latitude, position, project_id, project_name=name)
        
        # Create the 'horizons' tibble by joining the necessary tables
        horizon_tibble <- profile %>%
            full_join(element, by = "profile_id") %>%
            full_join(result_phys_chem, by = "element_id") %>%
            full_join(observation_phys_chem, by = "observation_phys_chem_id") %>%
            full_join(property_phys_chem, by = "property_phys_chem_id") %>%  
            select(profile_id, profile_code, element_id, type, order_element, upper_depth, 
                   lower_depth, specimen_id, specimen_code, result_phys_chem_id,
                   observation_phys_chem_r_label, value)
        
        horizon_tibble <- horizon_tibble %>% 
            # Spread the label_property values into separate columns
            pivot_wider(names_from = observation_phys_chem_r_label, values_from = value)
        
        horizon_tibble <- horizon_tibble[-1,]
        
        
        reactiveData$site_tibble <- site_tibble # Placeholder
        reactiveData$horizon_tibble <- horizon_tibble # Placeholder
        
        # Output for site data table
        output$siteResults <- renderTable({
            reactiveData$site_tibble
        })
        
        # Output for horizon data table
        output$horizonResults <- renderTable({
            reactiveData$horizon_tibble
        })
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("soil-DB", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            req(reactiveData$site_tibble, reactiveData$horizon_tibble)
            
            # Prepare a list with each data frame as a sheet
            sheets <- list(
                "Site Data" = reactiveData$site_tibble,
                "Horizon Data" = reactiveData$horizon_tibble
            )
            # Write the list to an Excel file, creating one sheet per data frame
            write_xlsx(sheets, path = file)
        }
    )
}
