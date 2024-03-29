insertProjectData <-
  function(site_tibble,
           uploaded_df.procedure,
           dbCon,
           session) {
    con = dbCon()
    
    # Show modal right before rendering
    showModal(modalDialog(
      title = "Processing data",
      "Please wait...",
      footer = NULL
    ))
    
    # Insert data into the 'glosis_procedures' table ----
    try({
      unique_data <-
        unique(procedures[c("label", "definition")])
      for (row in 1:nrow(unique_data)) {
        # Prepare the SQL INSERT statement
        query <- sprintf(
          "INSERT INTO glosis_procedures (name, description) VALUES ('%s', '%s') ON CONFLICT DO NOTHING;",
          unique_data$label[row],
          unique_data$definition[row]
        )
        safeExecute(con, query, session)
      }
      query <-
        sprintf(
          "DELETE FROM glosis_procedures a USING glosis_procedures b WHERE b.procedure_id < a.procedure_id AND a.name = b.name;"
        )
      safeExecute(con, query, session)
    })
    
    
    # Insert data into 'project' table ----
    tryCatch({
      unique_data <- unique(site_tibble[, c("project_name")])
      for (row in unique_data) {
        query <-
          sprintf(
            "INSERT INTO project (project_id, name) VALUES (DEFAULT, '%s') ON CONFLICT DO NOTHING;",
            row
          )
        # use con to get the current connection object
        safeExecute(con, query, session)
      }
      query <-
        sprintf(
          "DELETE FROM project a USING project b WHERE b.project_id < a.project_id AND a.name = b.name;"
        )
      safeExecute(con, query, session)
    })
    
    # Insert data into the 'site' table ----
    tryCatch({
      unique_data <-
        unique(site_tibble[, c("site_code", "longitude", "latitude")])
      
      for (row in 1:nrow(unique_data)) {
        query <-
          sprintf(
            "INSERT INTO site (site_code, location) VALUES ('%s', ST_SetSRID(ST_MakePoint(%f, %f), 4326)) ON CONFLICT DO NOTHING;",
            unique_data$site_code[row],
            unique_data$longitude[row],
            unique_data$latitude[row]
          )
        safeExecute(con, query, session)
      }
      query <-
        sprintf(
          "DELETE FROM site a USING site b WHERE b.site_id < a.site_id AND a.site_code = b.site_code AND a.location = b.location;"
        )
      safeExecute(con, query, session)
    })
    
    # ----
    # Insert data into the 'site_project' table ----
    tryCatch({
      unique_data <- unique(site_tibble[, c("site_code", "project_name")])
      
      for (i in 1:nrow(unique_data)) {
        pair <- unique_data[i,]
        
        # Retrieve 'site_id' based on 'site_code'
        site_id_query <-
          sprintf("SELECT site_id FROM site WHERE site_code = '%s'",
                  pair$site_code)
        site_id_result <- dbGetQuery(con, site_id_query, session)
        
        # Retrieve 'project_id' based on 'project_name'
        project_id_query <-
          sprintf("SELECT project_id FROM project WHERE name = '%s'",
                  pair$project_name)
        project_id_result <-
          dbGetQuery(con, project_id_query, session)
        
        if (nrow(site_id_result) > 0 && nrow(project_id_result) > 0) {
          site_id <- site_id_result$site_id[1]
          project_id <- project_id_result$project_id[1]
          
          # Insert the pair into 'site_project', avoiding duplicates
          insert_query <- sprintf(
            "INSERT INTO site_project (site_id, project_id) VALUES (%d, %d) ON CONFLICT DO NOTHING;",
            site_id,
            project_id
          )
          dbSendQuery(con, insert_query, session)
        }
      }
    }, error = function(e) {
      message(sprintf("Error inserting data into site_project table: %s", e$message))
    })
    
    
    # ----
    # Insert data into the 'location' and 'project_site_location' tables ----
    tryCatch({
      unique_data <-
        unique(site_tibble[, c("site_code", "project_name", "latitude", "longitude")])
      
      for (i in 1:nrow(unique_data)) {
        pair <- unique_data[i,]
        # Generate a textual representation of the coordinates for simplicity
        coords_text <-
          sprintf("POINT(%f %f)", pair$longitude, pair$latitude)
        
        # Insert location, checking if the exact textual representation exists
        location_query <- sprintf(
          "INSERT INTO location (coordinates, description) VALUES (ST_GeomFromText('%s', 4326), 'Description here') ON CONFLICT DO NOTHING;",
          coords_text
        )
        # Try to insert, if exists, get the existing location_id
        location_result <- dbGetQuery(con, location_query)
        
        # Check if the location was newly inserted
        if (nrow(location_result) > 0) {
          # If the location was inserted, use the returned location_id
          location_id <- location_result$location_id[1]
        } else {
          # If there was a conflict and no new row was inserted, fetch the existing location_id
          location_result <- dbGetQuery(
            con,
            sprintf(
              "SELECT location_id FROM location WHERE coordinates = ST_SetSRID(ST_MakePoint(%f, %f), 4326);",
              pair$longitude,
              pair$latitude
            )
          )
          if (nrow(location_result) > 0) {
            location_id <- location_result$location_id[1]
          } else {
            # Handle the case where the location_id could not be determined
            stop("Failed to obtain location_id for coordinates.")
          }
        }
        
        # Step 2: Retrieve 'site_id' based on 'site_code'
        site_id_result <-
          dbGetQuery(con,
                     sprintf(
                       "SELECT site_id FROM site WHERE site_code = '%s';",
                       pair$site_code
                     ))
        site_id <- site_id_result$site_id[1]
        
        # Step 3: Retrieve 'project_id' based on 'project_name'
        project_id_result <-
          dbGetQuery(
            con,
            sprintf(
              "SELECT project_id FROM project WHERE name = '%s';",
              pair$project_name
            )
          )
        project_id <- project_id_result$project_id[1]
        
        # Step 4: Insert into 'project_site_location', avoiding duplicates
        insert_query <- sprintf(
          "INSERT INTO project_site_location (project_id, site_id, location_id) VALUES (%d, %d, %d) ON CONFLICT (project_id, site_id, location_id) DO NOTHING;",
          project_id,
          site_id,
          location_id
        )
        dbExecute(con, insert_query)
      }
      query <-
        sprintf(
          "DELETE FROM location a USING location b WHERE b.location_id < a.location_id AND a.coordinates = b.coordinates;"
        )
      safeExecute(con, query, session)
    }, error = function(e) {
      message(sprintf("Error during location insertion: %s", e$message))
    })
    
    # ----
    #  Insert data into the 'plot' table and then associate plots with sites and projects ----
    tryCatch({
      unique_data <-
        unique(site_tibble[, c("site_code", "project_name", "plot_code", "plot_type")])
      
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row,]
        
        # Retrieve 'site_id' based on 'site_code'
        site_id_result <-
          dbGetQuery(
            con,
            sprintf(
              "SELECT site_id FROM site WHERE site_code = '%s'",
              current_row$site_code
            )
          )
        
        # Retrieve 'project_id' based on 'project_name'
        project_id_result <-
          dbGetQuery(
            con,
            sprintf(
              "SELECT project_id FROM project WHERE name = '%s'",
              current_row$project_name
            )
          )
        
        if (nrow(site_id_result) > 0 && nrow(project_id_result) > 0) {
          site_id <- site_id_result$site_id[1]
          project_id <- project_id_result$project_id[1]
          
          # Check if the plot_code already exists to avoid duplicate entries
          existing_plot <-
            dbGetQuery(
              con,
              sprintf(
                "SELECT plot_id FROM plot WHERE plot_code = '%s'",
                current_row$plot_code
              )
            )
          
          if (nrow(existing_plot) == 0) {
            # Insert new plot with site_id
            dbSendQuery(
              con,
              sprintf(
                "INSERT INTO plot (plot_code, plot_type, site_id) VALUES ('%s', '%s', %d)",
                current_row$plot_code,
                current_row$plot_type,
                site_id
              )
            )
          }
          
          # Now, ensure the site and project association in 'site_project' table
          # This might be redundant if 'site_project' population logic is handled elsewhere or if plots are not directly associated with projects
          insert_query <-
            sprintf(
              "INSERT INTO site_project (site_id, project_id) VALUES (%d, %d) ON CONFLICT DO NOTHING;",
              site_id,
              project_id
            )
          dbSendQuery(con, insert_query)
        }
      }
    }, error = function(e) {
      message(sprintf(
        "Error during plot insertion or plot population: %s",
        e$message
      ))
    })
    
    
    # ----
    # Insert data into the 'profile' table ----
    tryCatch({
      unique_data <- unique(site_tibble[, c("plot_code", "profile_code")])
      
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row,]
        
        # Retrieve 'plot_id' based on 'plot_code'
        plot_id_result <-
          dbGetQuery(
            con,
            sprintf(
              "SELECT plot_id FROM plot WHERE plot_code = '%s'",
              current_row$plot_code
            )
          )
        
        if (nrow(plot_id_result) > 0) {
          plot_id <- plot_id_result$plot_id[1]
          
          # Check if the profile_code already exists to avoid duplicate entries
          existing_profile <-
            dbGetQuery(
              con,
              sprintf(
                "SELECT profile_id FROM profile WHERE profile_code = '%s'",
                current_row$profile_code
              )
            )
          
          if (nrow(existing_profile) == 0) {
            # Insert new profile with plot_id
            dbSendQuery(
              con,
              sprintf(
                "INSERT INTO profile (profile_code, plot_id) VALUES ('%s', %d)",
                current_row$profile_code,
                plot_id
              )
            )
          }
          # If profile_code already exists, you might want to update the plot_id or take other actions based on your application logic
        }
      }
    }, error = function(e) {
      message(sprintf("Error during profile insertion: %s", e$message))
    })
    
    # ----
    # Insert data into the 'element' table ----
    tryCatch({
      unique_data <-
        unique(site_tibble[, c("type",
                               "upper_depth",
                               "lower_depth",
                               "specimen_code",
                               "profile_code")])
      
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row,]
        # Retrieve 'profile_id' based on 'profile_code'
        query <-
          sprintf(
            "SELECT profile_id FROM profile WHERE profile_code = '%s'",
            current_row$profile_code
          )
        profile_id_result <- dbGetQuery(con, query)
        
        if (nrow(profile_id_result) > 0) {
          profile_id <- profile_id_result$profile_id[1]
          query <-
            sprintf(
              "INSERT INTO element (profile_id, type, upper_depth, lower_depth, specimen_code) VALUES (%d, '%s', %d, %d, '%s') ON CONFLICT DO NOTHING;",
              profile_id,
              current_row$type,
              current_row$upper_depth,
              current_row$lower_depth,
              current_row$specimen_code
            )
          # Insert all attributes including 'profile_id' in one go
          dbExecute(con, query)
        }
      }
      query <-
        sprintf(
          "DELETE FROM element a USING element b WHERE b.element_id < a.element_id AND a.profile_id = b.profile_id AND a.specimen_code = b.specimen_code;"
        )
      safeExecute(con, query, session)
    }, error = function(e) {
      message(sprintf("Error during element insertion: %s", e$message))
    })
    
    # ----
    # Insert data into the 'specimen' table ----
    tryCatch({
      for (i in 1:nrow(site_tibble)) {
        current_row <- site_tibble[i,]
        
        # Retrieve 'plot_id' based on 'plot_code'
        plot_id_result <-
          dbGetQuery(
            con,
            sprintf(
              "SELECT plot_id FROM plot WHERE plot_code = '%s'",
              current_row$plot_code
            )
          )
        
        if (nrow(plot_id_result) > 0) {
          plot_id <- plot_id_result$plot_id[1]
          # query <- sprintf(
          #   "INSERT INTO specimen (code, plot_id, specimen_prep_process_id, depth) VALUES ('%s', %d,%d,%d) ON CONFLICT DO NOTHING;",
          #   current_row$specimen_code,
          #   plot_id,
          #   ifelse(is.na(current_row$specimen_prep_process_id), NA, current_row$specimen_prep_process_id),
          #   as.integer(mean(c(current_row$lower_depth,current_row$upper_depth))))
          query <- sprintf(
            "INSERT INTO specimen (code, plot_id, depth) VALUES ('%s', %d, %d) ON CONFLICT DO NOTHING;",
            current_row$specimen_code,
            plot_id,
            as.integer(mean(
              c(current_row$lower_depth, current_row$upper_depth)
            ))
          )
          
          # Insert all attributes including 'profile_id' in one go
          safeExecute(con, query, session)
        }
      }
      query <-
        sprintf(
          "DELETE FROM specimen a USING specimen b WHERE b.specimen_id < a.specimen_id AND a.code = b.code AND a.plot_id = b.plot_id;"
        )
      safeExecute(con, query, session)
    }, error = function(e) {
      message(sprintf("Error during element specimen: %s", e$message))
    })
    
    # ----
    # Insert data into the 'unit_of_measure' table ----
    tryCatch({
      unique_data <-
        unique(uploaded_df.procedure[, c("observation_phys_chem_r_label",
                                         "label",
                                         "units",
                                         "reference")])
      for (row in 1:nrow(unique_data)) {
        # Prepare the SQL INSERT statement
        query <- sprintf(
          "INSERT INTO unit_of_measure (label, description, url) VALUES ('%s','%s','%s') ON CONFLICT DO NOTHING;",
          unique_data$units[row],
          paste0(
            unique_data$observation_phys_chem_r_label[row],
            "_",
            unique_data$label[row]
          ),
          unique_data$reference[row]
        )
        safeExecute(con, query, session)
      }
      query <-
        sprintf(
          "DELETE FROM unit_of_measure a USING unit_of_measure b WHERE b.unit_of_measure_id < a.unit_of_measure_id AND a.description = b.description  AND a.label = b.label;"
        )
      safeExecute(con, query, session)
    })
    
    # ----
    # Insert data into the 'procedure_phys_chem' table ----
    tryCatch({
      unique_data <-
        unique(uploaded_df.procedure[, c("observation_phys_chem_r_label",
                                         "label",
                                         "definition",
                                         "reference")])
      
      for (row in 1:nrow(unique_data)) {
        # Prepare the SQL INSERT statement
        query <- sprintf(
          "INSERT INTO procedure_phys_chem (label, definition, url) VALUES ('%s', '%s', '%s') ON CONFLICT DO NOTHING;",
          paste0(
            unique_data$observation_phys_chem_r_label[row],
            "_",
            unique_data$label[row]
          ),
          unique_data$definition[row],
          unique_data$reference[row]
        )
        safeExecute(con, query, session)
      }
      query <-
        sprintf(
          "DELETE FROM procedure_phys_chem a USING procedure_phys_chem b WHERE b.procedure_phys_chem_id < a.procedure_phys_chem_id AND a.label = b.label;"
        )
      safeExecute(con, query, session)
    })
    
    
    # Insert data into the 'property_phys_chem' table ----
    tryCatch({
      unique_data <-  unique(property_phys_chem[c("r.label")])
      for (row in 1:nrow(unique_data)) {
        # Prepare the SQL INSERT statement
        query <- sprintf(
          "INSERT INTO property_phys_chem (label) VALUES ('%s') ON CONFLICT DO NOTHING;",
          unique_data$r.label[row]
        )
        safeExecute(con, query, session)
      }
      query <-
        sprintf(
          "DELETE FROM property_phys_chem a USING property_phys_chem b WHERE b.property_phys_chem_id < a.property_phys_chem_id AND a.label = b.label;"
        )
      safeExecute(con, query, session)
    })
    
    
    # Insert data into the 'observation_phys_chem' table ----
    tryCatch({
      unique_data <-
        unique(left_join(
          uploaded_df.procedure,
          property_phys_chem,
          by = join_by(observation_phys_chem_r_label == r.label)
        ))
      unique_data$r.label <-
        paste0(unique_data$observation_phys_chem_r_label,
               "_",
               unique_data$label)
      
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row,]
        # Retrieve 'property_phys_chem_id' based on 'label'
        query <-
          sprintf(
            "SELECT property_phys_chem_id FROM property_phys_chem WHERE label = '%s'",
            current_row$observation_phys_chem_r_label
          )
        property_phys_chem_id_result <- dbGetQuery(con, query)
        
        # Retrieve 'procedure_id' based on 'label'
        query <-
          sprintf(
            "SELECT procedure_phys_chem_id FROM procedure_phys_chem WHERE label = '%s'",
            current_row$r.label
          )
        procedure_id_result <- dbGetQuery(con, query)
        
        # Retrieve 'unit_of_measure_id' based on 'label'
        query <-
          sprintf(
            "SELECT unit_of_measure_id FROM unit_of_measure WHERE label = '%s'",
            current_row$units
          )
        unit_id_result <- dbGetQuery(con, query)
        
        
        if (nrow(property_phys_chem_id_result) > 0) {
          # Prepare the SQL INSERT statement
          query <- sprintf(
            "INSERT INTO observation_phys_chem (property_phys_chem_id, procedure_phys_chem_id, unit_of_measure_id, value_min, value_max, observation_phys_chem_r_label) VALUES (%d, %d, %d, %f, %f, '%s') ON CONFLICT DO NOTHING;",
            property_phys_chem_id_result[1, ],
            procedure_id_result[1, ],
            unit_id_result[1, ],
            min(site_tibble[, names(site_tibble) %in% current_row$observation_phys_chem_r_label], na.rm =
                  TRUE),
            max(site_tibble[, names(site_tibble) %in% current_row$observation_phys_chem_r_label], na.rm =
                  TRUE),
            current_row$observation_phys_chem_r_label
          )
          safeExecute(con, query)
        }
      }
      query <-
        sprintf(
          "DELETE FROM observation_phys_chem a USING observation_phys_chem b WHERE b.observation_phys_chem_id < a.observation_phys_chem_id AND a.observation_phys_chem_r_label = b.observation_phys_chem_r_label;"
        )
      safeExecute(con, query, session)
    })
    
    # # Insert data into the 'result_phys_chem' table ----
    tryCatch({
      unique_data <-
        site_tibble[, names(site_tibble) %in% c("specimen_code", c(uploaded_df.procedure)[[1]])]
      variables <- names(unique_data)[-1]
      
      for (row in 1:nrow(unique_data)) {
        current_row <- unique_data[row,]
        for (col in variables) {
          # Retrieve 'observation_phys_chem_id' based on 'observation_phys_chem_r_label'
          query <-
            sprintf(
              "SELECT observation_phys_chem_id FROM observation_phys_chem WHERE observation_phys_chem_r_label = '%s'",
              col
            )
          observation_phys_chem_id_result <- dbGetQuery(con, query)
          
          # Retrieve 'element_id' based on 'label'
          query <-
            sprintf(
              "SELECT element_id FROM element WHERE specimen_code = '%s'",
              current_row$specimen_code
            )
          element_id_result <- dbGetQuery(con, query)
          
          
          if (nrow(observation_phys_chem_id_result) > 0) {
            observation_id <-
              observation_phys_chem_id_result$observation_phys_chem_id[1]
            element_id <- element_id_result$element_id[1]
            value <- as.numeric(current_row[[col]])
            
            # Use parameterized query for insert to prevent SQL injection
            query <-
              sprintf(
                "INSERT INTO result_phys_chem (observation_phys_chem_id, element_id ,value) VALUES (%d, %d, %f) ON CONFLICT DO NOTHING;",
                observation_id,
                element_id,
                value
              )
            dbExecute(con, query)
          }
        }
      }
    }, error = function(e) {
      message("An error occurred: ", e$message)
    })

    
    # Insert NIR spectroscopic data ----
    # Assuming `reflectance_data` is a data frame or tibble with columns: 'element_code', 'file_path', and 'metadata_json'
    # 'element_code' is a unique identifier for elements, which we will use to retrieve 'element_id'
    # 'file_path' is the path to the file
    # 'metadata_json' is a JSON string containing metadata
    # tryCatch({
    #   for (i in 1:nrow(reflectance_data)) {
    #     current_row <- reflectance_data[i, ]
    #
    #     # Retrieve 'element_id' based on 'element_code'
    #     element_id_query <- sprintf("SELECT element_id FROM element WHERE type = '%s'", current_row$element_code)
    #     element_id_result <- dbGetQuery(con, element_id_query)
    #
    #     if (nrow(element_id_result) > 0) {
    #       element_id <- element_id_result$element_id[1]
    #
    #       # Insert the file reference into 'soil_mid_ir_reflectance_files', assuming metadata is properly formatted JSON
    #       insert_query <- sprintf(
    #         "INSERT INTO soil_mid_ir_reflectance_files (element_id, file_path, metadata) VALUES (%d, '%s', '%s') ON CONFLICT DO NOTHING;",
    #         element_id,
    #         current_row$file_path,
    #         current_row$metadata_json  # Ensure this is properly escaped JSON string
    #       )
    #       dbSendQuery(con, insert_query)
    #     } else {
    #       message(sprintf("Element ID not found for element code: %s", current_row$element_code))
    #     }
    #   }
    # }, error = function(e) {
    #   message(sprintf("Error inserting data into soil_mid_ir_reflectance_files table: %s", e$message))
    # })
    #
    # After rendering, remove the modal
    removeModal()
    # Notify the user it's ready
    shiny::showNotification("Data processed successfully!", type = "error")  }




