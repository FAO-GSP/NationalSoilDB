# NationalSoilDB - SQL data injection
Shiny app to inject soil data from cvs files to SQL.
 - The script 'create_tables.R' allows to create empty PostgreSQL tables according to the definitions for soil databases at GSP. There must be a proper DB created in the PostgreSQL server (e.g. CREATE DATABASE carsis;). Credentials to enter the server must be adjusted in the R script.

 - The scripts 'ui.R', 'server.R' and 'global.R' contains the information for running the Shiny app. Credentials to enter the server must be adjusted in 'global.R'.

 - The files 'test.csv' and 'test_error.csv' contain examples of proper and wrong data to be uploaded to SQL DB through the Shiny app. Uploading wrong data shows a message advertising that data is wrong and it must be checked. 

