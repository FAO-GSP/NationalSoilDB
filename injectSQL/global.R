# PostgreSQL credentials
driver_name <- "PostgreSQL"
database_name <- "carsis"
host_name <- "localhost"
port_number <- "5432"
user_name <- "luislado"
password_name <- ""

# Expected data types for the uploaded file
expected_vars <- list(
  project_id = "integer",
  project_name = "character",
  site_id = "integer",
  site_code = "character",
  longitude = "numeric",
  latitude = "numeric",
  plot_id = "integer",
  plot_code = "character",
  plot_type = "character",
  profile_id = "integer",
  profile_code = "character",
  position = "character"
)
