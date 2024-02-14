# NationalSoilDB - SQL data injection
Shiny app to inject soil data from cvs files to SQL.
 - The script 'create_tables.R' allows to create empty PostgreSQL tables according to the definitions for soil databases at GSP. There must be a proper DB created in the PostgreSQL server (e.g. CREATE DATABASE carsis;). Credentials to enter the server must be adjusted in the R script.

 - The scripts 'ui.R', 'server.R' and 'global.R' contains the information for running the Shiny app. Credentials to enter the server must be adjusted in 'global.R'.

 - The files 'test.csv' and 'test_error.csv' contain examples of proper and wrong data to be uploaded to SQL DB through the Shiny app. Uploading wrong data shows a message advertising that data is wrong and it must be checked. 


PostgreSQL install
- How to install PostgreSQL
	Mac:
	Download PostgreSQL app from https://postgresapp.com
	Download   ➜   Move to Applications folder   ➜   Double Click
	Click "Initialize" to create a new server 	Configure your $PATH to use the included command line tools (optional):
    * sudo mkdir -p /etc/paths.d &&
    * echo /Applications/Postgres.app/Contents/Versions/latest/bin | sudo tee /etc/paths.d/postgresapp

	Allow access to Terminal from the app

	Windows:
	Download PostgreSQL Installer for Windows at https://www.enterprisedb.com/downloads/postgres-postgresql-downloads
	Double-click on the installer file (administrator privileges)
	
	Linux:
	Add the PostgreSQL Apt repository to your Linux distro:
    * sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release-cs)-pgdg main" &gt; /etc/apt/sources.list.d/pgdg.list'
    * wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add - s
	Install PostgreSQL:	
    * sudo apt update
    * sudo apt install postgresql postgresql-contrib
    * sudo systemctl start postgresql.service
      
	To switch to the postgres account on your server type:
    sudo -i -u postgres
    
	To access the PostgreSQL prompt type:
     psql
    
	To exit the PostgreSQL prompt type:
    \q

- How to create a new database
	Double click on a database to open psql terminal (allow to Open a Terminal) and type:  CREATE DATABASE name_of_database;
	(e.g. CREATE DATABASE carsis;)
	(To see the existing databases use  \list)
	(To see existing tables in the database use \dt)

- How to create Soil Information tables:
	Open the script ‘create_tables.R’ and modify the database specifications, you must adjust information on:
			* database_name
			* hostname
			* port	
			* userid	
			* password
	Run the script  ‘create_tables.R’ 

- How to populate tables:
Use the shiny app injectSQL (ui, server and global files as shiny app)

- How to delete completely a database from the server:
    - Within psql, type: DROP DATABASE carsis with (FORCE);
