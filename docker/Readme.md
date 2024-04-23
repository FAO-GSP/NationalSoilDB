README: Dockerized PostgreSQL and Shiny App for Soil Data Management
This Dockerfile creates a PostgreSQL database and a Shiny web application designed for inserting soil data according to the ISO 28258 data model. Docker Desktop is required for installation.

Instructions:
* Install Docker Desktop:
    * Download and install Docker Desktop from Docker's official website.
* Download the Repository:
    * Download or clone the repository containing the Docker configuration.
* Build and Run the Docker Containers:
    * Open a terminal or command prompt.
    * Navigate to the downloaded repository folder:
      cd /path/to/repository/docker  
    * Run the following command to build and start the Docker containers:
      docker-compose up --build

      
Installation Details:
* The installation will create two new folders in your docker folder:
    * data/: This folder serves as a mirror volume on your disk where the PostgreSQL databases will be stored.
    * init-scripts/: Contains scripts that are executed when the postgres-data folder is created.

Using the Application:
* Once Docker containers are up and running, access the application at: http://localhost:3838.
* Main Panel Overview:
    * The main panel displays tables required by the ISO-28158 data model.
    * Click on 'Find Database' button to connect to a PostgreSQL database. Provide a database name.
        * If the database exists, a message in blue will indicate its availability.
        * If the database does not exist, the application will create a new database with the indicated name following the ISO-28158 data model structure. A message in orange will indicate the creation process and availability.
    * Once the database is available/created, click 'Connect to Database' to access its contents.
    * Use the 'Browse' button to add new data to the database. Input data must be in xlsx format and comply with specified standards.
* Test Data:
    * Use the provided simulated data in the 'test_data' folder for projects such as "CARSIS" and "TEST" to test the application.
* Visualization Dashboard:
    * Click 'Render Dashboard' to create a visualization dashboard displaying location and properties of your data as an HTML file.
    * The resulting file will be stored in the 'r-scripts' folder and can be opened with any web browser.


Note: Ensure Docker Desktop is running before starting the application. For any issues or inquiries, please refer to the documentation or contact the repository maintainers.
