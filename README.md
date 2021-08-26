# Data Explorer
## About
Data Explorer was created for the <a href="https://merkleresearchgroup.org/">Merkle Research Group</a> in the Summer of 2021. It can quickly parse migration data (in the form of an ESRI Shapefile) and generate up to 6 unique graphs to aid in data analysis. 

## Installation
In order to use the Data Explorer App, you must have the most recent version of RStudio and R Language.

1) Download the code directly from this Github.
2) Open RStudio, and use the "Files" tab on the bottom right to navigate to where the downloaded code is.
3) Open and run the "Install Dependencies.R" and "WriteFileCsv.R" files (found in Data-Explorer/Data Explorer App/Scripts/Helper Scripts).
4) Still in RStudio, click File --> Open Project --> /Data-Explorer/Data Explorer App/Data-Explorer.Rproj
4) After opening the R Project through RStudio, open the App.R file if it is not already open.
5) Click "Run App". The app should now open.

## How to Use the App
The app has 4 main tabs, which all have subpanels:
- Data Upload: This is where you will upload your migration data and other GIS data.
- Data Define: This is where you will define various parameters for your data.
- Visualize Data: This is where you can quickly generate plots of your data.
- Clear Data: This is where you can clear your parameters and data from the App's local storage.

The App's structure was created in such a way, that you will follow each tab on a left-to-right basis, and in each tab, complete each panel on a top-to-bottom basis. More detailed instructions have been included below.


### Data Upload Section
#### ShapeFile Upload
- Select your migration data (in the form of an ESRI Shapefile) using the File Select button. The migration data must be in your computer's Home Directory (by default the Documents folder) in order to be found.
- After the migration data is loaded into the app, select the ID and Date columns for the data. These columns are necessary for later movement calculations.
- After pressing the "Upload Data" button, the App will proceeed to calculate movement parameters, format the data, and save this modified data in its local storage. You can find it in the Data/Migration Data Directory. 
- A success message should appear when the data has been uploaded.

#### TIF/IMG File Upload
- Select your TIF or TIF files. You may select more than one at a time.
- After selecting these files, you will be prompted define each files' datatype. Preexisting datatypes will also be displayed. Try to avoid unnecessary whitespace (datatype "cover" is not the same as datatype "cover ").
- After pressing the "Import File" button, the file information will be saved to a .csv file, which can be found in Data/Raster Layers.


### Data Define Section
#### Define Biological Year
The app will, by default, have a biological year defined. Make sure that the seasons and start month are appropriate. If you would like to change them, make the necessary changes, and click the "Save Changes" button at the bottom. 

Note: Seasons are not limited to Spring, Summer, Autumn and Winter.

#### Continuous vs Discrete
For all of the TIF/IMG files uploaded, select if the data is continuous or discrete, and press the "Load Data" button to save the information. By default, movement parameters (distance, speed, etc) are continuous and cannot be changed.

You must define if the datatype is continuous or discrete in order to visualize that datatype later.

#### Retrieve Data
The data uploaded earlier is saved in the app's storage, and not held in memory. As such, this section allows you to load the migration data into memory so that visualizations can be done on it.

Select the datatypes (uploaded through the IMG/TIF files) you'd like to load the data with. After pressing "Load Data", the app will load the data into memory, and add extract the appropriate values of each datatype for each migration data point.

<b>Note: The data must be loaded in this section in order to use visualizations, regardless of if you have TIF/IMG files or not.</b>

### Data Visualize Section
In this section, simply select the datatypes you would like to display. You may also select a groupBy to group the data by month, season or biological year.

If the "Split IDs" option is pressed, you may compare data for particular individuals side-by-side.
If the "Chronological" option is pressed, unique biological year - month/season pairs will be shown in chronological order.

### Data Clear Section
If you would like to quickly clear any options in the app, you can using this section. Simply press the appropriate button and wait for the app to finish.
