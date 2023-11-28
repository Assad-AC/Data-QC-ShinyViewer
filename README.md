# Data-QC-ShinyViewer
 
This is an RShiny application designed to support interactivity with quality control checks.

In the deps folder, the FUNCTION file should be overwritten with the code for the QC checks, following the format and use of the function specified therein. 

The MODULE file in the deps folder calls on the FUNCTION file to run the quality control checks. The result is a dataframe with nested dataframes, labels, and UI elements, which are used to create the UI elements and their specifications in the app.

To use the application, ensure the use of an appropriate FUNCTION file for the data which is being quality-checked. Place the data which should be tested in the Input folder, ensuring they are .xlsx files with the data being the first sheet of the file. Open the app.R file to in RStudio to run the app.