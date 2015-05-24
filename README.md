# Getting-and-Cleaning-Data-Course-Project
Getting and Cleaning Data Course Repo

## run_Analysis.r Read Me File
We assume Working Directory has been set to the root directory provided in the exercise.

We initialize environment variables relating to:
* Measurement files 
  * X_train
  * X_test
* User Id Lists
  * subject_train
  * subject_test
* Activity Id Lists
  * y_train
  * y_test

In their relative sub folders
* train
* test

We read the Human Readable Label files: 
* Feature_Name from features.txt
* Activity_list activity_labels.txt

Into table elements


#### We are using a similar set of code to read the measurement data for the test and train datasets.

#### For both "Test" and "Train" repeat :

* Itialize a set temp variables from the gloabal setting we did above.
* Read the measurment data into "DataSet" using the "Feature_Name" as table headers.
* Read the activity Ids into a table from the activity Id file.
* Replace the numeric values into textual using the "factor" function with Activity_list which we pre-loaded.
* we use cbind to merge the 2 tables mentioned above "Activity_Name" and "DataSet"

* Load the user ids as yet another table and then merge using cbind.
* Rename the user id column into a meaningful name.

##### the data is stored respectively in Test_DataSet and Train_DataSet.

#### We merge the 2 data sets using rbind.

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

We have completed Steps 1- 4 from the assignment 

Next we move onto creating a filtered data set containing only the data for mean and standard deviation measurement

------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

* We read the new Colun Name list from the data
* We create a new data set "Filtered_Set" where we filter using grep, only the columns containing "mean" and "std" in addition to the User_Id and Activity_Name columns
* Next we create a new clean data set to calculate the avergae measurement per user id per activity name
* We make sure this data set is clean and empty containing jsut the headers and an empty line.
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

* First we iterate through the user id's
  * During each repeat we have a nested iteration on the changing activity Id's /Names
    * while in iterate through the differnet columns
    * And calculate the actual averege per user per activity for each measurment type 
  * Once each row is calculated it is added into the averege data set. using rbind
* Once we finish going over all of the users and all of the activities we remove the first empty line 
* and then write to a file the results with out the row names  
 

 

