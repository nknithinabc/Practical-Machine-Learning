README for final project of Coursera MOOC Practical Machine Learning
====================================================================


Background
--------------------------

Author: Nithin Krishna

These files were produced for the final Project assignment of Coursera's MOOC Practical Machine Learning from Johns Hopkins University. Here is the introduction of the exercise:

For more information about the several MOOCs comprised in this Specialization, please visit:
https://www.coursera.org/specialization/jhudatascience/




Repository Structure
--------------------------
 	/Code
 		/final code  #Here you can find the final code used to produce this assignment
 			project.R #final script to conduct ML analysis and prediction
 		/raw code #Here you can find all code used to explore and mess around this 	assignment
 			explorAnalysis.R #All raw code used to conduct exercise
 			random_personal_notes.txt #Some notes taken while doing the exercise. Not necessairly formated for other people to understand
 			server.R #Initial Exploratory Analysis using Shiny Server (runApp())
 			ui.R 	##Initial Exploratory Analysis using Shiny Server (runApp())
 	/figures	#some plots drawn during exploratory analysis
 		PlotA1v1.png #First Decision Tree plot, with caret package method="rpart"
 		PlotA1v12.png #same decision tree plot with caret package, but drawn with Fancy package
 		PlotA1v2.png 	#decision tree plot with rpart package
 		PlotA1v22.png 	#decision tree plot with rpart package, but drawn with Fancy package



Project given Description
--------------------------

> Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

> The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).

2. You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details. 
  


Data Sources
--------------------------

The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 



Author notes about the Project
-----------------------------------------------

Please see the Code Book file 'project_report.Rmd' for a more complete project summary. It describes the variables, the data, any transformations or work that I performed to clean up the data, and Machine Learning algorithms used for the prediction.




