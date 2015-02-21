library(shiny)
library(ggplot2)

#dataset <- diamonds

#Training File URL
trainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#Test File URL
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainFilePath <- "./data/pml-training.csv"
testFilePath <- "./data/pml-testing.csv"

if(!file.exists("./data")) {
	dir.create("./data")
}
if(!file.exists("./data/pml-training.csv")) {
	download.file(fileUrl, destfile = trainFilePath, method="curl")
}
if(!file.exists("./data/pml-training.csv")) {
	download.file(fileUrl, destfile = testFilePath, method="curl")
}

trainData <- read.csv(trainFilePath); testData <- read.csv(testFilePath)

dataset <- trainData

shinyUI(pageWithSidebar(
	headerPanel("Final Project Shiny App"),
	
	sidebarPanel(
		
		h3('sidebarPanel text'),
		# numericInput('id1', 'Numeric input, labeled id1', 0, min=0, max=10, step=1),
		# submitButton('Submit'),

		# checkboxGroupInput("id2", "Checkbox",
		# 		c("Value 1" = "1",
		# 		  "Value 2" = "2",
		# 		  "Value 3" = "3"
		# 			)
		# 	),
		# dateInput("date", "Date:"),

		# numericInput('glucose', 'Glucose mg/dl', 90, min=50, max=200, step=5),
		# submitButton('Submit'),

		# sliderInput('mu', 'Guess at the mean', value=70, min = 62, max=74, step = 0.05),
		# submitButton('Submit'),

		#demo app for Shiney.io
		sliderInput('sampleSize', 'sampleSize', min=1, max=nrow(dataset),
				value=min(1000, nrow(dataset)), step=500, round=0),
		selectInput('x', 'X', names(dataset)),
		selectInput('y', 'Y', names(dataset)),
		selectInput('color', 'Color', c('None', names(dataset))),

		checkboxInput('jitter', 'Jitter'),
		checkboxInput('smooth', 'Smooth'),

    	selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
    	selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))

		),
	
	mainPanel(
		h3('Main Panel text'),
		plotOutput('plot')
		# h3('Illustrating Outputs'),
		# verbatimTextOutput("oid1"),
		# h4("You entered"),
		# verbatimTextOutput("oid2"),
		# h4("You entered"),
		# verbatimTextOutput("odate"),

		# code('some code'),
		# h4('some ordinary text'),

		# h3('Results of preduction'),
		# h4('You entered:'),
		# verbatimTextOutput("inputValue"),
		# h4('Which resulted in a prediction of '),
		# verbatimTextOutput("prediction"),

		# plotOutput('newHist')
	)
))