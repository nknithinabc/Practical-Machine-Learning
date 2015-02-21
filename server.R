library(shiny)

#library(UsingR)
#data(galton)
library(ggplot2)

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

shinyServer(
	function(input, output) {
		
		# output$oid1 <- renderPrint({ input$id1 })
		# output$oid2 <- renderPrint({ input$id2 })
		# output$odate <- renderPrint({ input$date })

		# output$inputValue <- renderPrint({ input$glucose })
		# output$prediction <- renderPrint({ diabetesRist(input$glucose) })

		# output$newHist <- renderPlot({
		# 	hist(galton$child, xlab='child height', col="lightblue", main="Histogram")
		# 	mu <- input$mu
		# 	lines(c(mu, mu), c(0,200), col="red", lwd=5)
		# 	mse <- mean((galton$child - mu)^2)
		# 	text(63, 150, paste("mu = ", mu))
		# 	text(63, 140, paste("MSE = ", round(mse,2)))
		# 	})


		dataset <- reactive(function() {
			trainData[sample(nrow(trainData), input$sampleSize), ]
			})

		output$plot <- reactivePlot(function() {

			p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
			
			if (input$color != 'None') 
				p <- p + aes_string(color=input$color)

			facets <- paste(input$facet_row, '~', input$facet_col)
			if (facets != '. ~ .')
				p <- p + facet_grid(facets)

			if (input$jitter)
				p <- p + geom_jitter()
			if (input$smooth)
				p <- p + geom_smooth()
			print(p)

			}, height=700)
	}
)