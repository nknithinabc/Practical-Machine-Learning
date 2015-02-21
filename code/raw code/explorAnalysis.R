

###### 1) Getting & Cleaning Data

#Training data source URL: 
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#Test data source URL
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
myData <- read.csv(url(trainUrl))


##Exploring around the data:

str(myData)
summary(myData)

table(myData$classe)
#   A    B    C    D    E 
#5580 3797 3422 3216 3607

#Percentage-wise
prop.table(table(myData$classe))*100
#       A        B        C        D        E 
# 28.43747 19.35073 17.43961 16.38977 18.38243 

## Starting to clean the data:

##Besides 'NA' values, there are also a lot of '#DIV/0!' and empty values:
#SO, Doing NAs for the variables that have '#DIV/0!' and empty values:
myData <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))

###Note: I took some Notes about variables and conclusions: see random_personal_notes.txt

#Trying to identify variables that have very no variability/less significance for prediction:
myDataNZV <- nearZeroVar(myData, saveMetrics=TRUE)


#[1] 11776   160
#[1] 7846  160

###Cleaning of Data
##Transformation 1) Cleaning NearZeroVariance Variables

#Creating another subset without the NZVs:
myNZVvars <- names(training) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
"kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
"max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
"var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
"stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
"kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
"max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
"kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
"skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
"amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
"skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
"max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
"amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
"avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
"stddev_yaw_forearm", "var_yaw_forearm")
trainingV2 <- training[!myNZVvars]
dim(trainingV2)
#[1] 11776   100

##Transformation 2) Killing first column of Dataset - ID
#Removing first ID variable so that it does not interfer with ML Algorithms:
trainingV2 <- trainingV2[c(-1)]
dim(trainingV2)
#[1] 11776    99 #as expected..

##Transformation 3) Cleaning Variables with too many NAs:

#Let's deal with NAs
any(is.na(trainingV2)) 
#[1] TRUE 
#OK, so.. how many?
#summary(trainingV2)
#Glup, our Dataset contains a whole lot of NAs... How many exactly?
colSums(is.na(trainingV2))

#For Variables that have more than a 60% threshold of NA's I'm going to leave them out:

trainingV3 <- trainingV2 #creating another subset to make sure no screwing up
#Loop "à la Python" style (sorry, kind of in a hurry.. IMPROVE WITH APPLY FUNCTION IF TIME)
for(i in 1:length(trainingV2)) { #for every column in the training dataset
	if( sum( is.na( trainingV2[, i] ) ) /nrow(trainingV2) >= .6 ) { #if nº NAs > 60% of total observations
		for(j in 1:length(trainingV3)) {
			if( length( grep(names(trainingV2[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
				trainingV3 <- trainingV3[ , -j] #Remove that column
			}	
		} 
	}
}
dim(trainingV3)
#[1] 11776    58 

any(is.na(trainingV3)) 
#[1] FALSE #Guess this meant the extermination of all those NAs...


#######Doing the same for our improvised Testing set, removing NZV:
##Removing NZV:
testingV2 <- testing[!myNZVvars]

#Removing First Column - ID
testingV2 <- testingV2[c(-1)]
#Cleaning NAs
testingV3 <- testingV2
for(i in 1:length(testingV2)) { #for every column in the "improved" testing dataset
	if( sum( is.na( testingV2[, i] ) ) /nrow(testingV2) >= .6 ) { #if nº NAs > 60% of total observations
		for(j in 1:length(testingV3)) {
			if( length( grep(names(testingV2[i]), names(testingV3)[j]) ) ==1)  { #if the columns are the same:
				testingV3 <- testingV3[ , -j] #Remove that column
			}	
		} 
	}
}
dim(testingV3)
#[1] 7846   58


#Making Validation/Test set provided uniform according to these transformations:
#Note that I will call The Testing Set "validation" (instead of Test as some people do)
validation <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
#Removing NZV
validationV2 <- validation[!myNZVvars]
#Removing ID column
validationV2 <- validationV2[c(-1)]
#Removing Extra column that does not match our first Dataset
validationV2 <- validationV2[c(-length(validationV2))] #Remove Problem_ID column
#Removing Variables with more than 60% NAs
validationV3 <- validationV2
for(i in 1:length(validationV2)) { #for every column in the real Testing dataset, that I called Validation set
	if( sum( is.na( validationV2[, i] ) ) /nrow(validationV2) >= .6 ) { #if nº NAs > 60% of total observations
		for(j in 1:length(validationV3)) {
			if( length( grep(names(validationV2[i]), names(validationV3)[j]) ) ==1)  { #if the columns are the same:
				validationV3 <- validationV3[ , -j] #Remove that column
			}	
		} 
	}
}
dim(validationV3)
#[1] 20 57

#Coercing Validation data set to have same Class type as Training data
	# This loop sets the same class type for equivelant Columns between two data sets
validationV4 <- validationV3 #just to make sure I don't mess up anything..
for (i in 1:length(validationV3) ) {
	for(j in 1:length(validationV4)) {
		if( length( grep(names(validationV3[i]), names(validationV4)[j]) ) ==1)  {
			class(validationV4[j]) <- class(validationV3[i])
		}      
	}      
}
for(i in 1:length(validationV4)) {
	if(class(validationV4[,i]) == "charactor") {
		as.Factor(validationV4[,i])
	}
}

#Not enough, so going to try something different to force Coertion:
validationV5 <- rbind(trainingV3[2, -58] , validationV4)
#Now remove the first row, so not to change the data:

validationV5 <- validationV5[-1,]


#Confirm that they are really Equal:
#identical(class(colnames(validationV3)), class(colnames(trainingV2)) )
#[1] TRUE
#identical(class(colnames(validationV3)), class(colnames(validationV2)) )


###### 2) Start testing ML algorithms
#Using caret package to start testing ML predictions
library(caret)
library(rpart)
set.seed(12345)
## Aproach A) Using Decision Trees:
#modFitA1 <- train(classe ~ ., method="rpart", data=trainingV3)
#print(modFitA1$finalModel)

#plot tree:
png(filename='./figures/PlotA1v1.png',
    width = 600, height = 600, units = "px",
     bg = "white")

plot(modFitA1$finalModel, uniform=TRUE, main="Classification Tree")
text(modFitA1$finalModel, use.n=TRUE, all=TRUE, cex=.8)

dev.off()

#plot decision tree more fancy:
library(rattle)
png(filename='./figures/PlotA1v2.png',
    width = 600, height = 600, units = "px",
     bg = "white")

fancyRpartPlot(modFitA1$finalModel)
dev.off()

#Validating the predictor Model:
predictions <- predict(modFitA1, newdata=testingV2)



#Could not control the type = class  in order to have same length, so trying different approach:
#Trying with rpart package:

modFitA1 <- rpart(classe ~ ., data=trainingV3, method="class")
print(modFitA1)

#plot tree:
png(filename='./figures/PlotA1v12.png',
    width = 600, height = 600, units = "px",
     bg = "white")

plot(modFitA1, uniform=TRUE, main="Classification Tree")
text(modFitA1, use.n=TRUE, all=TRUE, cex=.8)

dev.off()

#plot decision tree more fancy:
library(rattle)
png(filename='./figures/PlotA1v22.png',
    width = 600, height = 600, units = "px",
     bg = "white")

fancyRpartPlot(modFitA1)
dev.off()

predictionsA1 <- predict(modFitA1, testingV3, type = "class")

confusionMatrix(predictionsA1, testingV3$classe)

#Overall Statistics
                                          
#               Accuracy : 0.8683          
#                 95% CI : (0.8607, 0.8757)
#    No Information Rate : 0.2845          
#    P-Value [Acc > NIR] : < 2.2e-16       
                                          
#                  Kappa : 0.8335 


## Aproach B) Using Random Forests:
library(randomForest)
modFitB1 <- randomForest(classe ~. , data=trainingV3)

#fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
#                    FamilyID2, data=train, importance=TRUE, ntree=2000)

predictionsB1 <- predict(modFitB1, testingV3, type = "class")

confusionMatrix(predictionsB1, testingV3$classe)

#Overall Statistics
                                         
 #              Accuracy : 0.999          
 #                95% CI : (0.998, 0.9996)
 #   No Information Rate : 0.2845         
 #   P-Value [Acc > NIR] : < 2.2e-16      
                                         
 #                 Kappa : 0.9987         
 #Mcnemar's Test P-Value : NA 

#Random Forests yielded better Results, as expected!

#Finally, using the provided Test Set:
#For Decision Tree would be like this, but not going to use it:
predictionsA2 <- predict(modFitA1, validationV4, type = "class")

#For Random Forests is:
predictionsB2 <- predict(modFitB1, validationV5, type = "class")






