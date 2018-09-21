# Utkarsh Kulshrestha
# kuls.utkarsh1205@gmail.com
# Data Scientist

library(ggplot2)
library(tabplot)
library(caret)

#Load the data
data <- read.csv("/home/utkarsh/Downloads/RealEstate.csv", stringsAsFactors = FALSE)

#Exploratory Analysis
head(data)
dim(data)
summary(data)
str(data)
unique(data$Location)

#Replace the spaces within the Locations at the start
data$Location <- gsub("^ ", "", data$Location)
unique(data$Location)

# Exclude price per sq ft
data$Price.SQ.Ft <- NULL
# Remove MLS
data$MLS <- NULL

#Corelation check
nums <- sapply(data, is.numeric)
plot(data[,nums])
cor(data[,nums])

#Check for zero variance
nearZeroVar(data[, -2], saveMetrics = TRUE)
tableplot(data)

#Remove the location visits more than 3
freq <- as.data.frame(table(data$Location))
loc <- freq[freq$Freq > 3,]
loc

data <- data[data$Location %in% loc$Var1, ]
# Check how many rows we have now
nrow(data)

#Scatterplot a graph price vs size
plot(data$Size, data$Price, main="Price and Sq Ft", xlab="Sq Ft ", ylab="Price", pch=19)

#Partition the data into training and test dataset
set.seed(3456)
trainIndex <- createDataPartition(data$Price, p = .8,
                                  list = FALSE,
                                  times = 1)

train <- data[ trainIndex,]
test  <- data[-trainIndex,]

# Create 10 folds of cross validation.
fitControl <- trainControl(method = "cv")

#Simple Regression model with size and price only also known as linear regression
simplemodel <- train(Price~Size, train, method = "lm", trControl = fitControl)

#Predict the values for test data
pred <- predict(simplemodel, test)
#RMSE should be as low as possible but one should not do overfitting
RMSE(pred, test$Price)
#Plot the graph for predicted values
plot(test$Size, test$Price, main="Price and Sq Ft", xlab="Sq Ft ", ylab="Price", pch=19)
abline(simplemodel$finalModel$coefficients, col="red")

#Do some more exploration to improve the model or RMSE 
boxplot(data$Size)
boxplot(Price~Status, data = data)
boxplot(Price~Bedrooms, data = data)
boxplot(Price~Bathrooms, data = data)

#Regression model with more features also known as multiple regression
set.seed(3456)
allfeaturesmodel <- train(Price~., train, method = "lm", trControl = fitControl)
allfeaturesmodel
simplemodel

#Predict the values for test dataset
allFeaturesPred <- predict(allfeaturesmodel, test)
#check RMSE value
RMSE(allFeaturesPred, test$Price)

#Check important features or variables; caret will internally create dummy variables
varImp(allfeaturesmodel$finalModel)

#Create a PCA model 
set.seed(3456)
pcamodel <- train(Price~., train, method = "lm", trControl = fitControl, preProcess = "pca" )
pcaPred <- predict(pcamodel, test)

simplemodel
allfeaturesmodel
pcamodel

#RMSE values for every model
RMSE(pred, test$Price)
RMSE(allFeaturesPred, test$Price)
RMSE(pcaPred, test$Price)

#Parameter check for PCA
coefficients(pcamodel$finalModel)

#Let's have some fun now for your clearity
house1 = data[which(data$Size==2371),]
house1
predict(allfeaturesmodel,house1)
