library(caret)

#Data importing and loading
dataurl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
download.file(url = dataurl, destfile = "wine.data")
wine_df <- read.csv("wine.data", header = FALSE)

#Structure of wine dataset
str(wine_df)

#Divide the dataset into training and testing
set.seed(3033)
intrain <- createDataPartition(y = wine_df$V1, p= 0.7, list = FALSE)
training <- wine_df[intrain,]
testing <- wine_df[-intrain,]

#Exploratory Analysis
dim(training); dim(testing);
anyNA(wine_df)
summary(wine_df)

#Convert the numerical variables into categorical variables 
training[["V1"]] = factor(training[["V1"]])

#Tarin the KNN model on our training data 
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(V1 ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

#Put your model on to the test dataset
test_pred <- predict(knn_fit, newdata = testing)
test_pred

#Plot the confusion matrix and check the classes
confusionMatrix(test_pred, testing$V1 )
