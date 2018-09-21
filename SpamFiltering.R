#Name: Utkarsh Kulshrestha
#Email: kuls.utkarh1205@gmail.com
#Designation: Data Scientist
#install.packages('tm')
#install.packages('wordcloud')
#install.packages('e1071')
#install.packages('gmodels')
#install.packages('SnowballC')

library(tm)
library(wordcloud)
library(e1071)
library(gmodels)
library(SnowballC)

#Read the data from the csv
spam <- read.csv('C:sms_spam.csv')
#Converting type as a categorical variable
spam$type <- factor(spam$type)
table(spam$type)

#visualise the frequent words in each category using a wordcloud. 
#Words such as 'free', 'call', 'now', appear often in the spam category.
spam_messages <- subset(spam,type=="spam")
ham_messages <- subset(spam, type=="ham")
#Wordcloud for Spam messages
wordcloud(spam_messages$text, max.words = 100, scale = c(3,0.5))
#Wordcloud for Ham messages
wordcloud(ham_messages$text, max.words = 100, scale = c(3,0.5))

#Data preparation for statistical modelling
corpus <- VCorpus(VectorSource(spam$text)) 
dtm <- DocumentTermMatrix(corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

#Data Partitioning and Further Cleaning
trainLabels <-spam[1:4169,]$type
testLabels <- spam[4170:5559,]$type
#check for the 13% spam in training dataset
prop.table(table(trainLabels))
#check for the 13% spam in testing dataset
prop.table(table(testLabels))

#Document-Term Matrix
dtmTrain <- dtm[1:4169,]
dtmTest <- dtm[4170:5559,]

#Low frequency words are removed as they are unlikely to be useful in the model. 
#Only words that occur more than 5 times are used.
freqWords <- findFreqTerms(dtmTrain,5)
#Frequent words in training dataset
freqTrain <- dtmTrain[,freqWords]
#Frequent words in testing dataset
freqTest <- dtmTest[,freqWords]

#The DTM matrix uses 1's or 0's depending on whether the word occurs in the sentence or not. 
#Naive Bayes classifier works with categorical features. 
#1 and 0 is therefore converted to Yes or No. 
#This is applied to every column (i.e. margin=2).
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
train <- apply(freqTrain, MARGIN = 2,
               convert_counts)
test <- apply(freqTest, MARGIN = 2,
              convert_counts)

#Train the Naive bayes classifier on the data
#As an example, the output for the word 'call' is displayed. 
#The probability that a message with the word 'call' is spam is higher than compared to ham.
classifier <- naiveBayes(train, trainLabels)
classifier[2]$tables$call

#Confusion matrix for the performance measurements
testPredict <- predict(classifier, test)
CrossTable(testPredict, testLabels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
