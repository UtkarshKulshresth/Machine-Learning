# Utkarsh Kulshrestha
# kuls.utkarsh1205@gmail.com
# Data Scientist

library(RTextTools)
library(e1071)

pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)

# build dtm
matrix= create_matrix(tweets[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 
# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(tweets[1:10,2]) )

# test the validity
predicted = predict(classifier, mat[11:15,]); predicted
table(tweets[11:15, 2], predicted)
recall_accuracy(tweets[11:15, 2], predicted)

# Let's try SVM on sentimental analysis
## Linear
svmClassifier = svm (x=mat[1:10,], y=as.factor(tweets[1:10,2]), type='C', kernel='linear')
pred1 = predict (svmClassifier, mat[11:15,])
table(tweets[11:15, 2], pred1)
#recall_accuracy(tweets[11:15, 2], pred1)

## Non linear
svmClassifier = svm (x=mat[1:10,], y=as.factor(tweets[1:10,2]), type='C', kernel='polynomial', degree=2)
pred2 = predict (svmClassifier, mat[11:15,])
table(tweets[11:15, 2], pred2)

## Non linear
svmClassifier = svm (x=mat[1:10,], y=as.factor(tweets[1:10,2]), type='C', kernel='radial', gamma=0.1)
pred3 = predict (svmClassifier, mat[11:15,])
table(tweets[11:15, 2], pred3)

## Non Liner
svmClassifier = svm (x=mat[1:10,], y=as.factor(tweets[1:10,2]), type='C', kernel='radial', gamma=0.1, cost=10)
pred4 = predict (svmClassifier, mat[11:15,])
table(tweets[11:15, 2], pred4)

recall_accuracy(tweets[11:15, 2], pred1)*100
recall_accuracy(tweets[11:15, 2], pred2)*100
recall_accuracy(tweets[11:15, 2], pred3)*100
recall_accuracy(tweets[11:15, 2], pred4)*100

# But how to find best one

# A nice tool in package e1071 is the possibility of tuning the parameters by 10-CV grid search:

mytunedsvm <- tune.svm(x = mat[1:10,], y=as.factor(tweets[1:10,2]), gamma = 2^(-1:1), cost = 2^(2:4)) 
summary(mytunedsvm)
#plot (mytunedsvm, transform.x=log10, xlab=expression(log[10](gamma)), ylab="C")

svmClassifier = svm (x=mat[1:10,], y=as.factor(tweets[1:10,2]), type='C', kernel='radial', gamma=0.5, cost=4)
pred4 = predict (svmClassifier, mat[11:15,])
table(tweets[11:15, 2], pred4)
