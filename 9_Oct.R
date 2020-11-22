summary(iris)
apply(iris[,1:4], 2, sd)
#install.packages("FNN")
library(FNN)
index = 1 : 100 ( iris )
index = 1 : nrow ( iris )
print(index)
testindex = sample ( index, trunc ( length ( index ) / 3 ) )
testset = iris [ testindex , ]
trainset = iris [ - testindex , ]
prediction = knn( trainset [ , 1:4 ] , testset [ , 1:4 ] , trainset [ , 5 ] , 5 )
tab = table ( pred = prediction, true = testset [ , 5 ] )
#install.packages("caret")
library(caret)
#install.packages("e1071")
confusionMatrix (tab)

model = svm ( Species ~ . , data = trainset )
prediction = predict ( model, testset [ , 1 : 4 ] )
tab = table ( pred = prediction, true = testset [ , 5 ] )
confusionMatrix (tab)

model = randomForest ( Species ~ . , data = trainset )
prediction = predict ( model, testset [ , 1 : 4 ] )
tab = table ( pred = prediction, true = testset [ , 5 ] )
confusionMatrix (tab)

prediction = predict ( model, testset [ , 1 : 4 ] )
tab = table ( pred = prediction, true = test [ , 5 ] )
tab = table ( pred = prediction, true = testset [ , 5 ] )
confusionMatrix (tab)


