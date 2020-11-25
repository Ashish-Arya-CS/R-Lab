# Reading the data
card_data = read.csv('c:/Users/KIIT/Downloads/creditcard.csv')
summary(card_data)
library(corrplot)
# Removing the column of time as it has nothing to do with model training 
card_data <- card_data[, !(names(card_data) == "Time")]
card_data
# Data Analysis- Finding the correlation between data
corr <- cor(card_data, method = "pearson")
corrplot.mixed(corr, lower = "number", upper = "circle", tl.pos = "lt", diag = "u")
# We saw that the data is not correlated.So we will have to consider all columns for model training
# Finding if the data is balanced or not
table(card_data$Class)/nrow(card_data)
# We found that the data is not balanced.So balancing the data using ubSMOTE from "unbalanced" library
library(unbalanced) 
balanced <- ubSMOTE(X = card_data[,-30], Y = as.factor(card_data$Class),
                                         perc.over=200, perc.under=800,  verbose=TRUE ) 
# Viewing the balanced data 
balanceddf <- cbind(balanced$X, Class = balanced$Y)
table(balanceddf$Class)/nrow(balanceddf)
# Plotting the data with respect to Class for better intuition of the features
library(caret)
for (i in seq(from =1, to = 29, by = 2))
{
  show(
    featurePlot(
      x = balanceddf[, c(i,i+1)], 
      y = balanceddf$Class,plot = "density", 
      scales = list(x = list(relation="free"), 
                    y = list(relation="free")), 
      adjust = 1.5, pch = "|", layout = c(2,1 ), auto.key=TRUE
    )
  )
}
#As we can see from the plotting, columns v15,v22,v24,v25 and v26 have less impact on our target. 
#Hence, we will exclude these columns and divide the data into 70% and 30%.
newbalanceddf<-balanceddf[,-c(15,22,24,25,26)]
nrow(newbalanceddf)
ncol(newbalanceddf)
# Test-Train split
index = 1 : nrow(newbalanceddf)
testindex = sample(index, trunc(length(index)/3))
testset = newbalanceddf[testindex, ]
trainset = newbalanceddf[-testindex, ]
# Using KNN Algo- Achieved 97.56% accuracy
library(FNN)
View(trainset)
prediction = knn(trainset [ ,1:24], testset [ ,1:24], trainset [ ,25], 3)
tab = table ( pred = prediction, true = testset [ , 25 ] )
confusionMatrix (tab)
# Using RandomForest Algo- Achieved 98.36% accuracy
library(randomForest)
model = randomForest ( Class ~ . , data = trainset )
prediction = predict ( model, testset [ , 1 : 24 ] )
tab = table ( pred = prediction, true = testset [ , 25 ] )
confusionMatrix (tab)
# Using SVM Algo- Achieved 97.79% accuracy
library(e1071)
model = svm ( Class ~ . , data = trainset )
prediction = predict ( model, testset [ , 1 : 24 ] )
tab = table ( pred = prediction, true = testset [ , 25 ] )
confusionMatrix (tab)
# Hence RandomForest gave best results.
# Now lets see prediction on just one data
testdata = testset[1,]
pred = predict( model, testdata[ , 1 : 24])
testdata = testset[12,]
pred = predict( model, testdata[ , 1 : 24])